
PRO RasterSubsetViaShapefile, Fid, shpFile=shpFile, pos=pos, $
  inside=inside, outFile=outFile, r_fid=r_fid

  COMPILE_OPT idl2
  ENVI, /RESTORE_BASE_SAVE_FILES
  ENVI_BATCH_INIT

  CATCH, err
  IF (err NE 0) THEN BEGIN
    CATCH, /CANCEL
    PRINT, 'ERROR: ' + !ERROR_STATE.MSG
    MESSAGE, /RESET
    RETURN
  ENDIF

  ENVI_FILE_QUERY, fid, ns=ns, nl=nl, nb=nb, $
    dims=dims, fname=fname, bnames=bnames

  IF ~N_ELEMENTS(pos)      THEN pos     = LINDGEN(nb)
  IF ~N_ELEMENTS(inside)   THEN inside  = 1
  IF ~KEYWORD_SET(outFile) THEN outFile = envi_get_tmp()

  ;��ȡshp�ļ�����Ϣ
  oshp=OBJ_NEW('IDLffShape',shpFile)
  IF ~OBJ_VALID(oshp) THEN RETURN
  oshp->GETPROPERTY,n_entities=n_ent,$ ;��¼����
    Attribute_info=attr_info,$ ;������Ϣ���ṹ�壬 nameΪ������
    ATTRIBUTE_NAMES = attr_names, $
    n_attributes=n_attr,$ ;���Ը���
    Entity_type=ent_type  ;��¼����

  iProj = ENVI_PROJ_CREATE(/geographic)
  ;�Զ���ȡprj�ļ���ȡͶӰ���ϵ
  potPos = STRPOS(shpFile,'.',/reverse_search)  ;
  prjfile = STRMID(shpFile,0,potPos[0])+'.prj'

  IF FILE_TEST(prjfile) THEN BEGIN
    OPENR, lun, prjFile, /GET_LUN
    strprj = ''
    READF, lun, strprj
    FREE_LUN, lun

    CASE STRMID(strprj, 0,6) OF
      'GEOGCS': BEGIN
        iProj = ENVI_PROJ_CREATE(PE_COORD_SYS_STR=strprj, $
          type = 1)
      END
      'PROJCS': BEGIN
        iProj = ENVI_PROJ_CREATE(PE_COORD_SYS_STR=strprj, $
          type = 42)
      END
    ENDCASE
  ENDIF

  oProj = ENVI_GET_PROJECTION(fid = fid)

  ;Ȼ��ʹ��ROI������Ĥͳ��
  roi_ids = !NULL
  FOR i = 0, n_ent-1 DO BEGIN
    ;
    ent = oshp->GETENTITY(i, /ATTRIBUTES) ;��i����¼

    ;���ent���Ƕ���Σ������
    IF ent.SHAPE_TYPE NE 5 THEN CONTINUE

    N_VERTICES=ent.N_VERTICES ;�������

    parts=*(ent.PARTS)

    verts=*(ent.VERTICES)
    ; ���������ת��Ϊ�����ļ��ĵ������
    ENVI_CONVERT_PROJECTION_COORDINATES,  $
      verts[0,*], verts[1,*], iProj,    $
      oXmap, oYmap, oProj
    ; ת��Ϊ�ļ����
    ENVI_CONVERT_FILE_COORDINATES,fid,    $
      xFile,yFile,oXmap,oYmap

    IF MIN(xFile) GE ns OR $
      MIN(yFile) GE nl OR $
      MAX(xFile) LE 0 OR $
      MAX(yFile) LE 0 THEN CONTINUE

    ;��¼XY����䣬�ü���
    IF i EQ 0 THEN BEGIN
      xmin = ROUND(MIN(xFile,max = xMax))
      yMin = ROUND(MIN(yFile,max = yMax))
    ENDIF ELSE BEGIN
      xmin = xMin < ROUND(MIN(xFile))
      xMax = xMax > ROUND(MAX(xFile))
      yMin = yMin < ROUND(MIN(yFile))
      yMax = yMax > ROUND(MAX(yFile))
    ENDELSE

    ;����ROI
    N_Parts = N_ELEMENTS(Parts)

    FOR j=0, N_Parts-1 DO BEGIN
      roi_id = ENVI_CREATE_ROI(color=i,     $
        ns = ns ,  nl = nl)
      IF j EQ N_Parts-1 THEN BEGIN
        tmpFileX = xFile[Parts[j]:*]
        tmpFileY = yFile[Parts[j]:*]
      ENDIF ELSE BEGIN
        tmpFileX = xFile[Parts[j]:Parts[j+1]-1]
        tmpFileY = yFile[Parts[j]:Parts[j+1]-1]
      ENDELSE

      ENVI_DEFINE_ROI, roi_id, /polygon,    $
        xpts=REFORM(tmpFileX), ypts=REFORM(tmpFileY)

      ;����е�ROI��Ԫ��Ϊ0���򲻱���
      ENVI_GET_ROI_INFORMATION, roi_id, NPTS=npts
      IF npts EQ 0 THEN CONTINUE

      roi_ids = [roi_ids, roi_id]
    ENDFOR
  ENDFOR

  ;������Ĥ���ü�����
  ENVI_MASK_DOIT,         $
    AND_OR = 2,           $
    IN_MEMORY=0,          $
    ROI_IDS= roi_ids,     $ ;ROI��ID
    ns = ns, nl = nl,     $
    inside=inside,        $ ;�����ڻ���
    r_fid = m_fid,        $
    out_name = envi_get_tmp()

  CASE inside OF
    0: out_dims=dims
    1: BEGIN
      ;������Ĥ ---��������ü�
      ;out_dims����round�����򱨴�
      xMin = xMin >0
      xMax = ROUND(xMax) < (ns-1)
      yMin = yMin >0
      yMax = ROUND(yMax) < (nl-1)
      out_dims = [-1,xMin,xMax,yMin,yMax]
    END
  ENDCASE

  ENVI_MASK_APPLY_DOIT, FID = fid,      $
    POS = POS,                          $
    DIMS = out_dims,                    $
    M_FID = m_fid, M_POS = [0],         $
    VALUE = !values.F_NAN, r_fid = r_fid,           $;value is background value
    OUT_NAME = outFile

  ; ��Ĥ�ļ�ID�Ƴ�
  ENVI_FILE_MNG, ID = m_fid,/REMOVE,/DELETE

  OBJ_DESTROY,oshp
END

pro sub_raster_by_shp_bat
  compile_opt idl2
  ENVI,/restore_base_save_files
  ENVI_BATCH_INIT

  work_dir=DIALOG_PICKFILE(title='选择文件所在路径',/directory)
  CD,work_dir

  files_road=FILE_SEARCH(work_dir,'*.hdr',count=fnum)
  basena=FILE_BASENAME(files_road, '.hdr')

  shpfile=DIALOG_PICKFILE(title='Select shp ',FILTER = '*.shp')
  ;  a=intarr(fnum)
  ;if there is no gap_fill data
  IF(fnum LE 0) THEN BEGIN
    print,'There are no valid files to be processed, end the program!'
    ENVI_BATCH_EXIT;退出初始化批处理格式
  endif

  e = envi()
  for i=0, fnum-1 do begin
    ;    data_year=strtrim(fix(strmid(basena[i],9,4)),2);当前操作数据年份（整型）
    ;    print,'number: ' + data_year
    print,'number: ' + strmid(string(i+1),2)
    raster =  e.openraster(files_road[i])
    Fid = ENVIRasterToFID(raster)
    ENVI_FILE_QUERY,fid,ns = ns,nl = nl,nb = nb,dims = dims ,INTERLEAVE = interleave1,$
      DATA_TYPE = data_type
    MAP_INFO=ENVI_GET_MAP_INFO(fid=fid)

    ;    a[i] = data_year
    ;    if i gt 1 then begin
    ;      if a[i] eq a[i-1] then begin
    ;        ;        data_year = data_year+'2'
    ;        data_year = data_year+i
    ;      endif
    ;    endif
    ;outFile='B:\result\' + strmid(basena[i],3,6)+data_year + 'Fmask'
    outFile='B:\NDVI\adjust\' + basena[i] +'.dat'
    r_fid=0
    RasterSubsetViaShapefile, Fid, shpFile = shpFile, pos = pos, inside = 1, outFile = outFile, r_fid = r_fid
    ENVI_FILE_MNG, ID = fid, /REMOVE
    ENVI_FILE_MNG, ID = r_fid, /REMOVE
    if r_fid eq -1 then begin
      print,'failed to subset!'
      return
    endif
  endfor
  print,'all done!'
end
