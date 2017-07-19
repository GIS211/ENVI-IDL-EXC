
;+
; :Author: LEE
;-
;9.5 updata
;select two band export

pro take_twolayer_landsat
  compile_opt idl2
  ENVI,/restore_base_save_files
  ENVI_BATCH_INIT

  work_dir=DIALOG_PICKFILE(title='选择文件所在路径',/directory)
  CD,work_dir


  files_road=FILE_SEARCH(work_dir,'*.hdr',count=fnum)
  basena=FILE_BASENAME(files_road, '.hdr')

  ;if there is no gap_fill data
  IF(fnum LE 0) THEN BEGIN
    print,'There are no valid files to be processed, end the program!'
    ENVI_BATCH_EXIT;退出初始化批处理格式
  endif

  e = envi()
  for i=0, fnum-1 do begin

    print,'number: ' + string(i+1)
    raster =  e.openraster(files_road[i])
    fid = ENVIRasterToFID(raster)
    ENVI_FILE_QUERY,fid,ns=ns,nl=nl,nb=nb,dims=dims ,INTERLEAVE = interleave1,$
      DATA_TYPE = data_type
    MAP_INFO=ENVI_GET_MAP_INFO(fid=fid)
    L = fltarr(ns,nl,2)

    for j=0,1 do begin

      t=0
      case j of
        0: t=2
        1: t=3
      endcase
      L[*,*,j] = Envi_Get_Data(Fid = fid,dims = dims,pos=t)
      ;    qt=where(L EQ min(L))
      ;    L[qt]=nodata
      envi_file_mng, id=fid, /remove
    endfor

    outputname = 'B:\过程文件\2layerlandsat\'+basena[i]

    ENVI_WRITE_ENVI_FILE, L, out_name=outputname, /nocopy, $
      ns=ns,nl=nl,nb=2, map_info=map_info
      
    envi_file_mng,id=fid,/remove
    ;   write_image, outputname[i], 'tiff', L[*,*,*]
    ;   ENVI_SETUP_HEAD,r_fid=fid, fname=outputname[i], INTERLEAVE = interleave1,$
    ;     ns=ns,nl=nl,nb=2 , data_type = data_type ,MAP_INFO=MAP_INFO
    ;
    ;  e.ExportRaster, L, outputname[i], 'TIFF'
  endfor

  print,'all done!'
  ENVI_BATCH_EXIT
end