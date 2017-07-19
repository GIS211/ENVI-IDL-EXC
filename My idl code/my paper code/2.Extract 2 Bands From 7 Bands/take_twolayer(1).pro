;+
; :Author: LEE
;-
;select two band export

pro take_twolayer
  compile_opt idl2
  ENVI,/restore_base_save_files
  ENVI_BATCH_INIT

  work_dir=DIALOG_PICKFILE(title='选择文件所在路径',/directory)
  CD,work_dir


  files_road=FILE_SEARCH(work_dir,'*.tif',count=fnum)
  basena=FILE_BASENAME(files_road)

  ;if there is no gap_fill data
  IF(fnum LE 0) THEN BEGIN
    print,'There are no valid files to be processed, end the program!'
    ENVI_BATCH_EXIT;退出初始化批处理格式
  endif

  e = envi()
  for i=0, fnum-1 do begin
    p=2000+i
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
        0: t=0
        1: t=1
      endcase
      L[*,*,j] = Envi_Get_Data(Fid = fid,dims = dims,pos=t)
      ;    qt=where(L EQ min(L))
      ;    L[qt]=nodata
      L = L*float(L ne MIN(L, /nan))/(L ne MIN(L, /nan));把背景值设置成-nan
    endfor
    outputname = 'I:\2layermodis2\modis' +strtrim(string(p+1),2)
    ENVI_SETUP_HEAD,r_fid=fid, fname=outputname, INTERLEAVE = interleave1,$
      ns=ns,nl=nl,nb=2 , DATA_IGNORE_VALUE=min(L), data_type = data_type ,MAP_INFO=MAP_INFO
    ENVI_WRITE_ENVI_FILE, L, out_name=outputname, /nocopy, $
      ns=ns,nl=nl,nb=2, map_info=map_info

    ;   write_image, outputname[i], 'tiff', L[*,*,*]

    ;
    ;  e.ExportRaster, L, outputname[i], 'TIFF'
  endfor

  print,'all done!'
  ENVI_BATCH_EXIT
end