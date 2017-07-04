pro extract_neededdata

  compile_opt idl2
  ENVI,/restore_base_save_files
  ENVI_BATCH_INIT
  ;open workpath
  work_dir=DIALOG_PICKFILE(title='请选择数据所在文件夹,/directory)
  CD,work_dir

  TXTfiles=FILE_SEARCH(work_dir,'*.tif',count=fnum);获取tif文件
  head_basena=FILE_BASENAME(TXTfiles);tif文件名

  IF(fnum LE 0) THEN BEGIN
    print,'There are no data to be processed, end the program!'
    ENVI_BATCH_EXIT;数据错误判断
  endif

  a = fltarr(fnum)
  a = !values.f_nan
  ; data processing
  e=envi()
  raster=e.OpenRaster(image_files)
  fid = ENVIRasterToFID(raster)
  ENVI_FILE_QUERY,fid,ns=ns,nl=nl,nb=nb,dims=dims
  MAP_INFO=ENVI_GET_MAP_INFO(fid=fid)

  for i=0,fnum-1 do begin;循环文件
    image_files = TXTfiles[i]
    raster=e.OpenRaster(image_files)
    fid = ENVIRasterToFID(raster)
    ENVI_FILE_QUERY,fid,ns=ns,nl=nl,nb=nb,dims=dims
     MAP_INFO=ENVI_GET_MAP_INFO(fid=fids)
     
  endfor

end