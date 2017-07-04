;+
; :Author: LEE
; the program be used to batch caculate NDVI
;-
function ndvi_caculate,B3,B4
  NDVI=(float(B4)-float(B3))/(float(B3)+float(B4));nir-red/nir+red
  return,NDVI
END

pro batch_ndvi
  ;load the envi model------
  compile_opt idl2
  ENVI,/restore_base_save_files
  ENVI_BATCH_INIT
  ;---------------------------------
  path_dir='H:\NDVI_SAVE\';OUT_PATH,please creat the folder first
  ;---------------------------------
  ;open the tif to be batch caculated-------
  ;open workpath

  work_dir=DIALOG_PICKFILE(title='选择文件所在路径',/directory)
  CD,work_dir
  ;get the image to be caculated
  img_path=FILE_SEARCH(work_dir,'*.HDR',count=fnum)
  img_file=FILE_BASENAME(img_path)

  ;number control
  IF(fnum LE 0) THEN BEGIN
    print,'There are no valid MTL.TXT files to be processed, end the program!'
    ENVI_BATCH_EXIT;退出初始化批处理格式
  endif

  ;open file
  for i=0,fnum-1 do begin;循环计算NDVI
    ;print,'cyc1'
    e=envi()
    raster=e.OpenRaster(img_file[i])
  t0=systime(1)
    NUM=N_ELEMENTS(raster)

    ;select multi bands raster and get fid
    FOR i1=0, NUM -1 DO BEGIN
      ;      print,'cyc2'
      fid=ENVIRasterToFID(raster[i1])
      ENVI_FILE_QUERY,fid,ns=ns,nl=nl,nb=nb,dims=dims
      IF nb EQ 6 THEN fid = fid[i1]
      BREAK
    ENDFOR
    bas_img_file=strtrim(strmid(img_file[i],0,10),2)
    number=i+1
    ENVI_FILE_QUERY,fid,ns=ns,nl=nl,nb=nb,dims=dims
    MAP_INFO=ENVI_GET_MAP_INFO(fid=fid)
    print,'caculate number:'+string(number)
    B3=envi_get_data(fid=fid,dims=dims,pos=2)
    B4=envi_get_data(fid=fid,dims=dims,pos=3)
    ;caculate the ndvi b3-b4/b3+b4------
    NDVI=ndvi_caculate(B3,B4)
    index=where(NDVI LT -1)
    NDVI[INDEX]=-1
    index2=where(NDVI GT 1)
    NDVI[index2]=1
    print,'max is:'+string(max(NDVI)),'min is:'+string(min(NDVI))
    NDVI_OUT=path_dir+bas_img_file+'_NDVI'

    ;write out as tif image------
    ENVI_WRITE_ENVI_FILE, NDVI, out_name=NDVI_OUT,$
      ns=ns,nl=nl, offset=offset, map_info=map_info
    print,'写出文件完成！'+string(number)
    print, 'time used:', floor((systime(1)-t0)/3600), 'h',floor(((systime(1)-t0) mod 3600)/60),'m',(systime(1)-t0) mod 60,'s'
  endfor
  print,'end of the program!'
end