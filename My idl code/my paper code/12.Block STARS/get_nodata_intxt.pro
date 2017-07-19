pro get_nodata_intxt
  compile_opt idl2
  ENVI,/restore_base_save_files
  ENVI_BATCH_INIT
  ;open workpath
  work_dir=DIALOG_PICKFILE(title='选择文件所在路径',/directory)
  CD,work_dir

  files_road=FILE_SEARCH(work_dir,'*.hdr',count=fnum)
  basena=FILE_BASENAME(files_road,'.hdr')

  ;if there is no gap_fill data
  IF(fnum LE 0) THEN BEGIN
    print,'There are no valid files to be processed, end the program!'
    ENVI_BATCH_EXIT;退出初始化批处理格式
  endif

  e = envi()
  fn_in = 'B:\NDVI_STACK_TEMP\in.txt'
  str=strarr(104)
  j=0

  for i=0, fnum-1 do begin
    print,'number: ' + basena[i]
    raster =  e.openraster(files_road[i])
    fid = ENVIRasterToFID(raster)
    ENVI_FILE_QUERY,fid,ns=ns,nl=nl,nb=nb,dims=dims ,INTERLEAVE = interleave1,$
      DATA_TYPE = data_type
    MAP_INFO=ENVI_GET_MAP_INFO(fid=fid)
    Dt = Envi_Get_Data(Fid = Fid,dims = dims,pos=0);
    if total(dt,/nan) eq 0 then begin
      envi_file_mng, id=fid, /remove
      print,'contine!'
      continue
    endif else begin
      numb = strmid(basena[i], 2, 4, /REVERSE_OFFSET)
      print,'write in'
      str[j]=numb
      j=j+1
      envi_file_mng, id=fid, /remove
    endelse

  endfor
  openw,lun,fn_in,/get_lun
  printf,lun,str
  free_lun,lun
  print,'all done! close the in.txt file!'

end