pro readf_test
  temp=''
  fmask_file = strarr(71)
  tm_file =strarr(71)
  modis_file =strarr(71)
  
  openr,lun,'G:\in.txt',/get_lun
  nl = file_lines('G:\in.txt')
  a = strarr(nl);save the last name of data
  i=0
  
  while ~eof(lun) do begin
    readf,lun,temp
    a[i] = temp
    i=i+1
  endwhile
  free_Lun,lun

  work_dir=DIALOG_PICKFILE(title='选择包含所有参与计算数据temp文件路径',/directory)
  CD,work_dir
  
  files_road = FILE_SEARCH(work_dir,'*.hdr',count=fnum)
  file_base = FILE_BASENAME(files_road,'.hdr')
  
  for i = 2000 , 2014 do begin
    fmask_file[i] = 1
    tm_file[i] = 1
    modis_file[i] = 1  
  endfor

  
end