;打开二进制文件，并赋予头文件信息，保存为envi标准格式

pro Change_Filename
  ;读取数据

  e = ENVI()

  file = DIALOG_PICKFILE(TITLE='选择带头文件的.dat文件',  FILTER='*.dat')

  infiles = DIALOG_PICKFILE(TITLE='选择存放没有二进制文件的文件夹', /DIRECTORY)
  ;输出路径
  out_dir = 'D:\result\'
  cd,infiles

  filename = file_search(count = n)

  raster = e.OpenRaster(file)
  fid1 = ENVIRasterToFID(raster)

  envi_file_query,fid1,data_type=data_type, xstart=xstart, $
    ystart=ystart, interleave=interleave, nb=nb, nl=nl, ns=ns,$
    offset=offset,dims=dims,file_type=file_type
  map_info=envi_get_map_info(fid=fid1)

  data = intarr(ns, nl, nb)

  for i = 0, n-1 do begin
    filenames = filename[i]
    openu, lun, filenames, /get_lun
    readu, lun, data
    
    out_name = out_dir + filenames + '.dat'
    ENVI_WRITE_ENVI_FILE, data, out_name=out_Name, /nocopy, $
      ns=ns,nl=nl,nb=nb, offset=offset, bnames=bnames, map_info=map_info
      
    print,'输出完成！' + string(i+1)

    free_lun, lun
  endfor

  print,'完成'
end