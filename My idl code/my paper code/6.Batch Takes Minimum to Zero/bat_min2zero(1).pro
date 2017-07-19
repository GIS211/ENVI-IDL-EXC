pro bat_min2zero
  compile_opt idl2

  envi,/restore_base_save_files
  envi_batch_init,log_file='batch.txt'

  work_dir=DIALOG_PICKFILE(title='选择文件所在路径',/directory)
  CD,work_dir

  file_lists=FILE_SEARCH(work_dir,'*.hdr',count=fnum)
  fbasena=FILE_BASENAME(file_lists,'.hdr')

  e = envi()
  for i=0, fnum-1 do begin
    print,'number: ' + string(i+1)
    raster =  e.openraster(file_lists[i])
    fid = ENVIRasterToFID(raster)
    ENVI_FILE_QUERY,fid,ns=ns,nl=nl,nb=nb,dims=dims ,INTERLEAVE = interleave1,$
      DATA_TYPE = data_type
    MAP_INFO=ENVI_GET_MAP_INFO(fid=fid)
    L = fltarr(ns,nl,nb)

    for j=0,nb-1 do begin
     L[*,*,j] = Envi_Get_Data(Fid = fid, dims = dims, pos = j)
    L[where(~finite(L))] = 0
     ; qt=where(L EQ min(L))
     ; L[qt]=0
;
;      L[*,*,j] = L[*,*,j]*float(L[*,*,j] ne 255)/(L[*,*,j] ne 255);把背景值设置成-nan
    endfor
          ENVI_FILE_MNG,id = fid, /remove
    outputname = 'B:\NDVI\' +fbasena[i] 
    ;    ENVI_SETUP_HEAD,r_fid=fid, fname=outputname, INTERLEAVE = interleave1,$
    ;      ns=ns,nl=nl,nb=2 , DATA_IGNORE_VALUE=min(L), data_type = data_type ,MAP_INFO=MAP_INFO
    ENVI_WRITE_ENVI_FILE, L, out_name=outputname, /nocopy, $
      ns=ns,nl=nl,nb=nb, map_info=map_info

  endfor
  print,'all done!'
;  ENVI_BATCH_EXIT
end