pro output_years2times

  compile_opt idl2
  ENVI,/restore_base_save_files
  ENVI_BATCH_INIT

  file = DIALOG_PICKFILE(title='选择year文件')
  basena=FILE_BASENAME(file,'.dat')

  envi_open_file,file,r_fid=fids
  envi_file_query,fids,ns=ns,nl=nl,nb=nb,dims=dims
  MAP_INFO=ENVI_GET_MAP_INFO(fid=fids)

  img = fltarr(ns,nl,nb)

  img = envi_get_data(fid=fids,dims=dims,pos=0)
  
  img[where(img gt 0)] = 1

  img[where(img ne 1)] = 0

  ref_out = "B:\结果\1.结果\year9"
  ENVI_WRITE_ENVI_FILE, img, out_name=REF_OUT, /nocopy, $
    ns=ns,nl=nl,nb=nb, offset=offset, bnames=bnames, map_info=map_info
end
