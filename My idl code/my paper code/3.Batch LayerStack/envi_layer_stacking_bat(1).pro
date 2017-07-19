;2016/9/1 debug for to while

pro envi_layer_stacking_bat
  compile_opt idl2

  envi,/restore_base_save_files
  envi_batch_init,log_file='batch.txt'

  work_dir=DIALOG_PICKFILE(title='选择文件所在路径',/directory)
  CD,work_dir

  file_lists=FILE_SEARCH(work_dir,'*.tif',count=fnum)
  fbasena=FILE_BASENAME(file_lists)

  sign = ceil(fnum/6)
  j=0
  a='a'
  ind_num = intarr(6)
  file_nam = strarr(sign)


  for i=0,fnum-1 do begin;循环获取所有文件名
    chart_name = strmid(fbasena[i],0,23);获取前23个独一无二的字符串进行匹配

    if a ne chart_name then begin
      a = chart_name
      file_nam[j] = a;存储独一无二的识别文件名
      j=j+1
    endif
  endfor

  for j=0,sign-1 do begin;头串循环
    T=0
    for i =0, fnum-1 do begin
      if STRCMP(file_nam[j],strmid(fbasena[i],0,23), 23, /FOLD_CASE) then begin
        ind_num[T] = i
        T =T+1
        endif
    endfor



    outputfile ='I:\modis_layerstack\'+string(file_nam[j]+'.tif');循环改变输出文件名

    fids=lonarr(n_elements(ind_num))
    dimses=lonarr(5,n_elements(ind_num))
    poses=lonarr(n_elements(ind_num))

    for i=0,n_elements(ind_num)-1 do begin
      envi_open_file,file_lists[ind_num[i]],r_fid=fids1
      envi_file_query,fids1,ns=ns,nl=nl,nb=nb

      fids[i]=fids1
      dimses[0,i]=[-1,0,ns-1,0,nl-1]
      proj=envi_get_projection(fid=fids,pixel_size=out_ps)

    endfor

    envi_doit,'envi_layer_stacking_doit',$
      fid=fids,pos=poses,dims=dimses,$
      out_dt=4,out_name=outputfile,$
      interp=2,out_ps=out_ps,$
      out_proj=proj,r_fid=r_fid
    for i=0,n_elements(ind_num)-1 do begin
      envi_file_mng,id=fids[i],/remove
    endfor
    envi_file_mng,id=r_fid,/remove
    print,'complected number:'+string(j+1)
  endfor


end