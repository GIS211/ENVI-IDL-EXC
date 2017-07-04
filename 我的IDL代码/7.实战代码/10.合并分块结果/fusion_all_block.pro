pro fusion_all_block

  ;open the data to be merge
  work_dir=DIALOG_PICKFILE(title='选择fine image文件所在路径',/directory)
  CD,work_dir

  FileName1=FILE_SEARCH(work_dir,'*.hdr',count=fnum);获取以_MTL.txt结尾的文件，并记录数量
  basena=FILE_BASENAME(FileName1,'.hdr');获取_MTL.TXT文件名

  ;please set the following parameters设置变量信息，具体设置和相关内容要参考文献
  ;----------------------------------------------------------------------

  patch_long=2000         ;设置最小处理块，按区域处理应该设置小一些，依据数据大小决定set the size of block,if process whole ETM scene, set 500
  temp_file='B:\stars分块\1\year_geotif\'    ;设置临时文件存储位置set the temporary file location
  ;------------------------------------------------------------------------
  ;
  ;  FileName1 = Dialog_PickFile(title = 'open the fine image of the first pair:')
  for i = 0 , fnum-1 do begin;循环文件数量

    envi_open_file,FileName1[i], /NO_REALIZE ,r_fid=fid
    envi_file_query,fid,ns=ns,nl=nl,nb=nb,dims=dims
    map_info = envi_get_map_info(fid = fid)
    envi_file_mng, id=fid, /remove
    orig_ns=ns
    orig_nl=nl
    n_ns=ceil(float(ns)/patch_long)
    n_nl=ceil(float(nl)/patch_long);ceil的意思是向正方向舍入 ，n_nl为行分块的位置，ns为列分块的位置

    ind_patch=intarr(4,n_ns*n_nl);ind_patch是4列n_ns*n_nl行数组
    for i_ns=0,n_ns-1,1 do begin;第几个列分块的位置
      for i_nl=0,n_nl-1,1 do begin;第几个行分块的位置
        ind_patch[0,n_ns*i_nl+i_ns]=i_ns*patch_long;第一列存储该块的列起始位置。存储在ind_patch
        ind_patch[1,n_ns*i_nl+i_ns]=min([ns-1,(i_ns+1)*patch_long-1]);第二列存储终止块列位置
        ind_patch[2,n_ns*i_nl+i_ns]=i_nl*patch_long;第一列存储该块的行起始位置
        ind_patch[3,n_ns*i_nl+i_ns]=min([nl-1,(i_nl+1)*patch_long-1]);第二列存储该块的行终止位置
      endfor
    endfor

    tempoutname=temp_file + basena[i] + 'temp';临时文件的存储位置

    pos=indgen(nb);按序列生成数组


    mfid=intarr(n_ns*n_nl)
    mdims=intarr(5,n_ns*n_nl)
    mpos=intarr(nb,n_ns*n_nl)
    pos=indgen(nb)
    x0=intarr(n_ns*n_nl)
    y0=intarr(n_ns*n_nl)

    for isub=0,n_ns*n_nl-1,1 do begin
      envi_open_file, tempoutname+strtrim(isub+1,1), /NO_REALIZE , r_fid= sub_fid
      if (sub_fid eq -1) then begin
        envi_batch_exit
        return
      endif
      envi_file_query,  sub_fid, ns=sub_ns, nl=sub_nl
      mfid[isub] = sub_fid
      mpos[*,isub] = indgen(nb)
      mdims[*,isub] = [-1,0, sub_ns-1,0, sub_nl-1]

      x0[isub]=ind_patch[0,isub]
      y0[isub]=ind_patch[2,isub]

    endfor

    xsize = orig_ns
    ysize = orig_nl
    pixel_size = [1.,1.]

    use_see_through = replicate(1L,n_ns*n_nl)
    see_through_val = replicate(0L,n_ns*n_nl)

    out_name='B:\NDVI_STACK_fusion\' + basena[i]
    envi_doit, 'mosaic_doit', fid=mfid, pos=mpos, $
      dims=mdims, out_name=out_name, xsize=xsize, $
      ysize=ysize, x0=x0, y0=y0, georef=0,MAP_INFO=map_info, $
      out_dt=4, pixel_size=pixel_size, $
      background=0, see_through_val=see_through_val, $
      use_see_through=use_see_through;
    for p=0,n_ns*n_nl-1,1 do begin
      envi_file_mng, id=mfid[p], /remove
    endfor

  endfor
  print,'done'
end