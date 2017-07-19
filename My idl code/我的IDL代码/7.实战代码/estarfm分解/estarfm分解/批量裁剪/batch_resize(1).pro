pro batch_resize

  ;open the fine image of the first pair
  work_dir=DIALOG_PICKFILE(title='选择fine image1文件所在路径',/directory)
  CD,work_dir

  FileName1=FILE_SEARCH(work_dir,'*.hdr',count=fnum);获取以_MTL.txt结尾的文件，并记录数量
  basena=FILE_BASENAME(FileName1,'.hdr');获取_MTL.TXT文件名
  
  ;please set the following parameters设置变量信息，具体设置和相关内容要参考文献
  ;----------------------------------------------------------------------
  w=25.0                 ;set the haif window size, if 25, the window size is 25*2+1=51 fine pixels
  num_class=6.0          ;不变set the estimated number of classes, please set a larger value if blending images with very few bands
  DN_min=0               ;最大最小值，这里是MODIS的0~10000之间，在运算的时候也可以使用扩大10000倍的方法set the range of DN value of the image,If byte, 0 and 255
  DN_max=10000.0
  patch_long=1000         ;设置最小处理块，按区域处理应该设置小一些，依据数据大小决定set the size of block,if process whole ETM scene, set 500
  temp_file='B:\temp\'    ;设置临时文件存储位置set the temporary file location
  ;------------------------------------------------------------------------
  ;
  ;  FileName1 = Dialog_PickFile(title = 'open the fine image of the first pair:')
  for i = 0 , fnum-1 do begin

    envi_open_file,FileName1[i],r_fid=fid
    envi_file_query,fid,ns=ns,nl=nl,nb=nb,dims=dims
    map_info = envi_get_map_info(fid = fid)

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
    for isub=0,n_ns*n_nl-1,1 do begin;第三个参数为增量，块的数量循环
      dims=[-1,ind_patch[0,isub],ind_patch[1,isub],ind_patch[2,isub],ind_patch[3,isub]];第一个块的维度（行列）
      envi_doit, 'resize_doit', fid=fid, pos=pos, dims=dims, interp=0, rfact=[1,1], $;其中，interp为最邻近插值法，rfact为缩小比例
        out_name=tempoutname+strtrim(isub+1,1), r_fid=r_fid1;批量裁剪
      envi_file_mng, id=r_fid1, /remove;清空裁剪临时文件
    endfor

    envi_file_mng, id=fid, /remove
  endfor

;  ;open the coarse image of the first pair粗分辨率影像也进行相应的处理
;  ;-----------------------------------------------------------
;  FileName2 = Dialog_PickFile(title = 'open the coarse image of the first pair:')
;  envi_open_file,FileName2,r_fid=fid
;  tempoutname=temp_file+'\temp_C1';临时文件存储
;  pos=indgen(nb)
;  for isub=0,n_ns*n_nl-1,1 do begin
;    dims=[-1,ind_patch[0,isub],ind_patch[1,isub],ind_patch[2,isub],ind_patch[3,isub]]
;    envi_doit, 'resize_doit', fid=fid, pos=pos, dims=dims, interp=0, rfact=[1,1], $
;      out_name=tempoutname+strtrim(isub+1,1), r_fid=r_fid1
;    envi_file_mng, id=r_fid1, /remove
;  endfor
;  envi_file_mng, id=fid, /remove
;
;
;  ;open the fine image of the second pair第二景中分辨率影像
;  ;-----------------------------------------------------------
;  FileName3 = Dialog_PickFile(title = 'open the fine image of the second pair:')
;  envi_open_file,FileName3,r_fid=fid
;  tempoutname=temp_file+'\temp_F2'
;  pos=indgen(nb)
;  for isub=0,n_ns*n_nl-1,1 do begin
;    dims=[-1,ind_patch[0,isub],ind_patch[1,isub],ind_patch[2,isub],ind_patch[3,isub]]
;    envi_doit, 'resize_doit', fid=fid, pos=pos, dims=dims, interp=0, rfact=[1,1], $
;      out_name=tempoutname+strtrim(isub+1,1), r_fid=r_fid1
;    envi_file_mng, id=r_fid1, /remove
;  endfor
;  envi_file_mng, id=fid, /remove
;
;  ;open the coarse image of the second pair第二景粗分辨率影像
;  ;-----------------------------------------------------------
;
;  FileName4 = Dialog_PickFile(title = 'open the coarse image of the second pair:')
;  envi_open_file,FileName4,r_fid=fid
;  tempoutname=temp_file+'\temp_C2'
;  pos=indgen(nb)
;  for isub=0,n_ns*n_nl-1,1 do begin
;    dims=[-1,ind_patch[0,isub],ind_patch[1,isub],ind_patch[2,isub],ind_patch[3,isub]]
;    envi_doit, 'resize_doit', fid=fid, pos=pos, dims=dims, interp=0, rfact=[1,1], $
;      out_name=tempoutname+strtrim(isub+1,1), r_fid=r_fid1
;    envi_file_mng, id=r_fid1, /remove
;  endfor
;  envi_file_mng, id=fid, /remove
;
;  ;open the coarse image of the prediction time预测时间的粗分辨率影像
;  ;-----------------------------------------------------------
;  FileName5 = Dialog_PickFile(title = 'open the coarse image of the prediction time:')
;  envi_open_file,FileName5,r_fid=fid
;  tempoutname=temp_file+'\temp_C0'
;  pos=indgen(nb)
;  for isub=0,n_ns*n_nl-1,1 do begin
;    dims=[-1,ind_patch[0,isub],ind_patch[1,isub],ind_patch[2,isub],ind_patch[3,isub]]
;    envi_doit, 'resize_doit', fid=fid, pos=pos, dims=dims, interp=0, rfact=[1,1], $
;      out_name=tempoutname+strtrim(isub+1,1), r_fid=r_fid1
;    envi_file_mng, id=r_fid1, /remove
;  endfor
;  envi_file_mng, id=fid, /remove


end