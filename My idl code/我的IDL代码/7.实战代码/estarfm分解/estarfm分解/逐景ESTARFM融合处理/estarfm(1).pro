
;---------------------------------------------------------------------------------
;                            ESTARFM PROGRAM
;               Using two pairs of fine and coarse images
;              the program can be used for whole TM scene
;        Developed by (1) Zhu Xiaolin,email: zhuxiaolin55@gmail.com
;             Beijing Normal University & the Ohio State University
;             
;            Debugging history: 
;            1)5/12/2012 correct the abnormal prediction
;            2)9/29/2013 correct the spatial distance calculation for integrate window 
;            3)7/10/2014 correct the abnormal value of spectral distance and use all bands to indentify background
;     
;Please cite the reference: Xiaolin Zhu, Jin Chen, Feng Gao, & Jeffrey G Masek.
;An enhanced spatial and temporal adaptive reflectance fusion model for complex
;heterogeneous regions. Remote Sensing of Environment,2010,114,2610-2623
;
;                     Copyright belong to Xiaolin Zhu
;---------------------------------------------------------------------------------


;function for open the file定义了一个getdata函数

Pro GetData,ImgData = ImgData,ns = ns,nl = nl,nb = nb,Data_Type = Data_Type,$
    FileName = FileName,Map_info = map_Info, Fid = Fid
    Filter = ['all file;*.*']
    Envi_Open_File,FileName,R_Fid = Fid
    Envi_File_Query,Fid,ns = ns,nl = nl,nb = nb,Data_Type = Data_Type
    map_info = envi_get_map_info(fid=Fid)
    dims = [-1,0,ns - 1 ,0,nl - 1]
    case Data_Type Of
        1:ImgData = BytArr(ns,nl,nb)    ;  BYTE  Byte
        2:ImgData = IntArr(ns,nl,nb)    ;  INT  Integer
        3:ImgData = LonArr(ns,nl,nb)    ;  LONG  Longword integer
        4:ImgData = FltArr(ns,nl,nb)    ;  FLOAT  Floating point
        5:ImgData = DblArr(ns,nl,nb)    ;  DOUBLE  Double-precision floating
        6:ImgData = COMPLEXARR(ns,nl,nb); complex, single-precision, floating-point
        9:ImgData = DCOMPLEXARR(ns,nl,nb);complex, double-precision, floating-point
        12:ImgData = UINTARR(ns,nl,nb)   ; unsigned integer vector or array
        13:ImgData = ULONARR(ns,nl,nb)   ;  unsigned longword integer vector or array
        14:ImgData = LON64ARR(ns,nl,nb)   ;a 64-bit integer vector or array
        15:ImgData = ULON64ARR(ns,nl,nb)   ;an unsigned 64-bit integer vector or array
    EndCase
    For i = 0,nb-1 Do Begin
       Dt = Envi_Get_Data(Fid = Fid,dims = dims,pos=i);
       ImgData[*,*,i] = Dt[*,*]
    EndFor
End

;-------------------------------------------------------------------
;                       main program
;-------------------------------------------------------------------

pro  ESTARFM

 t0=systime(1)                  ;the initial time of program running

 ;please set the following parameters设置变量信息，具体设置和相关内容要参考文献
;----------------------------------------------------------------------
 w=25.0                 ;set the haif window size, if 25, the window size is 25*2+1=51 fine pixels
 num_class=6.0          ;不变set the estimated number of classes, please set a larger value if blending images with very few bands
 DN_min=0               ;最大最小值，这里是MODIS的0~10000之间，在运算的时候也可以使用扩大10000倍的方法set the range of DN value of the image,If byte, 0 and 255
 DN_max=10000.0
 patch_long=300         ;设置最小处理块，按区域处理应该设置小一些，依据数据大小决定set the size of block,if process whole ETM scene, set 500
 temp_file='D:\temp'    ;设置临时文件存储位置set the temporary file location
;------------------------------------------------------------------------


 ;open the fine image of the first pair
;  FileName1 = Dialog_PickFile(title = 'open the fine image of the first pair:')
;  envi_open_file,FileName1,r_fid=fid
;  envi_file_query,fid,ns=ns,nl=nl,nb=nb,dims=dims
;  map_info = envi_get_map_info(fid = fid)
;  
;  orig_ns=ns
;  orig_nl=nl
;  n_ns=ceil(float(ns)/patch_long)
;  n_nl=ceil(float(nl)/patch_long);ceil的意思是向正方向舍入 ，n_nl为行分块的位置，ns为列分块的位置
;
;  ind_patch=intarr(4,n_ns*n_nl);ind_patch是4列n_ns*n_nl行数组
;  for i_ns=0,n_ns-1,1 do begin;第几个列分块的位置
;    for i_nl=0,n_nl-1,1 do begin;第几个行分块的位置
;        ind_patch[0,n_ns*i_nl+i_ns]=i_ns*patch_long;第一列存储该块的列起始位置。存储在ind_patch
;        ind_patch[1,n_ns*i_nl+i_ns]=min([ns-1,(i_ns+1)*patch_long-1]);第二列存储终止块列位置
;        ind_patch[2,n_ns*i_nl+i_ns]=i_nl*patch_long;第一列存储该块的行起始位置
;        ind_patch[3,n_ns*i_nl+i_ns]=min([nl-1,(i_nl+1)*patch_long-1]);第二列存储该块的行终止位置
;    endfor
;  endfor
;
;  tempoutname=temp_file+'\temp_F1';临时文件的存储位置
;
;  pos=indgen(nb);按序列生成数组
;  for isub=0,n_ns*n_nl-1,1 do begin;第三个参数为增量，块的数量循环
;      dims=[-1,ind_patch[0,isub],ind_patch[1,isub],ind_patch[2,isub],ind_patch[3,isub]];第一个块的维度（行列）
;      envi_doit, 'resize_doit', fid=fid, pos=pos, dims=dims, interp=0, rfact=[1,1], $;其中，interp为最邻近插值法，rfact为缩小比例
;      out_name=tempoutname+strtrim(isub+1,1), r_fid=r_fid1;批量裁剪
;      envi_file_mng, id=r_fid1, /remove;清空裁剪临时文件
;  endfor
;
;  envi_file_mng, id=fid, /remove
;
;
;;open the coarse image of the first pair粗分辨率影像也进行相应的处理
;  ;-----------------------------------------------------------
;  FileName2 = Dialog_PickFile(title = 'open the coarse image of the first pair:')
;  envi_open_file,FileName2,r_fid=fid
;  tempoutname=temp_file+'\temp_C1';临时文件存储
;  pos=indgen(nb)
;  for isub=0,n_ns*n_nl-1,1 do begin
;      dims=[-1,ind_patch[0,isub],ind_patch[1,isub],ind_patch[2,isub],ind_patch[3,isub]]
;      envi_doit, 'resize_doit', fid=fid, pos=pos, dims=dims, interp=0, rfact=[1,1], $
;      out_name=tempoutname+strtrim(isub+1,1), r_fid=r_fid1
;      envi_file_mng, id=r_fid1, /remove
;  endfor
;   envi_file_mng, id=fid, /remove
;
;
;    ;open the fine image of the second pair第二景中分辨率影像
;  ;-----------------------------------------------------------
;  FileName3 = Dialog_PickFile(title = 'open the fine image of the second pair:')
;  envi_open_file,FileName3,r_fid=fid
;  tempoutname=temp_file+'\temp_F2'
;  pos=indgen(nb)
;  for isub=0,n_ns*n_nl-1,1 do begin
;      dims=[-1,ind_patch[0,isub],ind_patch[1,isub],ind_patch[2,isub],ind_patch[3,isub]]
;      envi_doit, 'resize_doit', fid=fid, pos=pos, dims=dims, interp=0, rfact=[1,1], $
;      out_name=tempoutname+strtrim(isub+1,1), r_fid=r_fid1
;      envi_file_mng, id=r_fid1, /remove
;  endfor
;   envi_file_mng, id=fid, /remove
;
;  ;open the coarse image of the second pair第二景粗分辨率影像
;  ;-----------------------------------------------------------
;
;  FileName4 = Dialog_PickFile(title = 'open the coarse image of the second pair:')
;  envi_open_file,FileName4,r_fid=fid
;  tempoutname=temp_file+'\temp_C2'
;  pos=indgen(nb)
;  for isub=0,n_ns*n_nl-1,1 do begin
;      dims=[-1,ind_patch[0,isub],ind_patch[1,isub],ind_patch[2,isub],ind_patch[3,isub]]
;      envi_doit, 'resize_doit', fid=fid, pos=pos, dims=dims, interp=0, rfact=[1,1], $
;      out_name=tempoutname+strtrim(isub+1,1), r_fid=r_fid1
;      envi_file_mng, id=r_fid1, /remove
;  endfor
;   envi_file_mng, id=fid, /remove
;
; ;open the coarse image of the prediction time预测时间的粗分辨率影像
;  ;-----------------------------------------------------------
;  FileName5 = Dialog_PickFile(title = 'open the coarse image of the prediction time:')
;  envi_open_file,FileName5,r_fid=fid
;  tempoutname=temp_file+'\temp_C0'
;  pos=indgen(nb)
;  for isub=0,n_ns*n_nl-1,1 do begin
;      dims=[-1,ind_patch[0,isub],ind_patch[1,isub],ind_patch[2,isub],ind_patch[3,isub]]
;      envi_doit, 'resize_doit', fid=fid, pos=pos, dims=dims, interp=0, rfact=[1,1], $
;      out_name=tempoutname+strtrim(isub+1,1), r_fid=r_fid1
;      envi_file_mng, id=r_fid1, /remove
;  endfor
;   envi_file_mng, id=fid, /remove
;

;------------------------------------------------------------------
        ;process  each block
;-------------------------------------------------------------------


print,'there are total',n_ns*n_nl,' blocks'

for isub=0,n_ns*n_nl-1,1 do begin;逐块进行运算

;open each block image分别打开五个数据对应的块。filename这里是block，应该对应的是云阴影的位置

    FileName = temp_file+'\temp_F1';<------------------------------
     GetData,ImgData=fine1,ns = ns,nl = nl,nb = nb,Data_Type = Data_Type,FileName = FileName+strtrim(isub+1,1),Fid = Fid1
    fine1=float(fine1)

    FileName = temp_file+'\temp_C1'
    GetData,ImgData=coarse1,FileName = FileName+strtrim(isub+1,1),Fid = Fid2
    coarse1=FLOAT(coarse1)

    FileName = temp_file+'\temp_F2'
    GetData,ImgData=fine2,FileName = FileName+strtrim(isub+1,1),Fid = Fid3
    fine2=FLOAT(fine2)

    FileName = temp_file+'\temp_C2'
    GetData,ImgData=coarse2,FileName = FileName+strtrim(isub+1,1),Fid = Fid4
    coarse2=FLOAT(coarse2)

    FileName = temp_file+'\temp_C0'
    GetData,ImgData=coarse0,FileName = FileName+strtrim(isub+1,1),Fid = Fid5
    coarse0=FLOAT(coarse0)
;----------------------------------------------
    FileName = temp_file+'\temp_C0';<------------------------------
    GetData,ImgData=fmask,FileName = FileName+strtrim(isub+1,1),Fid = Fid6
;------------------------------------------------------------    
    fine0=fltarr(ns,nl,nb)     ;place the blended result存放结果  预测数据

   ;compute the uncertainty,0.2% of each band is uncertain不确定性计算，参照STARFM算法
    uncertain=(DN_max*0.002)*(2^0.5)

     similar_th=fltarr(nb,2)          ;compute the threshold of similar pixel seeking计算相似像元的搜索阈值

     for iband=0,nb-1,1 do begin;逐波段计算
       similar_th[iband,0]=stddev(fine1[*,*,iband])*2.0/num_class   ;pair 1第一波段的阈值
       similar_th[iband,1]=stddev(fine2[*,*,iband])*2.0/num_class   ;pair 2第二波段的阈值
     endfor

     ;compute the distance of each pixel in the window with the target pixel (integrate window)计算窗口内每一个像元到目标像元的距离
     D_D_all=fltarr((w*2+1),(w*2+1));建立一个窗口
        for jw=0.0,w*2,1 do begin;窗口内部的行列循环
          for iw=0.0,w*2,1 do begin
            D_D_all[iw,jw]=1.0+((w-iw)^2+(w-jw)^2)^0.5/float(w);窗口中每个像元距离(iw,jw)像元的距离
          endfor
        endfor
        
     D_D_all=reform(D_D_all,(w*2+1)*(w*2+1));从新设置数组，改变数组的维数  目的不明

     for j=0,nl-1,1 do begin              ;retieve each target pixel
       for i=0,ns-1,1 do begin

        if (total(fine1[i,j,*]) ne 0 and total(fine2[i,j,*]) ne 0) then begin    ;do not process the background 跳过背景进行处理、、、、

          ai=max([0,i-w])       ; the window location窗口的计算从0坐标开始，这里分别是上下左右的坐标值
          bi=min([ns-1,i+w])
          aj=max([0,j-w])
          bj=min([nl-1,j+w])

          ci=float(i-ai)      ;location of target pixel目标像元的值，目标像元是遍历全图像元的值
          cj=float(j-aj)

          position_cand=intarr((bi-ai+1)*(bj-aj+1))+1  ;place the location of each similar pixel存放相似像元所在的位置和值

          for ipair=0,1,1 do begin
             for iband=0,nb-1,1 do begin;循环波段
                 cand_band=intarr((bi-ai+1)*(bj-aj+1));一维数组
                 if ipair eq 0 then begin;对第一和第二个中分辨率影像窗口中的值进行获取
                    wind_fine=fine1[ai:bi,aj:bj,iband]
                 endif else begin
                    wind_fine=fine2[ai:bi,aj:bj,iband]
                 endelse
                 S_S=abs(wind_fine-wind_fine[ci,cj]);用窗体的每一个值减去中心像元的值
                 ind_cand=where(S_S lt similar_th[iband,ipair]);记录值在相似阈值以内的像元位置
                 cand_band[ind_cand]=1;将这些位置的值记录为1
                 position_cand=position_cand*cand_band;？？？？数值是1的地方是相似像元
             endfor
          endfor
          cand_band=0;标记归零
          indcand=where(position_cand ne 0,number_cand);记录不为零的像元位置及个数

          if (number_cand gt 5) then begin;如果相似像元大于5个

             S_D_cand=fltarr(number_cand)                ;compute the correlation 建立大小数量的数组
             for icand=0,number_cand-1,1 do begin
                iw=ai+(indcand[icand] mod (bi-ai+1));各个相似像元所在位置
                jw=aj+(indcand[icand]/(bi-ai+1));
                finecand=[fine1[iw,jw,*],fine2[iw,jw,*]];分别在fine1和fine2中的位置
                coasecand=[coarse1[iw,jw,*],coarse2[iw,jw,*]];分别在粗分辨率数据当中所在的位置
                if (max(finecand) eq min(finecand) or max(coasecand) eq min(coasecand)) then begin;如果中分辨率和粗分辨率的位置相同
                   S_D_cand[icand]=1.0;相关性视为1
                endif else begin
                S_D_cand[icand]=CORRELATE(finecand,coasecand);否则，相关性进行计算
                  if (S_D_cand[icand] ne S_D_cand[icand]) then begin
                     S_D_cand[icand]=0              ;correct the NaN value of correlation纠正空值
                  endif
                endelse
              endfor

              D_D_cand=fltarr(number_cand)        ;spatial distance
              if ((bi-ai+1)*(bj-aj+1) lt (w*2.0+1)*(w*2.0+1)) then begin   ;not integrate window
                 for icand=0,number_cand-1,1 do begin
                   iw=indcand[icand] mod (bi-ai+1)
                   jw=indcand[icand]/(bi-ai+1)
                   D_D_cand[icand]=1.0+((ci-iw)^2+(cj-jw)^2)^0.5/float(w)
                 endfor
              endif else begin
                 D_D_cand[0:number_cand-1]=D_D_all[indcand]      ;integrate window
              endelse

                 C_D=(1.0-S_D_cand)*D_D_cand+0.0000001            ;combined distance
                 weight=(1.0/C_D)/total(1.0/C_D)

              for iband=0,nb-1,1 do begin     ;compute V

                  fine_cand=[(fine1[ai:bi,aj:bj,iband])[indcand],(fine2[ai:bi,aj:bj,iband])[indcand]]
                  corse_cand=[(coarse1[ai:bi,aj:bj,iband])[indcand],(coarse2[ai:bi,aj:bj,iband])[indcand]]
                  if ( stddev(corse_cand) ge uncertain ) then begin ;to ensure changes in coarse image larger than uncertainty
                        regress_result=regress(corse_cand,fine_cand,FTEST=fvalue)
                        sig=1.0-f_pdf(fvalue,1,number_cand*2-2)
                           ;correct the result with no significancy or inconsistent change or too large value  
                        if (sig le 0.05 and regress_result[0] gt 0 and regress_result[0] le 5) then begin
                             V_cand=regress_result[0]
                        endif else begin
                             V_cand=1.0
                        endelse
                  endif else begin
                        V_cand=1.0
                  endelse

                    ; compute the temporal weight
                     difc_pair1=abs(mean(coarse0[ai:bi,aj:bj,iband])-mean(coarse1[ai:bi,aj:bj,iband]))+0.01^5
                     difc_pair2=abs(mean(coarse0[ai:bi,aj:bj,iband])-mean(coarse2[ai:bi,aj:bj,iband]))+0.01^5
                  

                    ;predict from pair1
                     coase0_cand=(coarse0[ai:bi,aj:bj,iband])[indcand]
                     coase1_cand=(coarse1[ai:bi,aj:bj,iband])[indcand]
                     fine01=fine1[i,j,iband]+total(weight*V_cand*(coase0_cand-coase1_cand))
                     ;predict from pair2
                     ;the final prediction
                     fine0[i,j,iband]=fine01
                     ;revise the abnormal prediction
                     if (fine0[i,j,iband] le DN_min or fine0[i,j,iband] ge DN_max) then begin
                        fine01=total(weight*(fine1[ai:bi,aj:bj,iband])[indcand])
                        fine0[i,j,iband]=fine01
                     endif
                  endfor
               endif else begin   ;for the case of no similar pixel selected 没有相似像元被选中

                     ; compute the temporal weight

                        fine0[i,j,iband]=fine1[i,j,iband]

               endelse
              endif
             endfor
            endfor

     ; change the type of prediction into the type same as the input image
    case Data_Type Of
        1:fine0 = Byte(fine0)    ;  BYTE  Byte
        2:fine0 = FIX(fine0)     ;  INT  Integer
        3:fine0 = LONG(fine0)    ;  LONG  Longword integer
        4:fine0 = FLOAT(fine0)   ;  FLOAT  Floating point
        5:fine0 = DOUBLE(fine0)  ;  DOUBLE  Double-precision floating
        6:fine0 = COMPLEX(fine0); complex, single-precision, floating-point
        9:fine0 = DCOMPLEX(fine0);complex, double-precision, floating-point
        12:fine0 = UINT(fine0)   ; unsigned integer vector or array
        13:fine0 = ULONG(fine0)   ;  unsigned longword integer vector or array
        14:fine0 = LONG64(fine0)   ;a 64-bit integer vector or array
        15:fine0a = ULONG64(fine0)   ;an unsigned 64-bit integer vector or array
    EndCase

       print,'finished ',isub+1,' block'
         tempoutname1=temp_file+'\temp_blended'
         Envi_Write_Envi_File,fine0,Out_Name = tempoutname1+strtrim(isub+1,1)
         envi_file_mng, id=Fid1, /remove, /delete
         envi_file_mng, id=Fid2, /remove, /delete
         envi_file_mng, id=Fid3, /remove, /delete
         envi_file_mng, id=Fid4, /remove, /delete
         envi_file_mng, id=Fid5, /remove, /delete
endfor

;--------------------------------------------------------------------------------------
;mosiac all the blended patch

  mfid=intarr(n_ns*n_nl)
  mdims=intarr(5,n_ns*n_nl)
  mpos=intarr(nb,n_ns*n_nl)
  pos=indgen(nb)
  x0=intarr(n_ns*n_nl)
  y0=intarr(n_ns*n_nl)

  for isub=0,n_ns*n_nl-1,1 do begin
      envi_open_file, tempoutname1+strtrim(isub+1,1), r_fid= sub_fid
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

    out_name=FileName5+'_ESTARFM'
    envi_doit, 'mosaic_doit', fid=mfid, pos=mpos, $
    dims=mdims, out_name=out_name, xsize=xsize, $
    ysize=ysize, x0=x0, y0=y0, georef=0,MAP_INFO=map_info, $
    out_dt=Data_Type, pixel_size=pixel_size, $
    background=0, see_through_val=see_through_val, $
    use_see_through=use_see_through

    for i=0,n_ns*n_nl-1,1 do begin
      envi_file_mng, id=mfid[i], /remove, /delete
    endfor

print, 'time used:', floor((systime(1)-t0)/3600), 'h',floor(((systime(1)-t0) mod 3600)/60),'m',(systime(1)-t0) mod 60,'s'


end