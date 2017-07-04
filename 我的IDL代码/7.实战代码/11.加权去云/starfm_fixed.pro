Pro GetData,ImgData = ImgData,ns = ns,nl = nl,nb = nb,Data_Type = Data_Type,$
  FileName = FileName,Map_info = map_Info, Fid = Fid
  Filter = ['all file;*.*']
  e = ENVI()
  DataColl=e.data
  raster = e.OpenRaster(FileName)
  fid=ENVIRasterToFID(raster)
  ;  Envi_Open_File,FileName,R_Fid = Fid
  Envi_File_Query,Fid,ns = ns,nl = nl,nb = nb,Data_Type = Data_Type
  map_info = envi_get_map_info(Fid = Fid)
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
    Dt = Envi_Get_Data(Fid = Fid , dims = dims , pos=i)
    ImgData[*,*,i] = Dt[*,*]
  EndFor

  datacoll.remove,raster
End

pro starfm_fixed
  ;====================================参数设置========================
  w = 13;窗口大小
  num_class = 4;假定分类数
  A = 4;距离因子
  out_dir = 'B:\'
  ;  txtname='B:\in_1.txt'
  ;  base_path='B:\finallist\'
  temp_file='B:\fusion_result\adjust\'
  DN_MIN = 0
  DN_MAX = 10000
  ;====================================参数设置========================
  compile_opt idl2
  envi, /restore_base_save_files
  envi_batch_init, log_file='d:\starfm.log'
  ;====================================get files========================

  file_name_F1 = dialog_pickfile(title = 'fine resolusion map of the 1st pair',FILTER = '*.hdr')
  file_name_C1 = dialog_pickfile(title = 'coarse resolusion map of the 1st pair',FILTER = '*.hdr')
  file_name_C2 = dialog_pickfile(title = 'coarse resolusion map of the 2nd pair single',FILTER = '*.hdr')
  file_name_F2 = dialog_pickfile(title = 'fine resolusion map of the 2st pair single',FILTER = '*.hdr')
  ;file_name_mask = dialog_pickfile(title = 'map of Fmask',FILTER = '*.hdr')

  ; file_all_F1=FILE_SEARCH(file_name_F1,'*.hdr',count=fnum1);计算目标
  F1_basena=FILE_BASENAME(file_name_F1,'.hdr')

  ;  file_all_F2=FILE_SEARCH(file_name_F2,'*.hdr',count=fnum2);辅助目标
  F2_basena=FILE_BASENAME(file_name_F2,'.hdr')

  ; file_all_C1=FILE_SEARCH(file_name_C1,'*.hdr',count=fnum3);modis1
  C1_basena=FILE_BASENAME(file_name_C1,'.hdr')

  ;  file_all_C2=FILE_SEARCH(file_name_C2,'*.hdr',count=fnum4);modis辅助目标
  C2_basena=FILE_BASENAME(file_name_C2,'.hdr')

  GetData,ImgData=fine1,ns = ns,nl = nl,nb = nb,Data_Type = Data_Type,FileName = file_name_F1,Fid = Fid1;插值目标
  fine1=float(fine1)

  GetData,ImgData=fine2,ns = ns1,nl = nl1,nb = nb1,FileName = file_name_F2,Fid = Fid2;必须辅助TM
  fine2=float(fine2)

  GetData,ImgData=coarse1,ns = ns2,nl = nl2,nb = nb2,FileName = file_name_C1,Fid = Fid3;必须辅助modis
  coarse1=FLOAT(coarse1)

  GetData,ImgData=coarse2,ns = ns3,nl = nl3,nb = nb3,FileName = file_name_C2,Fid = Fid4;必须辅助modis
  coarse2=FLOAT(coarse2)

  nl = min([nl,nl1,nl2,nl3,nl4])
  ns = min([ns,ns1,ns2,ns3,ns4])

  for band = 0, 1 do begin;波段循环

    data_F2 = fine2[*,*,band]
    predicted_data = fine1[*,*,band]
    data_C1 = coarse1[*,*,band]
    data_C2 = coarse2[*,*,band]
    ;          fmask1_data = fmask1[*,*]


    fmask_index = where((fine1[*,*,0] gt 2000  or fine1[*,*,0] lt 7500) and (fine1[*,*,1] gt 5000 or fine1[*,*,1] lt 9000))
    if fmask_index[0] eq -1 then continue
    ;获取行列号
    for fmask_index_arr = 0 , fmask_index.length-1 do begin ;像元循环
      d = array_indices(fmask1,fmask_index[fmask_index_arr])
      sample = d[0]
      line = d[1]

      ;取像元
      ;-----------------------------------------窗口边界---------------------------------------------------------------
      ;控制窗口不超过图像边界，还需要控制其内部取值

      ai=max([0,sample-w])
      bi=min([ns-1,sample+w])
      aj=max([0,line-w])
      bj=min([nl-1,line+w])
      ;读取窗口数据
      ci=float(sample-ai)      ;location of target pixel目标像元的值，目标像元是遍历全图像元的值
      cj=float(line-aj)

      data1 = data_C1[ai:bi,aj:bj]
      ;data2 = predicted_data[ai:bi,aj:bj]
      data3 = data_C2[ai:bi,aj:bj]
      data4 = data_F2[ai:bi,aj:bj]

      ;compute the uncertainty,0.2% of each band is uncertain不确定性计算，参照STARFM算法
      uncertain=(DN_max*0.002)*(2^0.5)

      similar_th=stddev(data4,/nan)/num_class

      ;compute the distance of each pixel in the window with the target pixel (integrate window)计算窗口内每一个像元到目标像元的距离
      D_D_all=fltarr((w*2+1),(w*2+1));建立一个窗口
      for jw=0.0,w*2,1 do begin;窗口内部的行列循环
        for iw=0.0,w*2,1 do begin
          D_D_all[iw,jw]=1.0+((w-iw)^2+(w-jw)^2)^0.5/float(w);窗口中每个像元距离(iw,jw)像元的距离
        endfor
      endfor

      D_D_all=reform(D_D_all,(w*2+1)*(w*2+1));从新设置数组，改变数组的维数  目的不明

      position_cand=intarr((bi-ai+1)*(bj-aj+1))+1  ;place the location of each similar pixel存放相似像元所在的位置和值

      cand_band=intarr((bi-ai+1)*(bj-aj+1));一维数组
      wind_fine=data_F2[ai:bi,aj:bj]
      S_S=abs(wind_fine-wind_fine[ci,cj]);用窗体的每一个值减去中心像元的值
      ind_cand=where(S_S lt similar_th);记录值在相似阈值以内的像元位置
      cand_band[ind_cand]=1;将这些位置的值记录为1
      position_cand=position_cand*cand_band;？？？？数值是1的地方是相似像元


      cand_band=0;标记归零

      similar_pixel_index=where(position_cand ne 0,number_cand);记录不为零的像元位置及个数
      if number_cand gt 5 then begin

        S_D_cand=fltarr(number_cand)                ;compute the correlation 建立大小数量的数组
        for icand=0,number_cand-1,1 do begin
          iw=ai+(similar_pixel_index[icand] mod (bi-ai+1));各个相似像元所在位置
          jw=aj+(similar_pixel_index[icand]/(bi-ai+1));
          finecand=data_F2[iw,jw];分别在fine1和fine2中的位置
          coasecand=data_C2[iw,jw];分别在粗分辨率数据当中所在的位置
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
            iw=similar_pixel_index[icand] mod (bi-ai+1)
            jw=similar_pixel_index[icand]/(bi-ai+1)
            D_D_cand[icand]=1.0+((ci-iw)^2+(cj-jw)^2)^0.5/float(w)
          endfor
        endif else begin
          D_D_cand[0:number_cand-1]=D_D_all[similar_pixel_index]      ;integrate window
        endelse

        C_D=(1.0-S_D_cand)*D_D_cand+0.0000001            ;combined distance
        weight=(1.0/C_D)/total(1.0/C_D,/nan)
        ;compute V
        fine_cand=(data4)[similar_pixel_index]
        corse_cand=(data3)[similar_pixel_index]
        if ( stddev(corse_cand,/nan) ge uncertain ) then begin ;to ensure changes in coarse image larger than uncertainty
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

        predicted_data[sample,line]=total(weight*(data_F2[ai:bi,aj:bj])[similar_pixel_index],/nan)

        if (data_F2[sample,line] le DN_min or data_F2[sample,line] ge DN_max) then begin
          predicted_data[sample,line] =total(weight*(data_F2[ai:bi,aj:bj])[similar_pixel_index],/nan)
        endif
      endif else begin   ;for the case of no similar pixel selected 没有相似像元被选中

        predicted_data[sample,line]=data_F2[sample,line]
      endelse

    endfor;像元循环

    PRINT,'band DONE'
    fine1[*,*,band] = predicted_data
  endfor;波段循环


  output_filename = 'B:\fusion_result\adjust\'  + F1_basena
  envi_file_mng,id=fid1,/remove
  envi_file_mng,id=fid2,/remove
  envi_file_mng,id=fid3,/remove
  envi_file_mng,id=fid4,/remove
  
  ENVI_WRITE_ENVI_FILE, fine1, out_name=output_filename, /nocopy, $
    ns=ns,nl=nl,nb=nb, offset=offset, bnames=bnames, map_info=map_info
  print,'写出文件完成！

  print,'done!'
end
