;+
; :Author: LEE
; ;生成tm、modis和fmsak的位置路径
; ；分别读取插值目标和辅助目标的分块文件
; ；分别打开插值目标和辅助目标
; 数据份年份循环
; 分波段循环
; ;对阴影所在位置进行计算，获取结果
; ;对阴影数据进行赋值
; ;输出
;-
Pro GetData,ImgData = ImgData,ns = ns,nl = nl,nb = nb,Data_Type = Data_Type,$
  FileName = FileName,Map_info = map_Info, Fid = Fid
  Filter = ['all file;*.*']
  e = ENVI()
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
End

pro image_merge_test
  compile_opt idl2
  envi, /restore_base_save_files
  envi_batch_init
  ;设置参数 计算窗口 计算波段 假定分类数 距离因子 位置信息
  p = 0 ;规定计算的波段数：0-1、1-2、2-3、3-4
  w = 25;窗口大小
  class_num = 4;假定分类数
  A = 4;距离因子

  txtname='B:\in.txt'
  base_path='B:\finallists\'
  temp_file='D:\temp'
  ;----------------------------------------------------------

  ;选择landsat modis fmask文件所在路径，以及最后预测文件输出的路径，让预测文件的文件名和源文件一样，做完以后，备份源文件后可直接替代
  file_name_F1 = dialog_pickfile(title = 'fine resolusion map of TM', /directory)
  file_name_C1 = dialog_pickfile(title = 'coarse resolusion map of MODIS', /directory)
  file_name_F2 = 'B:\';预测文件的输出位置
  file_name_mask = dialog_pickfile(title = 'coarse resolusion map of Fmask',/directory)

  file_all_F1=FILE_SEARCH(file_name_F1,'*.hdr',count=fnum1);landsat
  F1_basena=FILE_BASENAME(file_all_F1,'.hdr')

  file_all_C1=FILE_SEARCH(file_name_C1,'*.hdr',count=fnum2);modis1
  C1_basena=FILE_BASENAME(file_all_C1,'.hdr')

    file_all_mask1=FILE_SEARCH(file_name_mask,'*.hdr',count=fnum3);fmask1
   mask_basena=FILE_BASENAME(file_all_mask1,'.hdr')

  ;  file_all_C2 = file_all_C1;modis2
  ;  C2_basena = C1_basena

  ;获取可用文件后缀数组a
  temp=''
  openr,lun,txtname,/get_lun
  n_line = file_lines(txtname)
  xa = strarr(n_line)
  i=0
  while ~eof(lun) do begin
    readf,lun,temp
    xa[i] = temp
    i=i+1
  endwhile
  free_Lun,lun

  ;file_road = file_dirname(file_all_mask1[0]);获取文件路径

  v_F1_basena = strarr(fnum1)
  v_C1_basena = strarr(fnum1)
  v_mask_basena = strarr(fnum1)

  k=0
  z=0
  q=0
  for t = 0 , n_line-1 do begin;循环有效文件
    for i = 0 , fnum1-1 do begin;循环所有文件以求所有有效文件名
      if strcmp(strmid(F1_basena[i],3,4,/REVERSE_OFFSET),xa[t]) then begin
        v_F1_basena[k] = F1_basena[i]
        k=k+1
      endif

      if strcmp(strmid(mask_basena[i],3,4,/REVERSE_OFFSET),xa[t]) then begin
      v_mask_basena[z] = mask_basena[i]
      z=z+1
      endif

      if strcmp(strmid(C1_basena[i],3,4,/REVERSE_OFFSET),xa[t]) then begin
        v_c1_basena[q] = C1_basena[i]
        q=q+1
      endif
    endfor
  endfor


  print,'there are total 390 blocks'

  for indx1 = 0 , k-1 do begin;文件循环
    if strcmp(strmid(v_F1_basena[indx1],3,4),2014) then continue

    temp_C1 = base_path + 'MODIS\' + v_c1_basena[indx1] + '.hdr'
    temp_F1 = base_path + 'tm\' + v_F1_basena[indx1] + '.hdr'
    temp_fmask1 = base_path + 'fmask\' + v_mask_basena[indx1] + '.hdr'

    ;获取下几年年数据，x为年的倍数

;    z=indx1*13
    temp_F2 = base_path + 'tm\' + v_F1_basena[indx1+1] + '.hdr'
    temp_C2 = base_path + 'MODIS\' + v_c1_basena[indx1+1] + '.hdr'
    temp_fmask2 = base_path + 'fmask\' + v_mask_basena[indx1+1] + '.hdr'


    GetData,ImgData=fine1,ns = ns,nl = nl,nb = nb,Data_Type = Data_Type,FileName = temp_F1,Fid = Fid1;插值目标
    fine1=float(fine1)

    GetData,ImgData=fine2,FileName = temp_F2,Fid = Fid2;必须辅助TM
    fine2=float(fine2)

    GetData,ImgData=coarse1,FileName = temp_C1,Fid = Fid3;必须辅助modis
    coarse1=FLOAT(coarse1/10000.0)

    GetData,ImgData=coarse2,FileName = temp_C2,Fid = Fid4;必须辅助modis
    coarse2=FLOAT(coarse2/10000.0)

    GetData,ImgData=fmask1,FileName = temp_fmask1,Fid = Fid5;必须辅助1云判断
    fmask1=FLOAT(fmask1)

    GetData,ImgData=fmask2,FileName = temp_fmask2,Fid = Fid6;非必须除云判断
    fmask2=FLOAT(fmask2)


    ;    envi_open_file,file_name_F1,r_fid = fid_0203_30
    ;    envi_open_file,file_name_C1,r_fid = fid_0203_150
    ;    ;  envi_open_file,file_name_F2,r_fid = fid_0204_30
    ;    envi_open_file,file_name_C2,r_fid = fid_0204_150
    ;
    ;    envi_file_query, fid_0203_150, ns=ns, nl=nl, nb=nb , dims = dims
    ;    data_0203_150 = float(ENVI_GET_DATA(fid=fid_0203_150, dims=dims, pos=p))
    ;
    ;    envi_file_query, fid_0203_30, ns=ns, nl=nl, nb=nb , dims = dims
    ;    data_0203_30 = float(ENVI_GET_DATA(fid=fid_0203_30, dims=dims, pos=p))
    ;
    ;    envi_file_query, fid_0204_150, ns=ns, nl=nl, nb=nb , dims = dims
    ;    data_0204_150 = float(ENVI_GET_DATA(fid=fid_0204_150, dims=dims, pos=p))

    ;  envi_file_query, fid_0204_30, ns=ns, nl=nl, nb=nb , dims = dims
    ;  data_0204_30 = float(ENVI_GET_DATA(fid=fid_0204_30, dims=dims, pos=p))

    for band = 0, 1 do begin;波段循环

      fine_input = fine2[*,*,band]
      predicted_data = fine1[*,*,band]
      coarse1_data = coarse1[*,*,band]
      coarse2_data = coarse2[*,*,band]
      fmask1_data = fmask1[*,*,0]
      fmask2_data = fmask2[*,*,0]
      ;      predicted_data1 = fine1[*,*,0]
      ;      predicted_data2 = fine1[*,*,1]

;      mid_coarse2 = coarse2_data
;      mid_fine_input =  fine_input
      fmask_index = where(fmask1_data eq 2  or fmask1_data eq 4)
      if fmask_index eq -1 then continue
      ;获取行列号
      for fmask_index_arr = 0 , fmask_index.length-1 do begin ;像元循环
        x=0
;        coarse2_data=mid_fine_input
;        coarse2_data=mid_coarse2
        d = array_indices(fmask1_data,fmask_index[fmask_index_arr])
        sample = d[0]
        line = d[1]

;        label1:  IF fmask2_data[sample,line] gt 0.35 then begin
;          x=x+1

;          coarse2_data = coarse2[*,*,band]
;          ;          if x gt 13 then begin
;          ;            x=1
;          ;            temp_F2 = base_path + 'tm\' + v_F1_basena[indx1+x] + '.hdr'
;          ;          GetData,ImgData=fine2,FileName = temp_F2,Fid = Fid2;必须辅助TM
;          ;          fine2=float(fine2)
;          ;          fmask2_data = fine2[*,*,1]
;          GOTO,label1
;
;        ENDIF else begin
;
;        Endelse

        ;if ((fmask2_data[sample,line] eq 2) or (fmask2_data[sample,line] eq 4)) then continue

        ;这里接下来应该是循环行列
        ;for  sample = index_col+(w+1)/2-1 , ns- (w+1)/2 do begin
        ;for  line = index_row+(w+1)/2-1 , nl- (w+1)/2 do begin
        ;获取行列位置

        ;取像元，开窗口
        ;--------------------------------------------------------------------------------------------------------
        if (total(fine1[sample,line,*]) ne 0 ) then begin

          ai=max([0,sample-(w-1)/2])
          bi=min([ns-1,sample+(w-1)/2])
          aj=max([0,line-(w-1)/2])
          bj=min([ns-1,line+(w-1)/2])

          ;data1 = coarse2_data[(sample-(w-1)/2):(sample+(w-1)/2),0:(line+(w-1)/2)]
          ;
          data1 = coarse2_data[ai:bi,aj:bj]
          data2 = fine_input[ai:bi,aj:bj]
          data3 = coarse1_data[ai:bi,aj:bj]
          result = 0
          judge = 0


          similar_pixel_measure = stddev(data2)/class_num;相似像元阈值
          similar_pixel = fltarr(w,w);相似像元的范围
          if similar_pixel_measure ne 0 then begin;如果相似像元分级不为0
            similar_pixel[where((data2-data2[(w-1)/2,(w-1)/2]) lt similar_pixel_measure)] = 1;所有小于阈值的地方都为1
            ;阈值以内的内容视为相似像元的值
          endif else begin;如果为0，说明内部都是相似像元
            similar_pixel = 1
          endelse
          ;图像差值
          r_LM_fine = fltarr(w,w)
          r_MM_fine = fltarr(w,w)
          r_LM = (data1 - data2);整体Sijk
          r_MM = (data1 - data3);Tijk
          r_center_LM = abs(data1[(w-1)/2,(w-1)/2] - data2[(w-1)/2,(w-1)/2]);中心像元的差别Tijk
          r_center_MM = abs(data1[(w-1)/2,(w-1)/2] - data3[(w-1)/2,(w-1)/2]);Sijk
          ;注释：
          ;1、排除窗口中心像元差为0的情况，该情况视为最佳情况，即中心像元能够提供所有信息
          ;2、误差分两种情况讨论，1）中心像元误差为负，这种情况下，要求符合条件的像元的误差比中心像元误差值大
          ;                     2）中心像元误差为正，这种情况下，要求符合条件的像元的误差比中心像元误差值小
          ;                     不能用绝对值衡量是因为可能出现这种情况：绝对值小于误差，但实际偏离是比较大的
          ;3、每种情况里考虑到，如果没有适合的像元，即窗口中所有像元的差值，都比中心像元更加偏离0点，这样就不能为算法提供更好的信息
          ;   这种情况下，将中心像元的值存为最后结果，判断变量judge变为0，不参与之后的运算
          if r_center_LM ne 0 and r_center_MM ne 0 then begin
            if r_center_LM gt 0 then begin
              if min(r_LM) lt r_center_LM then begin
                r_LM_fine[where(r_LM lt r_center_LM)] = 1
                judge = 1
              endif else begin
                result = data3[(w-1)/2,(w-1)/2];中心像元能完全代表
                judge = 0
              endelse
            endif else begin
              if max(r_LM) gt r_center_LM then begin
                r_LM_fine[where(r_LM gt r_center_LM)] = 1
                judge = 1
              endif else begin
                result = data3[(w-1)/2,(w-1)/2]
                judge = 0
              endelse
            endelse
            if r_center_MM gt 0 then begin
              if min(r_MM) lt r_center_MM then begin
                r_MM_fine[where(r_MM lt r_center_MM)] = 1
                judge = 1
              endif else begin
                result = data2[(w-1)/2,(w-1)/2]
                judge = 0
              endelse
            endif else begin
              if max(r_MM) gt r_center_MM then begin
                r_MM_fine[where(r_MM gt r_center_MM)] = 1
                judge = 1
              endif else begin
                result = data2[(w-1)/2,(w-1)/2]
                judge = 0
              endelse
            endelse
          endif else begin
            if r_center_LM eq 0 then begin
              result = data3[(w-1)/2,(w-1)/2]
              judge = 0
            endif else begin
              result = data2[(w-1)/2,(w-1)/2]
              judge = 0
            endelse
          endelse
          ;有效像元
          if judge eq 1 then begin
            similar_pixel_index = where(similar_pixel*r_LM_fine*r_MM_fine eq 1)
            judge = 2
          endif
          ;计算权重
          ;--------------------------------------------------------------------------------------------------------
          if judge eq 2 then begin
            if similar_pixel_index[0] ne -1 then begin
              ;   图像之差
              S_lm = abs(data1[similar_pixel_index] - data2[similar_pixel_index]) > 0.00001
              T_mm = abs(data1[similar_pixel_index] - data3[similar_pixel_index]) > 0.00001
              ;   像素到中心像元的距离 ，并转成相对距离
              sub_index = ARRAY_INDICES([w,w],similar_pixel_index,/dimensions)
              distance = transpose(1+((sub_index[0,*] - (w-1)/2)^2 + (sub_index[1,*] - (w-1)/2)^2)^0.5/A)
              ;          print,'s----------------'
              ;          print,S_lm
              ;          print,'t----------------'
              ;          print,T_mm
              ;          print,'d----------------'
              ;          print,distance
              ;   最后权重
              weight = (1/S_lm*T_mm*distance)/total(1/S_lm*T_mm*distance)
              ;          print,'----------------'
              ;          print,weight
              judge = 3
            endif else begin
              result = 0
              judge = 0
            endelse
          endif
          ;加权求和
          ;--------------------------------------------------------------------------------------------------------
          if judge eq 3 then begin
            result = total(weight*(data3[similar_pixel_index]+data2[similar_pixel_index]-data1[similar_pixel_index]))
            judge = 0
          endif
          predicted_data[sample,line] = result
          ;      result_predicted_data[sample,line] = result
        endif
        ;print,'pixel done~'
      endfor;piexl end
      fine1[*,*,band] = predicted_data
      print,'done!band'
    endfor;band for end


    output_filename = file_name_F2 + v_F1_basena[indx1] + '_pr'
    envi_file_mng,id=fid1,/remove
    envi_file_mng,id=fid2,/remove
    envi_file_mng,id=fid3,/remove
    envi_file_mng,id=fid4,/remove
    envi_file_mng,id=fid5,/remove
    envi_file_mng,id=fid6,/remove
    ENVI_WRITE_ENVI_FILE, fine1, out_name=output_filename, /nocopy, $
      ns=ns,nl=nl,nb=nb, offset=offset, bnames=bnames, map_info=map_info
    print,'写出文件完成！'
  endfor;fiels for end
end
