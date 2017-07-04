pro image_merge_test
  p = 1 ;规定计算的波段数：0-1、1-2、2-3、3-4
  w = 25;窗口大小
  class_num = 4;假定分类数
  A = 4;距离因子
  ;  compile_opt idl2
  ;  envi, /restore_base_save_files
  ;  envi_batch_init, log_file='d:\test\batch.txt'
  file_name_F1 = dialog_pickfile(title = 'fine resolusion map of the 1st pair',path = 'D:\test\image\SD')
  file_name_C1 = dialog_pickfile(title = 'coarse resolusion map of the 1st pair',path = 'D:\test\image\SD')
  file_name_F2 = dialog_pickfile(title = 'fine resolusion map of the 2nd pair',path = 'D:\test\image\SD')
  file_name_C2 = dialog_pickfile(title = 'coarse resolusion map of the 2nd pair',path = 'D:\test\image\SD')
  envi_open_file,file_name_F1,r_fid = fid_0203_30
  envi_open_file,file_name_C1,r_fid = fid_0203_150
;  envi_open_file,file_name_F2,r_fid = fid_0204_30
  envi_open_file,file_name_C2,r_fid = fid_0204_150

  envi_file_query, fid_0203_150, ns=ns, nl=nl, nb=nb , dims = dims
  data_0203_150 = float(ENVI_GET_DATA(fid=fid_0203_150, dims=dims, pos=p))

  envi_file_query, fid_0203_30, ns=ns, nl=nl, nb=nb , dims = dims
  data_0203_30 = float(ENVI_GET_DATA(fid=fid_0203_30, dims=dims, pos=p))

  envi_file_query, fid_0204_150, ns=ns, nl=nl, nb=nb , dims = dims
  data_0204_150 = float(ENVI_GET_DATA(fid=fid_0204_150, dims=dims, pos=p))

;  envi_file_query, fid_0204_30, ns=ns, nl=nl, nb=nb , dims = dims
 ; data_0204_30 = float(ENVI_GET_DATA(fid=fid_0204_30, dims=dims, pos=p))

  predicted_data = fltarr(ns,nl)


  for  sample = 0 , ns- 1 do begin
    for  line = 0 , nl- 1 do begin
      ;取像元
      ;--------------------------------------------------------------------------------------------------------
      ai=max([0,sample-(w-1)/2])
      bi=min([ns-1,sample+(w-1)/2])
      aj=max([0,line-(w-1)/2])
      bj=min([ns-1,line+(w-1)/2])

      data1 = data_0203_150[ai:bi,aj:bj]
      data2 = data_0203_30[ai:bi,aj:bj]
      data3 = data_0204_150[ai:bi,aj:bj]
      result = 0
      judge = 0


      ;方差
      similar_pixel_measure = stddev(data_0203_30)*2.0/class_num;
      similar_pixel = fltarr(w,w)
      if similar_pixel_measure ne 0 then begin;如果不是同质像元
        similar_pixel[where((data2-data2[(w-1)/2,(w-1)/2]) lt similar_pixel_measure)] = 1
      endif else begin;同质像元相关性设置为1
        similar_pixel = 1
      endelse
      ;图像差值
      r_LM_fine = fltarr(w,w)
      r_MM_fine = fltarr(w,w)
      r_LM = abs(data1 - data2)
      r_MM = abs(data1 - data3)
      r_center_LM = abs(data1[(w-1)/2,(w-1)/2] - data2[(w-1)/2,(w-1)/2])
      r_center_MM = abs(data1[(w-1)/2,(w-1)/2] - data3[(w-1)/2,(w-1)/2])
      ;注释：
      ;1、排除窗口中心像元只差为0的情况，该情况视为最佳情况，即中心像元能够提供所有信息
      ;2、误差分两种情况讨论，1）中心像元误差为负，这种情况下，要求符合条件的像元的误差比中心像元误差值大
      ;                     2）中心像元误差为正，这种情况下，要求符合条件的像元的误差比中心像元误差值小
      ;                     不能用绝对值衡量是因为可能出现这种情况：绝对值小于误差，但实际偏离是比较大的
      ;3、每种情况里考虑到，如果没有适合的像元，即窗口中所有像元的差值，都比中心像元更加偏离0点，这样就不能为算法提供更好的信息
      ;   这种情况下，将中心像元的值存为最后结果，判断变量judge变为0，不参与之后的运算
      if r_center_LM ne 0 and r_center_MM ne 0 then begin
        if r_center_LM gt 0 then begin
          if abs(min(r_LM)) lt r_center_LM then begin
            r_LM_fine[where(r_LM lt r_center_LM)] = 1
            judge = 1
          endif else begin
            result = data3[(w-1)/2,(w-1)/2]
            judge = 0
          endelse
        endif else begin
          if abs(max(r_LM)) gt r_center_LM then begin
            r_LM_fine[where(r_LM gt r_center_LM)] = 1
            judge = 1
          endif else begin
            result = data3[(w-1)/2,(w-1)/2]
            judge = 0
          endelse
        endelse
        if r_center_MM gt 0 then begin
          if abs(min(r_MM)) lt r_center_MM then begin
            r_MM_fine[where(r_MM lt r_center_MM)] = 1
            judge = 1
          endif else begin
            result = data2[(w-1)/2,(w-1)/2]
            judge = 0
          endelse
        endif else begin
          if abs(max(r_MM)) gt r_center_MM then begin
            r_MM_fine[where(r_MM gt r_center_MM)] = 1
            judge = 1
          endif else begin
            result = data2[(w-1)/2,(w-1)/2]
            judge = 0
          endelse
        endelse
      endif 
;      else begin
;        if r_center_LM eq 0 then begin
;          result = data3[(w-1)/2,(w-1)/2]
;          judge = 0
;        endif else begin
;          result = data2[(w-1)/2,(w-1)/2]
;          judge = 0
;        endelse
;      endelse
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
          result = data2[(w-1)/2,(w-1)/2]
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
    endfor
  endfor

  print,'done'

  output_filename = file_name_F2 + '_p'
  openw,lun,output_filename,/get_lun
  writeu,lun,predicted_data
  free_lun,lun
  envi_setup_head,fname = output_filename , ns = ns , nl = nl , nb = 1$
    ,data_type = 4 , offset = 0 , interleave = 0 $
    , /write

end