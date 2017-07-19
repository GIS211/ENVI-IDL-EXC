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

pro stars_new
  compile_opt idl2
  ENVI,/restore_base_save_files
  ENVI_BATCH_INIT

  ;open workpath
  t0=systime(1)

  ;********************************************
  ;===========================step one=========================================
  ;set paraments
  ;prepare l,standar dev,free_degree
  ;get T
  time=20
  l=2;step-cut length
  free_degree = 2*l-2
  n_RSI = fltarr(time)
  n_px = 19
  txtname='B:\NDVI_STACK_TEMP\in.txt'
  all_FileName = Dialog_PickFile(title = 'find the temp image files:',/directory)
  CD,all_FileName
  FileName=FILE_SEARCH(all_FileName,'*.hdr',count=fnum)
  F1_basena = FILE_BASENAME(FileName,'.hdr')

  ;********************************************
  T=T_CVF(0.025,free_degree);two tailed test
  x_star=fltarr(time)
  ;get files matrix

  for tt = 0,19 do begin
    FILE_MKDIR, 'B:\stars分块\'+STRTRIM(string(tt+1),2);分块建文件夹
    FILE_MKDIR, 'B:\stars分块\'+STRTRIM(string(tt+1),2) + '\RSI_geotif\' ;分块建文件夹
    FILE_MKDIR, 'B:\stars分块\'+STRTRIM(string(tt+1),2) + '\meanr_geotif\'
    FILE_MKDIR, 'B:\stars分块\'+STRTRIM(string(tt+1),2) + '\year_geotif\'
  endfor
  
  ;get data
  ;筛选有数据的部分
  temp=''

  openr,lun,txtname,/get_lun
  n_line = file_lines(txtname)
  xa = strarr(n_line)
  iq=0
  while ~eof(lun) do begin
    readf,lun,temp
    xa[iq] = temp
    iq=iq+1
  endwhile
  free_Lun,lun
  k=0
  v_F1_basena = strarr(fnum)
  v_F1_basena2 = strarr(fnum)
  for t = 0 , n_line-1 do begin;循环有效文件
    for iq = 0 , fnum-1 do begin;循环所有文件以求所有有效文件名
      if strcmp(strmid(F1_basena[iq],2,4,/REVERSE_OFFSET),xa[t]) then begin
        v_F1_basena[k] = FileName[iq]
        v_F1_basena2[k] = F1_basena[iq]
        k=k+1
      endif
    endfor
  endfor

  for file_num = 0, k - 1 do begin;循环有效文件
    GetData,ImgData=fine,ns = ns,nl = nl,nb = nb,Data_Type = Data_Type, FileName = v_F1_basena[file_num],Fid = Fid,Map_info = map_Info;插值目标
    fine=float(fine)

    RSI_result=fltarr(ns,nl,20)
    meanr=fltarr(ns,nl,20)
    year_index=fltarr(ns,nl,20)

    RSI=fltarr(1,1,20);稳态阶段的均值存储矩阵

    ;=================================STEP FOUR=======================
    ;JUDGE WHETHER regime shift
    ;获取所有不满足要求的变量位置，并进行从新运算

    for n_col = 0, ns-1 do begin;列循环
      for n_row = 0, nl-1 do begin;行循环
        ;      for i=0, n_px do begin
        if strtrim(STRING(fine[n_col,n_row,0]),2) eq 'NaN' then continue
    p=0
        ;       print,'c_col:'+STRTRIM(string(n_col))+'n_row:'+STRTRIM(string(n_row))
        x = fine[n_col,n_row,0:n_px];x是该像元20年的变化
        STD=STDDEV(x[0,0,0:l-1]);该像元起始阶段的标准差
        DIFF=T*sqrt(2*(STD^2)/l);像元的normal DIFF初始

        ;稳态转移判断：用均值和步长判断第一个稳态均值，如果发生转移，从转移发生的点进行下一次计算
        ;稳态判断：x0~x(l-1)的均值加、减DIFF，如果x(l)超过DIFF的范围，则视作潜在的稳态转移。
        ;超过X_mean+diff、X_mean—diff，视其为新稳态判定的参数XR2
        s=0;起始年份
        i=0
        xr=fltarr(20);xr
        n_year=0
        label1:
        n_year=s+l+i-1;n_year是稳态探测的终止年份，s是新稳态的起始年份;n_year也是稳态变化的年份

        if n_year ge 18 or s ge 18 then continue;判断稳态转移点是否超过总范围，17正好可以满足最后一次验证到18

        ;上一稳态,找到变化点
        xr[s] = mean(x[0,0,s:n_year],/nan);第s~n_year年时的均值,转变发生在第n_year+1年,s为起始
        x1 = xr[s]+DIFF;稳态判断条件
        x2 = xr[s]-DIFF
        ;      num=l+i;shift years point num是稳态转移步长
        ; print,'n_year = '+ string(n_year)
        if x[0,0,n_year+1] gt x1 then begin;successed in i+l(n_year+1) up
          xr2=x1;xr'
          index=1
          ; print,'successed in gt x1'
        endif else if x[0,0,n_year+1] lt x2 then begin;successed in i+l(n_year+1) down
          xr2=x2
          index=2
          ; print,'successed in lt x2'
        endif else begin ;shift failed
          i=i+1;后移一位值
          goto,label1;go to recaculate mean
        endelse
        ;rsi
        st=0
        for k_ind=0,l-1 do begin;计算步长范围内的RSI
          if index eq 2 then begin ;down
            x_star[k_ind]=xr2-x[0,0,n_year+1+k_ind]
           ; print,'X - XR2 lt 0 '
          endif else begin;up
            x_star[k_ind]=x[0,0,n_year+1+k_ind]-xr2
           ; print,'X - XR2 gt 0'
          endelse
          n_RSI[k_ind]=x_star[k_ind]/l/std
          RSI[0,0,k_ind]=total(N_RSI[0:k_ind])
          if RSI[0,0,k_ind] lt 0 then begin
            ;print,'failed in total RSI less than 0'
            st=1
            break;跳出本层for循环
          endif
        endfor

        if st eq 1 then begin ;failed
          i=i+1
          ;  print,'failed in regime shift RSI lt 0'
          goto,label1
        endif else begin;successed 真的成功，需要改变初始值，清空步长
          meanr[n_col,n_row,p]=xr[s];成功转移以后，将同一稳态时间段内的均值保存到图像中，S为发生变化的那年
          RSI_result[n_col,n_row,p]=RSI[0,0,k_ind];将稳态转移指数存在稳态转移那一年的图像当中
          year_index[n_col,n_row,p]=1995+n_year
          p=p+1
          ;print,'successed in regime shift in RSI'
          i=0
          s=n_year+1;pass mean+-diff test，从新计算从转变年开始，s=year,改变初始值
          goto,label1;开启下一轮循环

        endelse
      endfor
    endfor



    for tt=0,19 do begin
      out_name='B:\stars分块\'+STRTRIM(string(tt+1),2) + '\RSI_geotif\'+v_F1_basena2[file_num]
      ENVI_WRITE_ENVI_FILE, RSI_result[*,*,tt], out_name=out_name, /nocopy, $
        ns=ns,nl=nl,nb=1, offset=offset, bnames=bnames, map_info=map_info
    endfor

    for tt=0,19 do begin
      out_name='B:\stars分块\'+STRTRIM(string(tt+1),2) + '\meanr_geotif\'+v_F1_basena2[file_num]
      ENVI_WRITE_ENVI_FILE, meanr[*,*,tt], out_name=out_name, /nocopy, $
        ns=ns,nl=nl,nb=1, offset=offset, bnames=bnames, map_info=map_info
    endfor

    for tt=0,19 do begin
      out_name='B:\stars分块\'+STRTRIM(string(tt+1),2) + '\year_geotif\'+v_F1_basena2[file_num]
      ENVI_WRITE_ENVI_FILE, year_index[*,*,tt], out_name=out_name, /nocopy, $
        ns=ns,nl=nl,nb=1, offset=offset, bnames=bnames, map_info=map_info
    endfor
    print, 'time used:', floor((systime(1)-t0)/3600), 'h',floor(((systime(1)-t0) mod 3600)/60),'m',(systime(1)-t0) mod 60,'s'

  endfor;文件循环结束

  ENVI_BATCH_EXIT;退出初始化批处理格式
  print, 'time used:', floor((systime(1)-t0)/3600), 'h',floor(((systime(1)-t0) mod 3600)/60),'m',(systime(1)-t0) mod 60,'s'
  print,'all end!'
end