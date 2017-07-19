;debug the wrong 6s parameter of etm+ 138 to 137 and make a function about 6s
;2016.7.23 change the dir to mobile storage and add a judge about whether the year data repeat
;change the LT5 to LT5 and L5 
pro my_MTLprocedure
  compile_opt idl2
  ENVI,/restore_base_save_files
  ENVI_BATCH_INIT
  ;open workpath
  work_dir=DIALOG_PICKFILE(title='选择文件所在路径',/directory)
  CD,work_dir

  TXTfiles=FILE_SEARCH(work_dir,'*_mtl.txt',count=fnum2);获取以_MTL.txt结尾的文件，并记录数量
  head_basena=FILE_BASENAME(TXTfiles);获取_MTL.TXT文件名
  ;if there is no gap_fill data
  IF(fnum2 LE 0) THEN BEGIN
    print,'There are no valid MTL.TXT files to be processed, end the program!'
    ENVI_BATCH_EXIT;退出初始化批处理格式
  endif
  a=intarr(fnum2)
  ; data processing
  for i=0,fnum2-1 do begin;总循环输出in文件
    data_year1=strtrim(fix(strmid(head_basena[i],9,4)),2);当前操作数据年份（整型）
    image_files=TXTfiles[i]
    Firs3Chr=strmid(head_basena[i],0,3);前三个字符，用来判断传感器类型
    ;    打开文件以及获取fid
    e=envi()
    raster=e.OpenRaster(image_files)

    NUM=N_ELEMENTS(raster)
    ;select multi bands raster and get fid
    FOR i1=0, NUM -1 DO BEGIN
      fid=ENVIRasterToFID(raster[i1])
      ENVI_FILE_QUERY,fid,ns=ns,nl=nl,nb=nb,dims=dims

      IF nb EQ 6 THEN fid = fid[i1]
      BREAK
    ENDFOR
    
    ;对是否有重复年份的数据进行判断
    data_year=data_year1
    a[i]=data_year
    if i gt 1 then begin
      if a[i] eq a[i-1] then begin
;        data_year = data_year+'2'
data_year = data_year+i
      endif
    endif

    MAP_INFO=ENVI_GET_MAP_INFO(fid=fid)
    print,'image numbs'+string(i+1)
    in_files=TXTfiles[i]; 6S correct paraments
    MY_6S_CORRECT,in_files,fid,i,data_year,MAP_INFO,ns,nl,nb,dims,Firs3Chr,head_basena
  endfor
  print,'运算完成！程序已终止！'
  ENVI_BATCH_EXIT;退出初始化批处理格式
end


; 6S ATO CORRECT
pro my_6S_CORRECT,in_files,fid,i,data_year,MAP_INFO,ns,nl,nb,dims,Firs3Chr,head_basena
  RADIANCE_MULT_BAND_1_value=''
  RADIANCE_MULT_BAND_2_value=''
  RADIANCE_MULT_BAND_3_value=''
  RADIANCE_MULT_BAND_4_value=''
  RADIANCE_MULT_BAND_5_value=''
  RADIANCE_MULT_BAND_6_value=''
  RADIANCE_MULT_BAND_7_value=''
  RADIANCE_ADD_BAND_1_value=''
  RADIANCE_ADD_BAND_2_value=''
  RADIANCE_ADD_BAND_3_value=''
  RADIANCE_ADD_BAND_4_value=''
  RADIANCE_ADD_BAND_5_value=''
  RADIANCE_ADD_BAND_6_value=''
  RADIANCE_ADD_BAND_7_value=''
  CORNER_UL_LAT_PRODUCT_value=''
  CORNER_UL_LON_PRODUCT_value=''
  CORNER_UR_LAT_PRODUCT_value=''
  CORNER_UR_LON_PRODUCT_value=''
  CORNER_LL_LAT_PRODUCT_value=''
  CORNER_LL_LON_PRODUCT_value=''
  CORNER_LR_LAT_PRODUCT_value=''
  CORNER_LR_LON_PRODUCT_value=''
  DATE_ACQUIRED_value=''
  DataGetArr=''
  DataGetMonth=''
  DataGetDay=''
  GMT=''
  Center_LAT=''
  Center_LON=''
  season=''
  iband=''

  ;打开文件夹
  asciifile=in_files
  line_counts=FILE_LINES(asciifile);获取文件每行的内容

  ;counts=line_counts[0]+1-1

  openr,lun,asciifile,/get_lun;打开文件
  ;for j=0,120 do readf,lun,void
  arr=strarr(line_counts[0]);获取每一行记录

  ;获取头文件信息

  readf,lun,arr

  for j=0,line_counts[0]-1 do begin;循环in.txt行列

    ;***************获取带有指定字符串的内容，构建in文件***************************************
    arrsuntmp = strsplit(arr[j],'=',/extract);记录中的=变为空格
    DATE_ACQUIRED_bool = strmatch(strtrim(arrsuntmp[0],2),'DATE_ACQUIRED',/FOLD_CASE);STRTRIM把输入字符串的首尾去除空格，flag=2；
    ;判断记录当中是否包含'DATE_ACQUIRED'字符串，bool类型
    if max(DATE_ACQUIRED_bool) eq 1 then begin;判断是否有选择到内容
      DATE_ACQUIRED_VALUE = arrsuntmp[1];获取记录
      DataGetArr=strsplit(DATE_ACQUIRED_value,'-',/extract);把-变成空格
      DataGetMonth=DataGetArr[1]
      DataGetDay=DataGetArr[2]
      Data_Month=fix(DataGetMonth)
      Data_Day=fix(DataGetDay)
    endif

    arrsuntmp = strsplit(arr[j],'=',/extract);记录中的=变为空格
    SCENE_CENTER_TIME_bool = strmatch(strtrim(arrsuntmp[0],2),'SCENE_CENTER_TIME',/FOLD_CASE)
    if max(SCENE_CENTER_TIME_bool) eq 1 then begin
      SCENE_CENTER_TIME_value = arrsuntmp[1]
      DataGetTimeArr=strsplit(SCENE_CENTER_TIME_value,':',/extract)

      GMT=double(strmid(strtrim(DataGetTimeArr[0],2),1,2))+double(strtrim(DataGetTimeArr[1],2))/60+double(strmid(DataGetTimeArr[2],0,8))/3600

      ;        minit=double(DataGetTimeArr[1])/60
      ;        sec=double(strmid(DataGetTimeArr[2],0,8))/3600

      ;        print,hour,minit,sec
    endif
    CORNER_UL_LAT_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_UL_LAT_PRODUCT',/FOLD_CASE)
    if max(CORNER_UL_LAT_PRODUCT) eq 1 then begin
      CORNER_UL_LAT_PRODUCT_value = arrsuntmp[1]
    endif
    CORNER_UL_LON_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_UL_LON_PRODUCT',/FOLD_CASE)
    if max(CORNER_UL_LON_PRODUCT) eq 1 then begin
      CORNER_UL_LON_PRODUCT_value = arrsuntmp[1]
    endif
    CORNER_UR_LAT_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_UR_LAT_PRODUCT',/FOLD_CASE)
    if max(CORNER_UR_LAT_PRODUCT) eq 1 then begin
      CORNER_UR_LAT_PRODUCT_value = arrsuntmp[1]
    endif
    CORNER_UR_LON_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_UR_LON_PRODUCT',/FOLD_CASE)
    if max(CORNER_UR_LON_PRODUCT) eq 1 then begin
      CORNER_UR_LON_PRODUCT_value = arrsuntmp[1]
    endif
    CORNER_LL_LAT_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_LL_LAT_PRODUCT',/FOLD_CASE)
    if max(CORNER_LL_LAT_PRODUCT) eq 1 then begin
      CORNER_LL_LAT_PRODUCT_value = arrsuntmp[1]
    endif
    CORNER_LL_LON_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_LL_LON_PRODUCT',/FOLD_CASE)
    if max(CORNER_LL_LON_PRODUCT) eq 1 then begin
      CORNER_LL_LON_PRODUCT_value = arrsuntmp[1]
    endif
    CORNER_LR_LAT_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_LR_LAT_PRODUCT',/FOLD_CASE)
    if max(CORNER_LR_LAT_PRODUCT) eq 1 then begin
      CORNER_LR_LAT_PRODUCT_value = arrsuntmp[1]
    endif
    CORNER_LR_LON_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_LR_LON_PRODUCT',/FOLD_CASE)
    if max(CORNER_LR_LON_PRODUCT) eq 1 then begin
      CORNER_LR_LON_PRODUCT_value = arrsuntmp[1]
    endif

    ;;;;;;;
    RADIANCE_MULT_BAND_1 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_1',/FOLD_CASE)
    if max(RADIANCE_MULT_BAND_1) eq 1 then begin
      RADIANCE_MULT_BAND_1_value = arrsuntmp[1]

    endif
    RADIANCE_MULT_BAND_2 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_2',/FOLD_CASE)
    if max(RADIANCE_MULT_BAND_2) eq 1 then begin
      RADIANCE_MULT_BAND_2_value = arrsuntmp[1]

    endif
    RADIANCE_MULT_BAND_3 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_3',/FOLD_CASE)
    if max(RADIANCE_MULT_BAND_3) eq 1 then begin
      RADIANCE_MULT_BAND_3_value = arrsuntmp[1]

    endif
    RADIANCE_MULT_BAND_4 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_4',/FOLD_CASE)
    if max(RADIANCE_MULT_BAND_4) eq 1 then begin
      RADIANCE_MULT_BAND_4_value = arrsuntmp[1]

    endif
    RADIANCE_MULT_BAND_5 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_5',/FOLD_CASE)
    if max(RADIANCE_MULT_BAND_5) eq 1 then begin
      RADIANCE_MULT_BAND_5_value = arrsuntmp[1]

    endif
    RADIANCE_MULT_BAND_6 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_6',/FOLD_CASE)
    if max(RADIANCE_MULT_BAND_6) eq 1 then begin
      RADIANCE_MULT_BAND_6_value = arrsuntmp[1]
    endif
    RADIANCE_MULT_BAND_7 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_7',/FOLD_CASE)
    if max(RADIANCE_MULT_BAND_7) eq 1 then begin
      RADIANCE_MULT_BAND_7_value = arrsuntmp[1]

    endif
    RADIANCE_ADD_BAND_1 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_1',/FOLD_CASE)
    if max(RADIANCE_ADD_BAND_1) eq 1 then begin
      RADIANCE_ADD_BAND_1_value = arrsuntmp[1]
    endif
    RADIANCE_ADD_BAND_2 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_2',/FOLD_CASE)
    if max(RADIANCE_ADD_BAND_2) eq 1 then begin
      RADIANCE_ADD_BAND_2_value = arrsuntmp[1]
    endif
    RADIANCE_ADD_BAND_3 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_3',/FOLD_CASE)
    if max(RADIANCE_ADD_BAND_3) eq 1 then begin
      RADIANCE_ADD_BAND_3_value = arrsuntmp[1]
    endif
    RADIANCE_ADD_BAND_4 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_4',/FOLD_CASE)
    if max(RADIANCE_ADD_BAND_4) eq 1 then begin
      RADIANCE_ADD_BAND_4_value = arrsuntmp[1]
    endif
    RADIANCE_ADD_BAND_5 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_5',/FOLD_CASE)
    if max(RADIANCE_ADD_BAND_5) eq 1 then begin
      RADIANCE_ADD_BAND_5_value = arrsuntmp[1]
    endif
    RADIANCE_ADD_BAND_6 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_6',/FOLD_CASE)
    if max(RADIANCE_ADD_BAND_6) eq 1 then begin
      RADIANCE_ADD_BAND_6_value = arrsuntmp[1]
    endif
    RADIANCE_ADD_BAND_7 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_7',/FOLD_CASE)
    if max(RADIANCE_ADD_BAND_7) eq 1 then begin
      RADIANCE_ADD_BAND_7_value = arrsuntmp[1]
    endif
  endfor
  ;获取头文件信息over

  free_lun,lun;关闭文件
  free_lun,lun;关闭文件
  ;str[WHERE(STRMATCH(arr[j], 'RADIANCE_MULT_BAND_1', /FOLD_CASE) EQ 1)]
  ;if(strmatch(arrsuntmp,'RADIANCE_MULT_BAND_1')!=0) then

  L=FLTARR(ns,nl,nb)
  REF=FLTARR(ns,nl,nb)
  for T=0,nb-1 do begin;循环波段进行辐射定标
    print,'bands'+string(T+1)

    if Firs3Chr eq 'LT5' or Firs3Chr eq 'L5' then begin
      iband = 25+T
    endif else begin
      iband = 137+T
    endelse
    print,'iband is '+string(iband)
    PRE_STR=string(iband)
    str_band=STRTRIM(PRE_STR,2)
    Center_LAT=(double(CORNER_UL_LAT_PRODUCT_value)+double(CORNER_UR_LAT_PRODUCT_value)+double(CORNER_LL_LAT_PRODUCT_value)+double(CORNER_LR_LAT_PRODUCT_value))/4
    Center_LON=(double(CORNER_UL_LON_PRODUCT_value)+double(CORNER_UR_LON_PRODUCT_value)+double(CORNER_LL_LON_PRODUCT_value)+double(CORNER_LR_LON_PRODUCT_value))/4


    tmg=FLTARR(6,1)
    tmb=FLTARR(6,1)
    
    ;RADIANCE_MULT_BAND_x  * dn + RADIANCE_ADD_BAND_x
    tmg[0,0] = strtrim(RADIANCE_MULT_BAND_1_value,2);mult增益
    tmg[1,0] = strtrim(RADIANCE_MULT_BAND_2_value,2)
    tmg[2,0] = strtrim(RADIANCE_MULT_BAND_3_value,2)
    tmg[3,0] = strtrim(RADIANCE_MULT_BAND_4_value,2)
    tmg[4,0] = strtrim(RADIANCE_MULT_BAND_5_value,2)
    tmg[5,0] = strtrim(RADIANCE_MULT_BAND_7_value,2)

    tmb[0,0] = strtrim(RADIANCE_ADD_BAND_1_value,2);add偏置
    tmb[1,0] = strtrim(RADIANCE_ADD_BAND_2_value,2)
    tmb[2,0] = strtrim(RADIANCE_ADD_BAND_3_value,2)
    tmb[3,0] = strtrim(RADIANCE_ADD_BAND_4_value,2)
    tmb[4,0] = strtrim(RADIANCE_ADD_BAND_5_value,2)
    tmb[5,0] = strtrim(RADIANCE_ADD_BAND_7_value,2)


    DATA_BAND=envi_get_data(fid=fid,dims=dims,pos=T)
    gain=float(tmg[T,0])
    bias=float(tmb[T,0])
    L[*,*,T]=gain*DATA_BAND+bias  ;L为定标后数据

    ;通过对时间的判断，对6S模型的参数进行设置

    user = '7';User-defined geometric conditions
    season = '4'; polar Summer'
    Aerosol_Modle='1'; Continental Model'
    Atmospheric_condi='35'; visibility or aot'
    Target_Altitude='-0.108'; (target level,negative value)'
    Sensor_Altitude='-750'; (sensor level)'
    Ground_Reflectance='0'; Homogeneous surface'
    Directional_effects='0'; No directional effects'
    Specify_surface_reflectance='2'; (mean spectral value)'
    Atm_correction_mode='0'; Atm. correction Lambertian'
    Reflectance='-0.3'; reflectance (negative value)'
    ; free_lun,lun


    ;  O_fn = strmid(fbasena[i],9,4)+'radic.tif';输出文件名

    fn_in=data_year+'in.txt';输入参数文件名
    fn_out_6s='outband'+data_year+'.txt';输出6S校正参数文件名

    print,'当前操作的数据年份'+data_year
    openw,lun,fn_in,/get_lun
    printf,lun,user
    printf,lun,strtrim(DataGetMonth,1),' ',strtrim(DataGetDay,1),' ',strtrim(GMT,1),' ',strtrim(Center_LON,1),' ',strtrim(Center_LAT,1)
    printf,lun,season;
    printf,lun,Aerosol_Modle;
    printf,lun,Atmospheric_condi;
    printf,lun,Target_Altitude;
    printf,lun,Sensor_Altitude;
    printf,lun,str_band
    printf,lun,Ground_Reflectance
    printf,lun,Directional_effects
    printf,lun,Specify_surface_reflectance
    printf,lun,Atm_correction_mode
    printf,lun,Reflectance
    free_lun,lun
    ;写入6S模型“in.txt”的校正参数




    ;输出辐射定标后的文件

    ;  file_type=ENVI_FILE_TYPE('TIFF and GeoTIFF')
    ;  ENVI_WRITE_ENVI_FILE, L ,out_name=O_fn , OUT_DT=4, ns=ns , nl=nl , nb=nb  , interleave=interleave , offset=offset, $
    ;  bnames=bnames , map_info=map_info
    ;*******************************************************************************
    ;***************************6S大气校正************************************************
    ;********************************************************************************
    ;调用6S模型进行计算
    str_line=''
    spawn, '6S.exe<'+fn_in+'>'+fn_out_6S,/hide
    ;读取大气校正参数nb
    openr, lun, fn_out_6S, /get_lun
    lines=file_lines(fn_in)
    print,'fn_in line numbers is '+string(lines)
    out_lines=file_lines(fn_out_6S)
    print,'out_line numbers is '+string(out_lines)
    flag=0
    while flag eq 0 do begin
      readf,lun,str_line
      if strpos(str_line,'coefficients xa xb xc') ne -1 then flag=1
    endwhile

    str_split=strsplit(str_line,' :', /extract);分割字符串

    ;提取校正系数
    xa=float(str_split[5])
    xb=float(str_split[6])
    xc=float(str_split[7])

    ;进行大气校正
    y=xa*L[*,*,T]-xb
    REF[*,*,T]=y/(1.0+xc*y)
    print, 'max is'+string(max(REF[*,*,T]))
    print,'min is'+string(min(REF[*,*,T]))
    ;删除前面生成的6S模型输入和输出文件
    ;file_delete, fn_in_6s, fn_out_6s, /quiet
    free_lun, lun
  endfor ;波段循环结束
  ;保存地表反射率
  ;o_fn=dialog_pickfile(title='地反射率数据保存为')

  REF_OUT='b:\'+strmid(head_basena[i],3,6)+data_year

  ENVI_WRITE_ENVI_FILE, REF, out_name=REF_OUT, /nocopy, $
    ns=ns,nl=nl,nb=nb, offset=offset, bnames=bnames, map_info=map_info
  print,'写出文件完成！'
  ;    ;write_image,REF_OUT,'TIFF',REF
  ;    ;REF.Export,REF_OUT,'tiff','bsq'
  ;    ;REF.Save

end
