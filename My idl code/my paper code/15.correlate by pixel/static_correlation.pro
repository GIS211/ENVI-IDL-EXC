pro static_correlation
  compile_opt idl2
  ENVI,/restore_base_save_files
  ENVI_BATCH_INIT
  ;open workpath
  NDVI_dir = DIALOG_PICKFILE(title='选择文件所在路径',/directory)
  CD,NDVI_dir
  NDVI_files = FILE_SEARCH(NDVI_dir,'*.hdr', count=fnum)

  TP_dir = DIALOG_PICKFILE(title='选择文件所在路径',/directory)
  CD,TP_dir
  TP_files = FILE_SEARCH(TP_dir,'*.tif', count=fnum2)

  ndvi_basena = FILE_BASENAME(NDVI_files);获取文件名
  tp_basena = FILE_BASENAME(TP_files);获取文件名

  IF(fnum LE 0 or fnum2 LE 0) THEN BEGIN
    print,'There are no valid FILES to be processed, end the program!'
    ENVI_BATCH_EXIT;退出初始化批处理格式
  endif

  for i=0,fnum-1 do begin;总循环输出in文件
    NDVI_name = ndvi_basena[i];当前操作数据年份（整型）

    for j = 0, fnum2-1 do begin
      label_1:
      if j eq 6 then break
      TP_name = tp_basena[j];当前操作数据年份（整型）

      if strmid(TP_name, 0, 4) eq strmid(NDVI_name, 0, 4) then begin

        e = envi()
        NDVI = e.OpenRaster(NDVI_files[i])
        TP = e.OpenRaster(TP_files[j])

        ;      NDVI_NUM=N_ELEMENTS(NDVI)
        ;     TP_NUM=N_ELEMENTS(TP)
        ;      FOR i1=0, NDVI_NUM -1 DO BEGIN

        NDVI_fid = ENVIRasterToFID(NDVI)
        TP_fid = ENVIRasterToFID(TP)
        ENVI_FILE_QUERY,NDVI_fid, ns=ns1, nl=nl1, nb=nb1, dims=dims1,data_type=data_type1
        ENVI_FILE_QUERY,TP_fid, ns=ns2, nl=nl2, nb=nb2, dims=dims2,data_type=data_type2

        MAP_INFO=ENVI_GET_MAP_INFO(fid=NDVI_fid)

;        data_arr1 = make_array(ns1,nl1,nb1,type = data_type1);BIP
;        data_arr1 = transpose(data_arr1,[1,2,0])
;        data_arr2 = make_array(ns2,nl2,nb2,type = data_type2)
        data_arr3 = make_array(ns2,nl2)

        data_arr1 = NDVI.getdata()
        data_arr1 = transpose(data_arr1,[1,2,0])
        data_arr2 = TP.getdata()

        data_arr1[where(data_arr1 eq min(data_arr1))]= 'nan'
        data_arr2[where(data_arr2 eq min(data_arr2))]= 'nan'
        for x = 0 , ns1-1 do begin

          for y = 0 , nl1-1 do begin
            data_arr3[x,y] = CORRELATE(data_arr1[x,y,0:19],data_arr2[x,y,0:19])
          endfor

        endfor
        REF_OUT='B:\'+strmid(tp_basena[j],0,8)

        ENVI_WRITE_ENVI_FILE, data_arr3, out_name=REF_OUT, /nocopy, $
          ns=ns,nl=nl,nb=nb, offset=offset, bnames=bnames, map_info=map_info
        print,'写出文件完成！'
      endif else begin
        j = j+1
        goto,label_1
      endelse

    endfor
  endfor

end
