pro tm_times1W
  compile_opt idl2

  envi,/restore_base_save_files
  envi_batch_init,log_file='batch.txt'

  work_dir=DIALOG_PICKFILE(title='选择文件所在路径',/directory)
  CD,work_dir

  file_lists=FILE_SEARCH(work_dir,'*.hdr',count=fnum)
  fbasena=FILE_BASENAME(file_lists,'.hdr')
  ;  numb = strmid(basena[i], 3, 4, /REVERSE_OFFSET)

  e = envi()

  for i=0, fnum-1 do begin
    print,'number: ' + string(i+1)
    raster =  e.openraster(file_lists[i])
    fid = ENVIRasterToFID(raster)
    ENVI_FILE_QUERY,fid,ns=ns,nl=nl,nb=nb,dims=dims ,INTERLEAVE = interleave1,$
      DATA_TYPE = data_type
    MAP_INFO=ENVI_GET_MAP_INFO(fid=fid)
    L = fltarr(ns,nl,2)
    A=fltarr(ns,nl)
    A=A+1
    for j=0,nb-1 do begin
      L[*,*,j] = Envi_Get_Data(Fid = fid, dims = dims, pos = j)
    endfor
    qt=where(L lt 0 or ~finite(L))
    L[qt]=0.0
    ;    qt=where(~(L[*,*,0] * L[*,*,1]) )
    data=fltarr(ns,nl)
    
    data=L[*,*,0]
    p1 = where(data eq 0.0)
    a[p1] = 0
    data = a * data
    L[*,*,0] = data
    
    data = L[*,*,1]
    P2 = where(data eq 0.0)
    a[P2] = 0
    data = a * data
    L[*,*,1] = data
;    
;    L[*,*,0] = a[P2,1] * L[*,*,0]
;    L[*,*,1] = a[P1,0] * L[*,*,1]
    ;    dims1=size(L)
    ;    mcolm=dims1[1]
    ;        nrow=dims1[2]
    ;    colm = p1 mod mcolm
    ;    row = (p1 / mcolm) mod nrow
    ;    f=p1 / (nrow*colm)
    ;    L[colm,row,f] = 0

    ;    L[colm,row] = 0
    ;    if ~(L[*,*,0] * L[*,*,1]) then begin
    ;      L[qt]=0
    ;    endif


    ;    L[*,*,0] = L[*,*,0]*float(L[*,*,0] ne 0)/(L[*,*,0] ne 0);把背景值设置成-nan
    ;    L[*,*,1] = L[*,*,1]*float(L[*,*,1] ne 0)/(L[*,*,1] ne 0);把背景值设置成-nan
    ENVI_FILE_MNG,id = fid, /remove
    outputname = 'B:\result\2014\'+fbasena[i]
    ;    ENVI_SETUP_HEAD,r_fid=fid, fname=outputname, INTERLEAVE = interleave1,$
    ;      ns=ns,nl=nl,nb=2 , DATA_IGNORE_VALUE=min(L), data_type = data_type ,MAP_INFO=MAP_INFO
    ENVI_WRITE_ENVI_FILE, L, out_name=outputname, /nocopy, $
      ns=ns,nl=nl,nb=2, map_info=map_info

  endfor
  print,'all done!'
  ;  ENVI_BATCH_EXIT
end