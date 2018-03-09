;+
  ; :Description:
  ;    Describe the procedure.
  ;  part 2 :
  ;    
  ;    after res_analysis.pro
  ;    
  ;  forecast ndvi by function: y = const + a1*x1 + a2*x2
  ;
  ;
  ;
  ; :Author: Li Yansen
  ;-
pro caculate_Res
  ;initialization
  e = ENVI()

  ;Get Data
  file_a1 = DIALOG_PICKFILE(TITLE='请输入a1',  FILTER='*.dat')
  file_a2 = DIALOG_PICKFILE(TITLE='请输入a2',  FILTER='*.dat')
  file_const = DIALOG_PICKFILE(TITLE='请输入const',  FILTER='*.dat')

  file_tem = DIALOG_PICKFILE(TITLE='请输入tem',  FILTER='*.dat')
  file_pre = DIALOG_PICKFILE(TITLE='请输入pre',  FILTER='*.dat')

  ;out put path
  out_dir = 'F:\PreNDVI.dat'

  ;Get Files
  raster_const = e.OpenRaster(file_const)
  raster_a1 = e.OpenRaster(file_a1)
  raster_a2 = e.OpenRaster(file_a2)

  raster_tem = e.OpenRaster(file_tem)
  raster_pre = e.OpenRaster(file_pre)

  fid_a1 = ENVIRasterToFID(raster_a1)
  fid_a2 = ENVIRasterToFID(raster_a2)
  fid_const = ENVIRasterToFID(raster_const)

  fid_tem = ENVIRasterToFID(raster_tem)
  fid_pre = ENVIRasterToFID(raster_pre)

  ENVI_FILE_QUERY,fid_tem ,ns = ns, nl = nl, nb = nb, dims = dims ,INTERLEAVE = interleave,$
    DATA_TYPE = data_type

  tileIterator_tem = raster_tem.CreateTileIterator(MODE='spectral', BANDS=[0:nb-1])
  tileIterator_pre = raster_pre.CreateTileIterator(MODE='spectral', BANDS=[0:nb-1])


  spatialRef = raster_tem.SPATIALREF

  ResultRaster = ENVIRaster(URI = out_dir, $

    NROWS = raster_tem.NROWS, $

    NCOLUMNS = raster_tem.NCOLUMNS, $

    NBANDS = nb, $

    DATA_TYPE = 'float', $

    SPATIALREF = spatialRef)

  const = raster_const.getdata()
  a1 = raster_a1.getdata()
  a2 = raster_a2.getdata()
  result  = fltarr(ns,nl,nb)

  FOR count = 1, tileIterator_tem.NTILES do begin; 列循环

    tile_tem = tileIterator_tem.Next()
    tile_pre = tileIterator_pre.Next()

    PRINT, 'Processing Tile Number:' + string(count)

    t1 = SYSTIME(1)

    FOR index = 0, nb - 1 DO BEGIN;有效行循环

      result[*,count-1,index] = const[*,count-1] + tile_tem[*,index]*a1[count-1] + tile_pre[*,index]*a2[*,count-1]

      currentSubRect = tileIterator_tem.CURRENT_SUBRECT;first row number same as last row number

    ENDFOR

  ENDFOR

  ResultRaster.SetData, result

  ResultRaster.save
  ;  ResultRaster.export,'F:\constRaster.tif','TIFF' ;文件转存
  print,'finished'


end