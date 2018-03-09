;+
      ; :Description:
      ;    Describe the procedure.
      ; this program aimed to caculate the residual, to analysis the influence of human and climate.
      ; 
      ; input var: ndvi as y, tem as x1, pre as x2
      ; 
      ; output var: a1,a2,const
      ; 
      ; FUNCTION: y = const + a1*x1 + a2*x2
      ;
      ;
      ; :Author: Li Yansen
      ; :Date: 2017/12/6
      ;-
pro Res_analysis

  ;initialization
  e = ENVI()

  ;Get Data
  file_ndvi = DIALOG_PICKFILE(TITLE='请选择输入文件',  FILTER='*.dat')
  file_tem = DIALOG_PICKFILE(TITLE='请选择输入气温文件',  FILTER='*.dat')
  file_pre = DIALOG_PICKFILE(TITLE='请选择输入降水文件',  FILTER='*.dat')

  ;out path
  out_a1 = 'F:\a1.dat'
  out_a2 = 'F:\a2.dat'
  out_const = 'F:\const.dat'

  ;Get Files
  raster_ndvi = e.OpenRaster(file_ndvi)
  raster_tem = e.OpenRaster(file_tem)
  raster_pre = e.OpenRaster(file_pre)

  fid_ndvi = ENVIRasterToFID(raster_ndvi)
  fid_tem = ENVIRasterToFID(raster_tem)
  fid_pre = ENVIRasterToFID(raster_pre)

  ENVI_FILE_QUERY,fid_ndvi ,ns = ns, nl = nl, nb = nb, dims = dims ,INTERLEAVE = interleave,$
    DATA_TYPE = data_type

  tileIterator_ndvi = raster_ndvi.CreateTileIterator(MODE='spectral', BANDS=[0:nb-1])
  tileIterator_tem = raster_tem.CreateTileIterator(MODE='spectral', BANDS=[0:nb-1])
  tileIterator_pre = raster_pre.CreateTileIterator(MODE='spectral', BANDS=[0:nb-1])

  spatialRef = raster_ndvi.SPATIALREF

  a1Raster = ENVIRaster(URI = out_a1, $

    NROWS = raster_ndvi.NROWS, $

    NCOLUMNS = raster_ndvi.NCOLUMNS, $

    NBANDS = 1, $

    DATA_TYPE = 'double', $

    SPATIALREF = spatialRef)

  a2Raster = ENVIRaster(URI = out_a2, $

    NROWS = raster_ndvi.NROWS, $

    NCOLUMNS = raster_ndvi.NCOLUMNS, $

    NBANDS = 1, $

    DATA_TYPE = 'double', $

    SPATIALREF = spatialRef)

  constRaster = ENVIRaster(URI = out_const, $

    NROWS = raster_ndvi.NROWS, $

    NCOLUMNS = raster_ndvi.NCOLUMNS, $

    NBANDS = 1, $

    DATA_TYPE = 'double', $

    SPATIALREF = spatialRef)

  a1_data = fltarr(ns,nl)
  a2_data = a1_data
  const_data = a1_data

  for count = 1, tileIterator_ndvi.NTILES do begin; 列循环

    tile_ndvi = tileIterator_ndvi.Next()
    tile_tem = tileIterator_tem.Next()
    tile_pre = tileIterator_pre.Next()


    PRINT, 'Processing Tile Number:' + string(count)

    ;   elements_num = n_elements(tile_ndvi)/(nb); 计算行数

    ;  data = fltarr(elements_num,nb)
    
    tile_ndvi[where(tile_ndvi) lt 0.02] = 0
    
    ;;//////////////////////////////////////////////////////
    ind = where(tile_tem GT -100, count_ind)

    dims = size(tile_ndvi, /dimensions)

    if count_ind eq 0 then continue

    ;;//////////////////////////////////////////////////////

    t1 = SYSTIME(1)

    FOREACH index, ind DO BEGIN;有效行循环

      ncol = dims[0]

      cols = index mod ncol

      ;     rows = index / ncol

      x1 = tile_tem[cols,*]; x1为降水因子

      x2 = tile_pre[cols,*]; x2为气温因子

      Y = transpose(tile_ndvi[cols,*])

      X = [x1,x2]

      ;初始化高斯误差

      measure_errors = REPLICATE(0.5, N_ELEMENTS(Y))

      ;多元线性回归

      result = REGRESS(X, Y, SIGMA=sigma, CONST=const, $

        MEASURE_ERRORS=measure_errors)




      a1_a2 = result

      a1 = a1_a2[0]
      a2 = a1_a2[1]

      ;output three layers: const, a1(tem), a2(pre)--> const+a1*x1+a2*x2


      currentSubRect = tileIterator_ndvi.CURRENT_SUBRECT;first row number same as last row number

      a1_data[cols,count - 1] = a1
      a2_data[cols,count - 1] = a2
      const_data[cols,count - 1] = const


    ENDFOREACH

  endfor

  a1Raster.SetData, a1_data

  a2Raster.SetData, a2_data

  constRaster.SetData, const_data

  constRaster.save

  a2Raster.save

  a1Raster.save

  t2 = systime(1)

  time = t2 - t1

  print,'time use :' + time

  PRINT, '已知的值： ', Y
end