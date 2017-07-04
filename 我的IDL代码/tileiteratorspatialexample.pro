PRO TileIteratorSpatialExample

  COMPILE_OPT IDL2



  ; Start the application

  e = ENVI()



  ; Select input data

  file = FILEPATH('qb_boulder_pan', ROOT_DIR=e.ROOT_DIR, $

    SUBDIRECTORY = ['data'])



  ; Open the original raster

  OrigRaster = e.OpenRaster(file)



  ; Create a new raster

  newFile = e.GetTemporaryFilename()

  EdgeDogRaster = ENVIRaster(URI=newFile, $

    NROWS=OrigRaster.NROWS, $

    NCOLUMNS=OrigRaster.NCOLUMNS, $

    NBANDS=OrigRaster.NBANDS, $

    DATA_TYPE=OrigRaster.DATA_TYPE)



  ; Iterate through the tiles of the original data

  tileIterator = OrigRaster.CreateTileIterator()



  count = 0

  FOREACH tile, tileIterator DO BEGIN

    count++

    PRINT,''

    PRINT, 'Tile Number:'

    PRINT, count



    ; Process the data

    processedTile = EDGE_DOG(tile)

    currentSubRect = tileIterator.CURRENT_SUBRECT



    EdgeDogRaster.SetData, processedTile, SUB_RECT=currentSubRect

  ENDFOREACH



  ; Finalize data

  EdgeDogRaster.Save



  ; Display new raster

  View = e.GetView()



  Layer = View.CreateLayer(EdgeDogRaster)

END
