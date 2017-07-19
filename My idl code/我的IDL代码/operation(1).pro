pro operation

  ; Launch the application and open a file
  e = ENVI()

  file = DIALOG_PICKFILE(PATH='examples\data', $
    TITLE='Select DICOM Patient File',  FILTER='*.dcm')


  raster = e.OpenRaster(file)



  ; Get the first band with a subset, refactoring the

  ; size in the x and y direction

  data = raster.GetData(BANDS=[0], SUB_RECT=[100,449,550,899])



  ; Create a new raster of the subsetted data

  newfile = e.GetTemporaryFilename()

  new_raster = ENVIRaster(data, URI=newfile)

  new_raster.Save



  ; Display the subset

  view=e.GetView()

  layer=view.CreateLayer(new_raster)
end
