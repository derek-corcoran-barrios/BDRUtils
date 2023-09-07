test_that("Test write_cog function", {

  # Create a sample SpatRaster
  r <- terra::rast(nrows = 5, ncols = 5, vals = 1:25)

  # Specify a test COG filename

  # Test 1: Check if the function successfully creates a COG file
  write_cog(SpatRaster = r, Name = "test.tif")
  expect_true(file.exists("test.tif"))

  # Test 2: Check if the function successfully creates a tfw file
  write_cog(SpatRaster = r, Name = "test.tif")
  expect_true(file.exists("test.tfw"))

  # Cleanup: Delete the test COG file
  file.remove("test.tif")
  file.remove("test.tfw")
})
