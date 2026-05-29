test_that("Tests excesstopography", {
  dem <- terra::rast(system.file("ex/elev.tif", package = "terra"))
  dem <- terra::project(dem, "EPSG:32632", res = 90.0)
  expect_silent(dem_ext <- excesstopography(GRIDobj(dem), tan(20 * pi / 180)))
  expect_true(inherits(dem_ext, "GRIDobj"))
  expect_true(all(terra::values(dem_ext$raster) <= terra::values(dem), na.rm=TRUE))

  ts <- dem
  terra::values(ts) <- runif(terra::size(ts))
  expect_silent(dem_ext <- excesstopography(GRIDobj(dem), GRIDobj(ts)))
  expect_true(all(terra::values(dem_ext$raster) <= terra::values(dem), na.rm=TRUE))
})
