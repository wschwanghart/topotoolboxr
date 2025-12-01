test_that("Checks if data path loads correctly", {
  srtm_bigtujunga30m_utm11 <- system.file("extdata",
                                          "srtm_bigtujunga30m_utm11.tif",
                                          package = "topotoolboxr")
  dem <- GRIDobj(srtm_bigtujunga30m_utm11)
  expect_s3_class(dem, "GRIDobj")
})