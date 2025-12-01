test_that("Checks if data path loads correctly", {
  data("srtm_bigtujunga30m_utm11")
  expect_type(srtm_bigtujunga30m_utm11, "character")
  expect_true(nchar(srtm_bigtujunga30m_utm11) > 0)
  expect_true(file.exists(srtm_bigtujunga30m_utm11))
  dem <- GRIDobj(srtm_bigtujunga30m_utm11)
  expect_s3_class(dem, "GRIDobj")
  expect_type(dem, "list")
})