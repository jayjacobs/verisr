vr <- json2veris("./data")

test_that("a new verisr object has the correct number of records", {
  expect_equal( nrow( vr), 100)
})

test_that("a new verisr object has the correct classes", {
  expect_equal(class(vr), c("verisr", "data.table", "data.frame"))
})