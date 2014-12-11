vr <- json2veris("./data")

test_that("a new verisr object has the correct number of records", {
  expect_equal( nrow( vr), 100)
})

test_that("a new verisr object has the correct classes", {
  expect_equal(class(vr), c("verisr", "data.table", "data.frame"))
})

test_that("a new verisr object has these columns", {
  # Note: not testing every column, just some key ones that have been added
  expect_equal(("victim.industry.name" %in% names(vr)), TRUE)
  expect_equal(("victim.industry2" %in% names(vr)), TRUE)
})