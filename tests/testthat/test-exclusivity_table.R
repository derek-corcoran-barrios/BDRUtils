

test_that("Test exclusivity_table function", {
  LT <- data.frame(
    planned_strictly_protected = c("Yes", "Yes", "Yes", "Yes", NA, NA, NA, NA),
    strictly_protected = c("Yes", "Yes", NA, NA, "Yes", "Yes", NA, NA),
    sea_natural_areas = c("Yes", NA, "Yes", NA, "Yes", NA, "Yes", NA),
    Area_sq_Km = c(4163.9369, 0.0043, 136.5007, 0.0001, 2153.2234, 23.0762, 24540.5325, 327940.2363)
  )

  # Test 1: Check if the function returns a data frame
  result <- exclusivity_table(c("strictly_protected", "sea_natural_areas"), LT)
  expect_s3_class(result, "data.frame")

  # Test 2: Check if the result contains the expected columns
  expected_columns <- c("Variable", "Total", "Exclusive", "Non_exclusive")
  expect_equal(colnames(result), expected_columns)


})
