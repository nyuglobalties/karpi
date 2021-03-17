test_that("Dummy column insertion works properly", {
  inserted <- insert_multiple_choice_dummies(
    mtcars,
    "cyl",
    c("4", "6", "8"),
    "."
  )

  new_cols <- paste0("cyl.", c(4, 6, 8))
  expect_true(all(new_cols %in% names(inserted)))

  cyl_col <- which("cyl" == names(mtcars))

  expect_true(which("cyl.4" == names(inserted)) == (cyl_col + 1))
  expect_true(which("cyl.6" == names(inserted)) == (cyl_col + 2))
  expect_true(which("cyl.8" == names(inserted)) == (cyl_col + 3))
})
