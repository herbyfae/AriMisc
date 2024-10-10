test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


# Creating a date vector


dates = seq(as.Date("13-09-2024", format = "%d-%m-%Y"), by = "day", length.out = 17)

set.seed(123)
dates = sample(dates, 100, replace = T)

summary(dates)

which(dates < "2024-09-21")
