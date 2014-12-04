context("colors are returned correctly")

test_that("Colors work", {
	x <- derrick.palette(6, "Office")
	expect_equal(length(x), 6)
	expect_equal(x[1], "#989D7B")
	# More tests needed
})
