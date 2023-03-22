test_that("ProtocolParameter works", {
	pp1 <- ProtocolParameter$new()
	pp1a <- pp1
	pp2 <- ProtocolParameter$new()
	expect_true(identical(pp1, pp1a))
	expect_false(identical(pp1, pp2))
})
