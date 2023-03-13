test_that("Person works", {
	test_person <- Person$new()

	good_email <- "person@example.com"
	bad_email <- "person@@example.com"
	good_orcid <- "0000-0000-0000-0000"
	bad_orcid <- "0000-0000-0000-000"

	expect_true(test_person$check_email(good_email))
	expect_error(
		test_person$check_email(bad_email),
		regexp = "Invalid email address"
	)

	expect_true(test_person$check_orcid(good_orcid))
	expect_error(
		test_person$check_orcid(bad_orcid),
		regexp = "Invalid ORCID"
	)

	test_person$set_email(good_email)
	expect_equal(test_person$email, good_email)
	expect_error(
		test_person$check_email(bad_email),
		regexp = "Invalid email address"
	)

	test_person$set_orcid(good_orcid)
	expect_equal(test_person$orcid, good_orcid)
	expect_error(
		test_person$check_orcid(bad_orcid),
		regexp = "Invalid ORCID"
	)

})
