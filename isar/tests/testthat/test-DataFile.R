test_that("DataFile works", {
	test_data_file <- DataFile$new()
	# x <- test_data_file$clone(deep = TRUE)
	# expect_true(identical(test_data_file, x))
	# expect_true(identical(test_data_file, test_data_file))

})
#
# test_data_file$get_id()
# x$get_id()
#
# test_data_file$filename
# x$filename
#
# test_data_file$label
# x$label
#
# test_data_file$generated_from
# x$generated_from
#
# test_data_file$comments
# x$comments
#
# obj_pub_props<- c(
# 	"filename",
# 	"label",
# 	"generated_from",
# 	"comments"
# )
#
# y <-test_data_file
# obj_pub_props_lgl <- c(purrr::map_lgl(
# 		obj_pub_props, ~identical(x[[.x]], y[[.x]])
# 	),
# 	identical(x$get_id(), y$get_id())
# )
#
#
# identical.DataFile <- function(
# 	x, y, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE,
# 	ignore.bytecode = TRUE, ignore.environment = FALSE,
# 	ignore.srcref = TRUE, extptr.as.ref = FALSE
# ) {print("working")}
#
#
#
# object <- 1
# class(object) <- "Example"
# class(object)
#
# print.Example <- function(x, ...) {print(as.character(x))}
# print(object)
#
# identical.DataFile <- function(x, y) {print("working")}
# #<- s3_identical_maker(obj_pub_props, get_id = TRUE)
#
# identical.DataFile(x,test_data_file)
# class(x)
# identical(x,test_data_file)
#
# sloop::s3_dispatch(identical(x,test_data_file))
#
# print.DataFile <- function(x) {
# 	print("printed")
# }
#
# print(x)
