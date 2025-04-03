#! /usr/bin/env Rscript
suppressPackageStartupMessages({
	library(isar)
	library(optparse)
})
option_list <- list(
	make_option(
		opt_str = c("--read-json-investigation", "-j"),
		# default = " %default",
		help = "Read a json representation of an ISA Investigation",
		type = "character",
		# dest = "", name in the options list of the stored value
		action = "store" # store, store_true, store_false, callback
		# callback = function()
	)
)
args <- parse_args2(
	OptionParser(option_list = option_list)# , args = c("--help")
)
if (!is.null(args$options$read_json_investigation)) {
	if(fs::file_exists(args$options$read_json_investigation)) {
		obj <- Investigation$new()
		# print(args)
		obj$from_json(args$options$read_json_investigation)
		obj
	}
}
