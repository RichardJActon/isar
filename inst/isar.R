#! /usr/bin/env Rscript
library(isar)
library(optparse)
option_list <- list(
	make_option(
		c("-", "--"),
		default = "",
		help = "",
		type = "",
		dest = "",
		action = ""
	)
)
parse_args2(
	OptionParser(option_list = option_list),
	args = c("--help")
)
