# pattern for lists of refernces ----
# for example with ontologies
# there is a 'list' of the ontology sources used in an investigation
# each ontology annotation with a specific term draws that term from a source
# each source should be listed in the ontology source references on the investigation
#

ex_lst <- list(
	study = list(
		cats_used = list(
			list(
				`@id` = "#cat/ingredients",
				type = "ingredients"#,
				# type = list(
				# 	termid = "1",
				# 	source = "recipies",
				# 	anno = "ingredients"
				# )
			),
			list(
				`@id` = "#cat/instructions",
				type = "instructions"#,
			)
		),
		samples = list(
			list(characteristics = list(
				list(
					cat = list(`@id` = "#cat/ingredients"),
					value = c("1" = "spice")
					#list(termid="1",source="ingredients",anno="spice")
				),
				name = "sample1 example 1"
			)),
			list(characteristics = list(
				list(
					cat = list(`@id` = "#cat/ingredients"),
					value = c("1" = "spice")
						#list(termid="1",source="ingredients",anno="spice")
				),
				name = "sample1 example 2"
			))
		)
	)
)

# instance ----
Char <- R6Class("Char", public = list(
	cat = NULL,
	value = NULL,
	Charcatlst = NULL,
	initialize = function(cat = NULL, value = NULL, Charcatlst = NULL){
		self$value <- value
		if (is.null(Charcatlst)) {
			self$Charcatlst <- CharCatLst$new()
		} else {
			self$Charcatlst <- Charcatlst
		}

		if(is.null(cat) || is.null(value)) {
			# does not handle all cases optimally
			self$cat <- cat
			self$value <- value
		} else {
			self$set_valid_cat_and_value(cat, value)
		}

	},
	check_cat = function(cat){
		check <- checkmate::check_r6(cat, "CharCat")
		error_with_check_message_on_failure(check)
	},
	set_cat = function(cat) {
		if(self$check_cat(cat)){
			self$cat <- cat
		}
	},
	set_valid_cat_and_value = function(cat, value) {
		if(self$check_cat(cat)) {
			if(cat$name %in% names(self$Charcatlst$CharCats)) {
				self$cat <- self$Charcatlst$CharCats[[cat$name]]
				# value present check - add value if missing?
				if(value %in% names(self$cat$values)) {
					self$value <- value
				} else {
					warning("Value not listed for category trying to add...")
					self$cat$add_values(purrr::set_names(value, value)) # use name if set?
					self$value <- value
				}
			} else {
				self$Charcatlst$CharCats[["UnknownCat"]] <- CharCat$new(
					name = "UnknownChar"
				)
				self$cat <- self$Charcatlst$CharCats[["UnknownCat"]]
				self$cat$add_values(purrr::set_names(value, value))  # use name if set?
				self$value <- value
			}
		}
	},
	from_list = function(lst) {
		self$set_valid_cat_and_value(
			CharCat$new(name = lst[["cat"]][["@id"]]), # throw away object? # use name - rework validation?
			lst[["value"]]
			#cat, value
		)
		#self$value <- lst[["value"]] # ont anno handle seperately
		#self$cat <- lst[["category"]][["@id"]]
	}
))

ch <- Char$new()
ch$cat
ch$Charcatlst$CharCats
ch$name

ex_lst$study$samples[[1]]$characteristics[[1]] %>% ch$from_list()

ch

# reference ----
CharCat <- R6Class("CharCat", public = list(
	name = character(),
	type = NULL,
	values = character(),
	initialize = function(name = character(), values = character()){
		self$name <- name
		#self$values <- values
		self$add_values(values)
	},
	check_values = function(values) { # s
		check <- checkmate::check_character(
			c(self$values, values), names = "unique"
		)
		error_with_check_message_on_failure(check)
	},
	set_values = function(values) { # s
		if(self$check_values(values)) {
			self$values <- values
		}
	},
	add_values = function(values) { # s
		if(self$check_values(values)) {
			self$values <- c(self$values, values)
		}
	},
	from_list = function(lst) {
		self$name <- lst$`@id` # check valid ontology
		self$type <- lst$type
	}
))

cc <- CharCat$new()
cc$name
cc$values
# named vector of possible values for Char value
# value id, name value
cc$values <- c(a="a",b="b")
cc$add_values(c(x="x",y="x"))
cc$values

# reference list ----
CharCatLst <- R6Class("CharCatLst", public = list(
	CharCats = list(),
	initialize = function(CharCats = list()){
		self$CharCats <- CharCats
	},
	#check_cat = function()
	#check_cats = function()
	add_cats = function(Cats) {
		self$CharCats <- c(self$CharCats, Cats)
	},
	from_list = function(lst) {
		self$CharCats <- lst %>% # assign overwrite
			purrr::map(~{
				cc <- CharCat$new()
				cc$from_list(.x)
				cc
			}) %>%
			purrr::set_names(purrr::map_chr(., ~.x$name))
			#self$add_cats() # add append?
	}
))

ccl <- CharCatLst$new()
# named list of char cat objects
ccl$CharCats <- list(
	x=CharCat$new(name = "x"),y=CharCat$new(name = "y")
)

ccl$from_list(ex_lst$study$cats_used)

s$Charcatlst$add_cats(ccl$CharCats)
# s$Charcatlst <- ccl #!!! need to use a set method update or breaks reference
# make private?
s$Charcatlst$CharCats
s$char1$Charcatlst$CharCats

# example of an object which will house the reference list ----
Stdy <- R6Class("Stdy", public = list(
	name = character(),
	Charcatlst = NULL,
	char1 = NULL,
	initialize = function(name = character(), Charcatlst = NULL, char1 = NULL){
		# checkmate::assert_r6(Charcatlst, "CharCatLst", null.ok = TRUE)
		# checkmate::assert_string(name)
		# checkmate::assert_r6(char1, "Char", null.ok = TRUE)
		if(is.null(Charcatlst)) {
			self$set_Charcatlst(CharCatLst$new())
		} else {
			self$Charcatlst <- Charcatlst#$new()
		}

		if (is.null(char1)) {
			self$set_char1(Char$new(Charcatlst = self$Charcatlst))
		} else {
			self$char1 <- char1
			# don't directly init - set method that checks charcatlst?
		}
	},
	check_Charcatlst = function(Charcatlst){
		check <- checkmate::check_r6(Charcatlst, "CharCatLst")
		error_with_check_message_on_failure(check)
	},
	set_Charcatlst = function(Charcatlst){
		if(self$check_Charcatlst(Charcatlst)) {
			self$Charcatlst <- Charcatlst
		}
	},
	check_char1 = function(char1){
		check <- checkmate::check_r6(char1, "Char")
		error_with_check_message_on_failure(check)
	},
	set_char1 = function(char1) {
		if(self$check_char1(char1)){
			self$char1 <- char1
		}
	},
	from_list = function(lst) {
		self$set_Charcatlst({
			ccl <- CharCatLst$new()
			ccl$from_list(lst[["cats_used"]])
			ccl
		})
		self$set_char1({
			ch <- Char$new(Charcatlst = self$Charcatlst)
			ch$from_list(lst[["samples"]][[1]][["characteristics"]][[1]])
			ch
		})
	}
))

s <- Stdy$new()
s$from_list(ex_lst$study)

s$name
s$Charcatlst
s$char1
s$char1$Charcatlst

s$Charcatlst$CharCats <- list(
	a=CharCat$new(name = "a"),b=CharCat$new(name = "b")
)
s$Charcatlst$CharCats
s$char1$Charcatlst$CharCats


###



ccl_ex <- CharCatLst$new(CharCats = list(ingredient = "ingredient"))

s_ex_noCharCats <- Stdy$new(name = "s_ex_noCharCats")

s_ex_CharCats <- Stdy$new(
	name = "s_ex_CharCats",
	Charcatlst = ccl_ex,
	char1 = Char$new("ingredient", name = "spice", Charcatlst = ccl_ex)
)
s_ex_CharCats$Charcatlst$CharCats
s_ex_CharCats$char1$Charcatlst

char_ex <- Char$new(cat = "NULL", name = "unspec")
char_ex$cat$values


# check set pattern ----
# check method validates the input for a given object property
# set calls check on the input before accepting it
#
