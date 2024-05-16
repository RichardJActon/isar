# pattern for lists of refernces ----
# for example with ontologies
# there is a 'list' of the ontology sources used in an investigation
# each ontology annotation with a specific term draws that term from a source
# each source should be listed in the ontology source references on the investigation
#

# example of an object which will house the reference list
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

	}
))

s <- Stdy$new()
s$name
s$Charcatlst
s$char1
s$char1$Charcatlst

s$Charcatlst$CharCats <- list(
	a=CharCat$new(name = "a"),b=CharCat$new(name = "b")
)
s$Charcatlst$CharCats
s$char1$Charcatlst$CharCats

# reference list
CharCatLst <- R6Class("CharCatLst", public = list(
	CharCats = list(),
	initialize = function(CharCats = list()){
		self$CharCats <- CharCats
	},
	add_cats = function(Cats) {
		self$CharCats <- c(self$CharCats, Cats)
	},
	from_list = function(lst) {

	}
))

ccl <- CharCatLst$new()
# named list of char cat objects
ccl$CharCats <- list(
	x=CharCat$new(name = "x"),y=CharCat$new(name = "y")
)

s$Charcatlst$add_cats(ccl$CharCats)
# s$Charcatlst <- ccl #!!! need to use a set method update or breaks reference
# make private?
s$Charcatlst$CharCats
s$char1$Charcatlst$CharCats

# reference
CharCat <- R6Class("CharCat", public = list(
	name = character(),
	values = character(),
	initialize = function(name = character(), values = character()){
		self$name <- name
		self$values <- values
	},
	from_list = function(lst) {

	}
))

cc <- CharCat$new()
cc$name
cc$values
# named vector of possible values for Char value
# value id, name value
cc$values <- c(a="a",b="b")



# instance
Char <- R6Class("Char", public = list(
	cat = NULL,
	name = character(),
	Charcatlst = NULL,
	initialize = function(cat = NULL, name = character(), Charcatlst = NULL){
		self$cat <- cat
		self$name <- name
		if (is.null(Charcatlst)) {
			self$Charcatlst <- CharCatLst$new()
		} else {
			self$Charcatlst <- Charcatlst
		}
		if(is.null(cat)) { self$cat <- NULL } else {
			if(self$cat %in% names(Charcatlst$CharCats)) {
				self$cat <- Charcatlst$CharCats[[self$cat]]
				# value present check - add value if missing?
			} else {
				Charcatlst$CharCats[["UnknownCat"]] <- CharCat$new(
					name = "UnknownChar",
					values = list(self$name)
				)
				self$cat <- Charcatlst$CharCats[["UnknownCat"]]
			}
		}

	},
	from_list = function(lst) {

	}
))

ch <- Char$new()
ch$cat
ch$Charcatlst$CharCats
ch$name

ch$
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
