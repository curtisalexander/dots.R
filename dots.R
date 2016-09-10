library("dplyr")
library("purrr")

## test function =====
# notice that I pass in dots to the function

# could actually check that the arguments passed in conform
#   to some acceptable list
# for instance, dots are ultimately passed through to grepl and the possible
#   options that can be passed to grepl are:
#     - ignore.case
#     - perl
#     - fixed
#     - value
#     - useBytes

# in my mind there are two options:
#   - ignore incorrectly passed in arguments to keep the program from crashing
#   - throw an error immediately informing the user of an incorrect argument
# my preference would be to use throw an error, but I've demonstrated both
#   arg_treatment can be "error" or "ignore"
match_cols <- function(tbl, patt, arg_treatment = "ignore", ...) {

  dots <- list(...)

  # printing
  if (!length(dots) == 0) {
    message("The names of the extra arguments are:")
    message(purrr::map_chr(dots,
                           ~ paste0(names(dots[match(.x, dots)]),
                                    " = ",
                                    .x,
                                    "\n")))
  }

  # columns from the dataframe
  cols <- dplyr::tbl_vars(tbl)

  grepl_options <- c("ignore.case", "perl", "fixed", "value", "useBytes")

  # either ignore an incorrect argument or throw an error
  if (arg_treatment == "ignore") {
    # filter out options that are not possible, side-stepping potential errors
    if (!length(dots) == 0) {
      keep_list <- purrr::keep(names(dots), ~ .x %in% grepl_options)
      dots_final <- dots[keep_list]
    } else {
      dots_final <- dots
    }
  } else if (arg_treatment == "error") {
      remove_list <- purrr::keep(names(dots), ~ !.x %in% grepl_options)
      if (length(remove_list) == 0) {
        dots_final <- dots
      } else {
        stop("Incorrect arguments were supplied\n",
             purrr::map_chr(dots[remove_list],
                            ~ paste0(names(dots[remove_list]),
                                    " = ",
                                    .x,
                                    "\n")))
      }
  } else {
    stop("arg_treatment must be either \"error\" or \"ignore\"")
  }

  # because we are manipulating the dots argument, construct a list of arguments
  #   to pass into grepl
  # splice inserts a list element in another element without creating nesting
  grepl_args <- purrr::splice(list(pattern = patt, x = cols), dots_final)
  purrr::keep(.x = cols,
              .p = do.call(grepl, grepl_args))
}

## setup =====
# create a dataframe
key <- c(1000L, 2000L, 3000L, 4000L, 1000L)
category <- c("A", "B", "C", "A", "C")
amount <- c("46.41", "118.11", "84.68", "493.59", "51.10")

test_df <- data.frame(key, category, amount)

## ignore dots =====
# returns "amount"
match_cols(tbl = test_df, patt = "^am")

# assertthat::are_equal(
#   match_cols(tbl = test_df, patt = "^AM"),
#              vector(mode = "character", length = 0))
# returns 0 length character vector as there aren't any matches
match_cols(tbl = test_df, patt = "^AM")

## utilize dots =====
# now pass ignore.case as a named parameter
# returns "amount"
match_cols(tbl = test_df,
           patt = "^am",
           ignore.case = TRUE)

# also returns "amount"
match_cols(tbl = test_df,
           patt = "^AM",
           ignore.case = TRUE)

## utilize dots, with incorrect arguments =====
# pass in a bogus argument

## "ignore"
# "ignore" is the default
# returns "amount"
match_cols(tbl = test_df,
           patt = "^am",
           ignore.case = TRUE,
           blah = 1)

# explicitly pass in "ignore"
# returns "amount"
match_cols(tbl = test_df,
           patt = "^am",
           arg_treatment = "ignore",
           ignore.case = TRUE,
           blah = 1)

# pass in a bogus value to "arg_treatment"
# throws an error
match_cols(tbl = test_df,
           patt = "^am",
           arg_treatment = "badarg",
           ignore.case = TRUE,
           blah = 1)

# pass "error" to arg_treatment
# throws an error
match_cols(tbl = test_df,
           patt = "^am",
           arg_treatment = "error",
           ignore.case = TRUE,
           blah = 1)
