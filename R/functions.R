#' Recode reverse scored values
#'
#' Recodes variables that are reverse scored on Likert-type scales
#'
#' @param x variable to be reverse coded
#' @param highest the number from which to subtract the observed value; default = 6
#'
#' @return correctly scored numeric vector
#'
#' @export

reverse = function(x, highest = 6) {
  x_rev = highest - x
  return(x_rev)
}

#' Subtract from existing values
#'
#' Subtracts constant from values
#'
#' @param x variable to modify
#' @param minus constant to subtract from original value; default = 1
#'
#' @return adjusted numeric vector
#'
#' @export

minus = function(x, minus = 1){
  x1 = x - minus
  return(x1)
}

#' Add to existing values
#'
#' Adds constant to values
#'
#' @param x variable to modify
#' @param minus constant to subtract from original value; default = 1
#'
#' @return adjusted numeric vector
#'
#' @export

add = function(x, add = 1){
  x1 = x + add
  return(x1)
}

#' Recode variables
#'
#' Recodes existing variables to new constant
#'
#' @param x variable to recode
#' @param value value to recode; default = 1
#' @param final desired value; default = 99
#'
#' @return recoded numeric vector
#'
#' @export

recode = function(x, value = 1, final = 99){
  x1 = ifelse(x == value, final, x)
  return(x1)
}

#' Dichotomize values
#'
#' Classifies existing values into two new categories or groups
#'
#' @param x variable to be dichotomized
#' @param cutoff value used to split groups; <= cutoff
#' @param group0 value assigned to members of first group; default = 0
#' @param group1 value assigned to members of second group; default = 1
#'
#' @return binary vector
#'
#' @export

dichotomize = function(x, cutoff, group0 = 0, group1 = 1){
  x1 = ifelse(x <= cutoff, group0, group1)
  return(x1)
}

#' Categorize values
#'
#' Classifies existing values into three new categories or groups
#'
#' @param x variable to be categorized
#' @param value1 first cutoff used to split groups; <= cutoff1
#' @param value2 second cutoff used to split groups; > cutoff1 & <= cutoff2
#' @param group0 value assigned to members of first group; default = 0
#' @param group1 value assigned to members of second group; default = 1
#' @param group2 value assigned to members of third group; default = 2
#'
#' @return categorical vector
#'
#' @export

categorize = function(x, value1, value2, group0 = 0, group1 = 1, group2 = 2){
  x1 = ifelse(x <= value1, group0,
              ifelse(x > value1 & x <= value2, group1, group2))
  return(x1)
}

`%>%` = magrittr::`%>%`
