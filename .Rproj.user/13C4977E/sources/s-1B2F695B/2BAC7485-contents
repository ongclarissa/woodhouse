#' Plot multiple histograms
#'
#' Plots histogram for each vector in data frame
#'
#' @param data data frame
#' @param binwidth binwidth in histogram; default = 1
#' @param ... other arguments that are passed to the ggplot() function
#'
#' @return plot in R environment
#'
#' @export
#'
#' @import ggplot2

histograms = function(data, binwidth = 1, ...){

  namae = names(data)

  for(i in seq_along(namae)){
    print(ggplot(data,
                 aes_string(x = namae[i])) +
            geom_histogram(binwidth = binwidth,
                           na.rm = TRUE,
                           alpha = 0.7,
                           fill = "lavenderblush4") +
            theme_bw())
  }
}

#' Plot multiple quantile-quantile plots
#'
#' Plots q-q plot for each vector in data frame
#'
#' @param data data frame
#' @param ... other arguments that are passed to the ggplot() function
#'
#' @return plot in R environment
#'
#' @export
#'
#' @import ggplot2

qqplots = function(data, ...){

  namae = names(data)

  for(i in seq_along(namae)){
    print(ggplot(data) +
            stat_qq(aes_string(sample = namae[i])) +
            labs(x = namae[i]) +
            theme_bw())
  }
}

#' Plot multiple scatterplots
#'
#' Plots scatterplot for DV and each specified IV in data frame
#'
#' @param data data frame
#' @param y dependent variable
#' @param x subsetted data frame of independent variables
#' @param fit specify modeling function for best fitting line, default = "lm"
#' @param ... other arguments that are passed to the ggplot() function
#'
#' @return plot in R environment
#'
#' @export
#'
#' @import ggplot2

scatterplots = function(data, y, x, fit = "lm", ...){

  yname = deparse(substitute(y))
  xname = names(x)

  for(i in seq_along(xname)){
    print(ggplot(data,
                 aes_string(y = yname,
                            x = xname[i])) +
            geom_point(na.rm = TRUE) +
            geom_smooth(method = fit,
                        se = FALSE,
                        na.rm = TRUE,
                        color = "indianred4") +
            theme_bw())
  }
}

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

#' Convert factors to numeric vectors
#'
#' Converts factors to numeric vectors without changing original values
#'
#' @param x variable to be converted
#'
#' @return numeric vector
#'
#' @export

to_numeric = function(x){
  x_num = as.numeric(as.character(x))
  return(x_num)
}

#' Do a preliminary cleaning of Qualtrics data
#'
#' Removes non-value rows and changes variable names to lowercase
#'
#' @param data dataframe to be cleaned
#'
#' @return dataframe
#'
#' @export

clean_qualtrics = function(data){

  data1 <- data %>%
    slice(-1, -2) %>%
    rename_all(tolower)

  return(data1)

}

#' Add prefix to variable name
#'
#' Renames variable by adding character string before existing variable name
#'
#' @param x variable to be renamed
#' @param prefix character string to be added
#'
#' @return renamed variable
#'
#' @export

add_prefix = function(x, prefix){

  x1 = paste0("prefix", x)
  return(x1)

}


#' Remove prefix from variable names
#'
#' Renames variables by removing character string from existing variable names
#'
#' @param data data where variables are located
#' @param prefix character string to be removed
#'
#' @return renamed variable
#'
#' @export

remove_prefix = function(data, prefix){

  data1 <- data %>%
    rename_at(vars(starts_with("prefix")), ~str_remove(., "prefix"))

  return(data1)

}

`%>%` = magrittr::`%>%`
