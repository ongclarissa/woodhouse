% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{categorize}
\alias{categorize}
\title{Categorize values}
\usage{
categorize(x, value1, value2, group0 = 0, group1 = 1, group2 = 2)
}
\arguments{
\item{x}{variable to be categorized}

\item{value1}{first cutoff used to split groups; <= cutoff1}

\item{value2}{second cutoff used to split groups; > cutoff1 & <= cutoff2}

\item{group0}{value assigned to members of first group; default = 0}

\item{group1}{value assigned to members of second group; default = 1}

\item{group2}{value assigned to members of third group; default = 2}
}
\value{
categorical vector
}
\description{
Classifies existing values into three new categories or groups
}
