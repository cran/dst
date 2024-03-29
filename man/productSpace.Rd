% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/productSpace.R
\name{productSpace}
\alias{productSpace}
\title{Product space representation of a relation}
\usage{
productSpace(tt, specnb, infovar)
}
\arguments{
\item{tt}{A (0,1) or boolean matrix, where the variables are set side by side, as in a truth table. Each variable has a number of columns equal to the number of possible values.}

\item{specnb}{A vector of integers ranging from 1 to \code{k}, where \code{k} is the number of subsets of the \code{tt} matrix. Values must start at one and can be increased by 1 or not. They determine the partitioning of the rows of the \code{tt} matrix between the \code{k} subsets.}

\item{infovar}{A two-column matrix containing identification numbers of the variables and the number of elements of each variable (size of the frame).}
}
\value{
The matrix of the product space representation of the relation.
}
\description{
This utility function takes the input matrix of a relation between two or more variables and yields its product space representation.
}
\examples{
 ttfw <- matrix(c(1,0,1,0,0,1,0,1,1,1,1,1),nrow = 3,
  byrow = TRUE, 
  dimnames = list(NULL, c("foul", "fair", "foul", "fair")) )
 specfw <- c(1,1,2) 
 infovarfw <- matrix(c(5,7,2,2), ncol = 2, 
 dimnames = list(NULL, c("varnb", "size")) )
 rownames(ttfw) <- nameRows(ttfw)
 ttfw
productSpace(tt = ttfw, specnb = specfw, infovar = infovarfw)
}
\author{
Claude Boivin
}
