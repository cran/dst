% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{swr}
\alias{swr}
\title{The Captain's Problem. \code{swr}: Relation between variables Sailing delay (S), Weather at sea (W), and Repairs at sea (R)}
\format{
An integer matrix with 6 rows and 10 columns.
\describe{
  \item{[1,c(1,2)]}{value = 0, not used}
  \item{[1,3:10]}{Identification numbers of the three variables. Column 3 to 6: variable 3; columns 7,8: variable 7, columns 9,10: variable 8}
  \item{nospec}{identification number of the specification}
  \item{m}{the value of the specification, a number between 0 and 1}
  \item{3}{1 if 3 is part of the specification, 0 otherwise}
  \item{2}{1 if 2 is part of the specification, 0 otherwise}
  \item{1}{1 if 1 is part of the specification, 0 otherwise}
  \item{0}{1 if 0 is part of the specification, 0 otherwise}
  \item{foul}{1 if foul is part of the specification, 0 otherwise}
  \item{fair}{1 if fair is part of the specification, 0 otherwise}
  \item{true}{1 if true is part of the specification, 0 otherwise}
  \item{false}{1 if false is part of the specification, 0 otherwise}
  }
}
\source{
{Almond, R.G. [1988] Fusion and Propagation in Graphical Belief Models. Computing Science and Statistics: Proceedings of the 20th Symposium on the Interface. Wegman, Edward J., Gantz, Donald T. and Miller, John J. (ed.). American Statistical Association, Alexandria, Virginia. pp 365--370.}
}
\usage{
swr
}
\description{
This dataset is the \code{tt} matrix establishing a relation between S, W and R, where S = 0:3, W = (foul, fair) and R = (true, false). The goal of this relation is to account for other causes of sailing delay. All the elements (s,w,r) of (S x W x R) where W or R is true add one day of sailing delay. We put a mass value of 0.9 to this subset. The remaining mass of 0.1 is allotted to the frame.
}
\details{
To construct the \code{tt} matrix, we put the variables S, W, R side by side, as in a truth table representation. Each triplet of the subset is described by a row of the matrix as a vector of zeros and ones.
}
\author{
Claude Boivin, Stat.ASSQ
}
\keyword{datasets}
