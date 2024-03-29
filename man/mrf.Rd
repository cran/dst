% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{mrf}
\alias{mrf}
\title{The Captain's Problem. \code{mrf}: Relation between variables No Maintenance (M = false) and  Repairs at sea (R)}
\format{
A (0,1) matrix with 4 rows and 6 columns.
\describe{
  \item{[1,c(1,2)]}{value = 0, not used}
  \item{[1,3:6]}{Identification numbers of the two variables. Column 3,4: variable 6; columns 5,6: variable 8}
  \item{nospec}{identification number of the specification}
  \item{m}{the value of the specification, a number between 0 and 1}
  \item{true}{1 if true is part of the specification, 0 otherwise}
  \item{false}{1 if false is part of the specification, 0 otherwise}
  }
}
\source{
{Almond, R.G. [1988] Fusion and Propagation in Graphical Belief Models. Computing Science and Statistics: Proceedings of the 20th Symposium on the Interface. Wegman, Edward J., Gantz, Donald T. and Miller, John J. (ed.). American Statistical Association, Alexandria, Virginia. pp 365--370.}
}
\usage{
mrf
}
\description{
This dataset is the \code{tt} matrix establishing a set of two relations between the two variables. First, Repairs = true if Maintenance = false in (M x R). We are 20\% sure that there will be Repairs if no maintenance. Second, Repairs = false if Maintenance = false in (M x R). We are 20\% sure that there will be no repairs if no maintenance.
}
\details{
These two relations are implication rules. The remaining mass of 0.6 is allotted to the frame. To construct the \code{tt} matrix, we put the variables M and R side by side, as in a truth table representation. Each pair of the subset is described by a row of the matrix as a vector of zeros and ones.
}
\author{
Claude Boivin, Stat.ASSQ
}
\keyword{datasets}
