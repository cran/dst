# dst 1.5.2



# dst 1.5.1.9004

## nzdsr: 2023-04-17.  

* Added another missing specification to variable vacuous
*  2023-04-01. Added a missing specification to variable vacuous.

## bcaTrunc: 
*  2023-01-08. New. Function to allow truncation of a belief function containing a large number of focal elements to a threshold value.

## bcaRel, dsrwon, elim, peeling: 
* Removed a recursive function and simplified code. 

## tabresul: 
* 2023-01-03. Function revised. Removed a lag between row labels and data

## dotprod: 
* 2023-01-04. Check to input parameters added.

## dsrwon: 
* Added parallel computing capability, using multiple cores.

# dst 1.5.1

* addTobca: 2021-12-13. Added a check to remove duplicate subsets.

* tabresul: 2021-12-14. Correction to the ordering of the three last rows of the table of results.

* Vignette Zadeh's Example: 2021-12-21. Done with the peeling algorithm.

# dst 1.5.0

* Functions revised. All functions : code simplification and documentation

* dsrwon: 2021-04-23. Calculation of the measure of conflict revised to take into account the case of the combination with the vacuous belief function. This case is used at the beginning of nzdsr (normalization) to update the I12 matrix.

* elim: 2021-05-30: example revised; 2021-07-06: call to fn matrixToMarray changed.  Call to new fn "nameCols_prod"
* extmin: 2021-08-19: call to fn matrixToMarray changed.
* marrayToMatrix: 2021-06-29. Function call changed. Call to new fn "nameCols_prod".
* matrixToMarray: 2021-06-18. Function call changed
* nameCols: 2021-06-28. Function call changed
* nameCols_prod: 2021-06-28. New function. Used in functions extmin and elim.
* tabresul: 2021-07-30. Sort the results in order to always show the frame at the last line of the table
* peeling: New. 2021-09-06. Compute the belief function of a variable of interest by successive elimination of all the other variables of an hypergraph.

# dst 1.4.1.9002

* Correction to function plautrans: The check for the presence of the empty set did not work properly.

# dst v1.2.1.9000

* 1) function extmin: If there is no missing variable, simply take the input as the result.

# dst v1.4.1
* version 1.4.1 published on CRAN 2020-03-28. As of 2020-02-23
* added utility function bcaPrint for printing of the subsets of non-zero  mass of a belief function.
* added examples vignettes: Zadeh's example, Captain's problem

* function dsrwon: added a check on values names to ensure that the two bca's combined are defined on the same space.

* function extmin: added checks on names of variables, as well as their number and their position in a product space

# dst 1.4.0
* version 1.4.0 published on CRAN 2019-08-20
* Correction to fn extmin: Added a test for variable names missing.

* Correction to productSpace function. Transpose of array removed at the end of the function.

# dst v 1.3.0 (on CRAN 2018-12-05)
* added utility functions 'matrixToMarray' and 'marrayToMatrix' to execute the product space conversion of multidimensional data represented by a matrix, and vice-versa.

* debugging of function 'extmin' (extension of data to a larger product space).

* debugging of function 'elim' (reduction of a product space by elimination of one variable).

# dst v1.2.0.9001
* 2018-07-07. 
* Correction to fn dsrwon: added the parameter relnb in the call of the function, to allow for specification the result with a new relation number.

# dst v1.2.0.9000
* 2018-05-01. 
* Correction to fn tabresul: added a check for the case of the empty set present with m_empty = 0

# dst v1.1.0.9000
* 2018-05-01. 
* Correction to fn belplau: added a check for the case of the empty set present with m_empty = 0
* Re-writed the referencing of source code taken on Rpubs

# dst v1.0.0 (Release date on CRAN: 2018-04-25)

* Changes from version 0.3:
* Added a vignette: The Monty hall Game, an introduction to belief functions.

* functions (new and updated):

* addTobca: New function. Adds some elements of 0 mass to an existing mass function.

* bca: New version. Sets a class named "bcaspec". Parameters added to work with definitions on product spaces (relations).

* bcaRel: New function to define a belief function on a product space.

* belplau: Calculation of measures of belief, plausibility and ratio of plausibility.

* decode: utility function
* dotprod: utility function
* double: utility function
* shape: utility function
* encode: New utility function. Convert a value to its representation in another chosen base.
* reduction: utility function to obtain the summary of a vector for any operator

* dsrwon: Combination of two mass functions

* elim: This is a new function. This function works on a relation defined on a product of two variables or more.  Having fixed a variable to eliminate from the relation,  the reduced product space is determined and the corresponding reduced bca is computed.This operation is also called "marginalization".

* extmin: Extension of a relation to a greater product space

* inters: Intersection of two tables of propositions

* nameRows: New function: Using the column names of a matrix to construct names for the rows

* nzdsr: Normalization of results from Dempster's rule of combination.

* productSpace: New function. Product space representation of a relation

* plautrans:Plausibility transformation of the singletons of a frame

* tabresul:  Prepare a table of results.

* Removal of obsolete functions:
butLast, combmasses, dempster, initsing, rplau, transfo

# dst v0.3: Initial release of package on CRAN (2015-01-13)
