# listcompr

The listcompr package is a light-weight collection of functions for list comprehension. It is intended as “syntactic sugar” for R and it is inspired by the list comprehension capabilities from Python. Next to lists, similar structures like vectors (of numeric or character type), data frames, or named lists can be easily composed. The intended area of application is especially combinatorics. Moreover the package may be used for the simple generation of small data sets for, e.g., “textbook examples” or for unit tests of your R code.

## Install it

To install the latest version from this repository, execute the following in R:
`
install.packages("devtools")
library(devtools)

install_github("patrickroocks/rpref")
library(rPref)
`

## Examples

A vector of all numbers in 1:10 which be divided by 3 or 4:
`
gen.vector(i, i = 1:10, i %% 3 == 0 || i %% 4 == 0)
# Returns: [1] 3 4 6 8 9
`

A list of tupels (i,j) where i,j are from 1:3 and i<=j:
`
gen.list(c(i, j), i = 1:3, j = i:3)
`

## More examples

Run
`
vignette("introduction", package = "listcompr")
`
to see a vignette with some more examples.
