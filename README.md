# listcompr

The **listcompr** package is a light-weight collection of functions for list comprehension. It is intended as “syntactic sugar” for R and it is inspired by the list comprehension capabilities from Python. Next to lists, similar structures like vectors (of numeric or character type), data frames, or named lists can be easily composed. The package may be used for the simple generation of small data sets for “textbook examples”, for unit tests of your R code, or for tiny mathematical tasks.

## Installation

To install the latest version from CRAN, simply use:

    install.packages("listcompr")

To install and load the latest development version from this repository, execute the following in R:

    install.packages("devtools")
    library(devtools)

    install_github("patrickroocks/listcompr", build_vignettes = TRUE)
    library(listcompr)

## Examples

### Tiny examples

We show the basic functionality to compose lists, vectors and data frames.
The first argument for each generator function (`gen.list`, `gen.vector`, and `gen.data.frame`) is a base expression, and the other arguments are variable ranges and conditions.

First, we want to get a vector of all numbers in `1:10` which can be divided by 3 or 4:

    gen.vector(i, i = 1:10, i %% 3 == 0 || i %% 4 == 0)
    ## Returns: c(3, 4, 6, 8, 9)

Next we want to compose a list of tuples `c(i, j)` where `i` and `j` are from `1:3` and` i <= j` holds:

    gen.list(c(i, j), i = 1:3, j = i:3)
    ## Returns: list(c(1, 1), c(1, 2), c(2, 2), c(1, 3), c(2, 3), c(3, 3))

There is also a function to compose a data frame which expects a named vector as base expression.
For example we can easily sum up `1:i` while iterating over `i`:

    gen.data.frame(c(n = a, sum = sum(1:a)), a = 1:10)

The first three lines of this data frame are:

            n sum
        1   1   1
        2   2   3
        3   3   6

### Nested calculations and named lists

In **listcompr**, the list and vector compositions can be nested. 
We create a vector containing all "perfect numbers" between 2 and 100, i.e., numbers where the sum of the divisors equals the number:

    gen.vector(a, a = 2:100, a == sum(gen.vector(x, x = 1:(a-1), a %% x == 0)))
    ## Returns: c(6, 28)

There are also functions to compose characters. For instance the package offers a function `gen.named.list`, where the first argument is the name (expressions in `{}`-brackets are substituted) of each list entry. The remaining arguments are the same as for `gen.list`. The following statement gives us all the divisors between 5 and 10:

    gen.named.list('divisors_of_{a}', gen.vector(x, x = 1:(a-1), a %% x == 0), a = 5:10)
    
The last list entry has the name `'divisors_of_10'` and the content `c(1, 2, 5)`.

### More examples

After installation of the package, run

    vignette("introduction", package = "listcompr")

to see a vignette with some more examples.

## Contact

To submit bugs or suggest improvements, feel free to write a mail to me: Patrick Roocks, mail@p-roocks.de
