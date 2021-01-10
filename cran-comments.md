## Response to the remarks on the last submission

> Please always write package names, software names and API (application programming interface) names in single quotes in title and description. e.g: Python --> 'python'

I decided to remove "Python" from the description. Changed it to 'python' in the vignette.

> Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation. Please write about the structure of the output (class) and also what the output means. (If a function does not return a value, please document that too, e.g. \value{No return value, called for side effects} or similar)
> Missing Rd-tags:
>      gen.list.char.Rd: \value
>      gen.list.expr.Rd: \value
>      gen.list.Rd: \value
>      gen.logical.and.Rd: \value

I added a `\value` field to all the functions. Moreover, I restructured the help texts a bit (e.g., introduced a section "Syntactic Features" in gen.list) to keep the order of the text as I intended it to read.

> Please do not modify the global environment (e.g. by using <<-). This is not allowed by the CRAN policies. 

I removed the two test cases doing this.
I also removed the usages of `<<-` from the inner helper function `add_segment()` in `prepare_char_pattern`. This did actually not modify the global environment, but only the parent environment. But as I am not sure if this is undocumented behavior of R, I removed that too.


## Test environments
* rhub: Fedora Linux, R-devel, clang, gfortran
* rhub: Ubuntu Linux 16.04 LTS, R-release, GCC
* local: Ubuntu 18.04, R 3.6.3

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

    * checking CRAN incoming feasibility ... NOTE
    Maintainer: ‘Patrick Roocks <mail@p-roocks.de>’
    
    New submission

I think this NOTE is OK.
