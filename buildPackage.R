## Building the package
## ---------------------------------------------------------------------

# setwd("~/Documents/Github/sws_util/") #Josh's directory

library(roxygen2)

## Remove the folder if it exists
if(file.exists("./faoswsUtil"))
    unlink("faoswsUtil", recursive = TRUE)

## Build the package
package.skeleton("faoswsUtil", code_files = paste("./codes/",
                           dir("./codes/", pattern = "\\.R$"), sep = ""),
                 force = FALSE)


## Include the DESCRIPTION file
file.copy(from = "./DESCRIPTION", to = "faoswsUtil/",
          overwrite = TRUE)
unlink("./faoswsUtil/Read\\-and\\-delete\\-me")

## Use roxygen to build the documentation
roxygenize("faoswsUtil")
unlink("./faoswsUtil/inst/", recursive = TRUE)

## Build and check the package
system("R CMD INSTALL --build faoswsUtil")
system("R CMD build faoswsUtil")
## system("R CMD check --as-cran faoswsUtil")

