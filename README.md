
# Introduction
This Repository contains the regmodelsuite Package

# R Package tutorial
Useful commands in order to create a R package.
1) devtools package
``` r
install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
```
2) Install packages
``` r
devtools::install_github(path) #
devtools::install(path) # Install source package
devtools::install_local(path) # Install Developerversion (.tar.gz)
```
3) Build

``` r
devtools::build() # Build as .tar.gz
devtools:build(binary = TRUE) # Build as binary. Depends on OS
usethis::create_package() # Helper function which creates a package related folder. Creates an .Rproj file to work in RStudio.
usethis::use_r() # Create a new .R Script within the Package.
devtools::load_all() # Reload all Scripts within the current session
devtools::check() # Checks if the package meets the standard requirements
```
![Alt Text](https://r-pkgs.org/diagrams/install-load.png)
4) Checking
Should be done regularly in order to check if the package is still valid.
```r
devtools::check() # Runs R CMD check 
```

# Function template
The main function accepts a formula and other parameters 
```r
functionWrapper(y ~ x, lambda, cv, data, ...) {
mframe <- model.frame(formula, data = data)

      # get the model matrix
      X <- model.matrix(formula, data = data)
      # get the response variable (lhs of the formula)
      y <- model.response(mframe)
}
```
the function then extracts the model matrix X and response vector y which then can be passed to other functions for estimation
```r
ridge(X, y, lambda) ...
```
