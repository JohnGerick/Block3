ProgressBar <- function(percent) {
    ## shows a graphical progress bar on the terminal
    ## it wont print a new line at the end!
    ## best called within a loop
    ## Args:
    ##      percent - a value between 0 and 100; will make the progress bar show n percent
    ## Returns:
    ##      0 - all good
    ##      1 - argument error 
    if(percent < 0 || percent > 100) {
        return(1)
    }
    BARLENGTH <- 71                                                # length of the progressbar in characters (for 80 character interface)
    nStars    <- round(percent*(BARLENGTH/100))                    # amount of stars to print in the bar
    stars     <- paste(rep("*",nStars), collapse="")
    fill      <- paste(rep(" ",BARLENGTH-nStars), collapse="")     # string for filling the rest of the bar
    cat(sprintf("\r |%s%s| %3.0f%%",stars, fill, percent))
    return(0)
}


## Unit test
UNITTEST <-  FALSE # TRUE #
if(UNITTEST) {
    if(ProgressBar(-1) == 1) {
        print("Lower bound check: Ok")
    }
    else {
        print("Lower bound check: Failed")
    }
        if(ProgressBar(100.1) == 1) {
        print("Upper bound check: Ok")
    }
    else {
        print("Upper bound check: Failed")
    }
    print("0", ProgressBar(0))
    print("0.5", ProgressBar(0.5))
    print("1", ProgressBar(1))
    print("10", ProgressBar(10))
    print("50", ProgressBar(50))
    print("100", ProgressBar(100))
    for(k in seq(1,100)) {
        ProgressBar(k)
        Sys.sleep(0.1)
    }
    ## just a new line at the end
    cat("\n")
}
