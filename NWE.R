## This Skript defines NWE for usage in Block3.R
##
## John Gerick und Fred Brockstedt hu-berlin 2013
##



##  Nadaraya-Watson Estimator
NWE <- function(matrix,KernFunction,bandWidth){
## computes new matrix using Nararaya-Watson Estimation
##
##input:
##	matrix - matrix with real
##	KernFunction - a function
##	nadWidth - a real
##
##output:
##	nmat - matrix with real
##
        nmat <- matrix
        n <- nrow(matrix)*ncol(matrix)
        z <- nrow(matrix)
        s <- ncol(matrix)

        xmatrix <- matrix(rep(1:(2*z-1),(2*s-1)),ncol=(2*s-1),byrow=FALSE)
        ymatrix <- matrix(rep(1:(2*s-1),(2*z-1)),ncol=(2*s-1),byrow=TRUE)

        Kh <- sapply( ((z-xmatrix)/bandWidth) , KernFunction) * sapply( ((s-ymatrix)/bandWidth) , KernFunction)
        Kh <- matrix(Kh,ncol=(2*s-1))

        for (zeil in 1:z){
                for (spal in 1:s){
                    tmp <- Kh[(z+1-zeil):(2*z-zeil),(s+1-spal):(2*s-spal)]
                    nmat[zeil,spal] <- sum(matrix * tmp)/sum(tmp)
                }
        }

        return(nmat)
}
