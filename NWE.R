#
#
# John Gerick und Fred Brockstedt hu-berlin 2013
#



##  Nadaraya-Watson Estimator
NWE <- function(matrix,KernFunction,bandWidth){
        nmat <- matrix
        n <- nrow(matrix)*ncol(matrix)
        z <- nrow(matrix)
        s <- ncol(matrix)

        ymatrix <- matrix(rep(1:(2*s-1),(2*z-1)),ncol=(2*s-1),byrow=TRUE)
        xmatrix <- matrix(rep(1:(2*z-1),(2*s-1)),ncol=(2*s-1),byrow=FALSE)

        Kh <- sapply( ((z-xmatrix)/bandWidth) , KernFunction) * sapply( ((s-ymatrix)/bandWidth) , KernFunction)
        Kh <- matrix(Kh,ncol=(2*s-1))

        for (zeil in 1:z){
                for (spal in 1:s){
                        nmat[zeil,spal] <- sum(matrix*Kh[(z+1-zeil):(2*z-zeil),(s+1-spal):(2*s-spal)] )/sum(Kh[(z+1-zeil):(2*z-zeil),(s+1-spal):(2*s-spal)] )
                }
        }
        return(nmat)
}
