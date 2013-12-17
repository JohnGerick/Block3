# 
# John Gerick , Fred Brockstedt 2013 hu-berlin.de
#
if (!"doMC" %in% installed.packages()) install.packages("doMC", repos='http://cran.us.r-project.org')
library("doMC")
registerDoMC(4)

source("NWE.R")


##load library
library("EBImage")

##read JPG Image
image <- readImage("katze.jpg")
imageComb <- image

##create normally distributed distortion
standardDiv <- 8/20   ##########     <<<<----    #########STÖRUNG VARIATION MÖGLICH####################
distortion <- matrix(rnorm((nrow(image)*ncol(image)),mean=0,sd=standardDiv),nrow=nrow(image))

##distort image
imageWithDistortion <- image
imageWithDistortion[,,1] <- image[,,1]+distortion
imageWithDistortion[,,2] <- image[,,2]+distortion
imageWithDistortion[,,3] <- image[,,3]+distortion


##GaussKern
GaussKern <- function(x) {
  return(exp((x*x)/(-2))/sqrt(2*pi)) 
  }
  
##SquareKern
SquareKern <- function(x) {
  if (abs(x)<=1) {return(0.5)}
  else {return(0)}
  }

##TriangleKern
TriangleKern <- function(x) {
  if (abs(x)<=1) {return(1-abs(x))}
  else {return(0)}
  }

##EpanechnikovKern
EpanechnikovKern <- function(x) {
  if (abs(x)<=1) {return((3-3*(x*x))/4)}
  else {return(0)}
  }


for (bandWidth in c(0.1,0.5,1,2)){
  foreach (kern = c("GaussKern", "SquareKern", "TriangleKern", "EpanechnikovKern")) %dopar% {
    funKern <- get(kern)
    blurImage <- NWE(imageWithDistortion,funKern,bandWidth)
    writeImage(blurImage,sprintf("output/%s-%3.5f.jpg", kern, bandWidth))
  }
}




##show Images

imageComb <- combine(imageComb,imageWithDistortion)
display(imageComb)
