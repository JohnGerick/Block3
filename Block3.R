## reads an image named katze.jpg and distorts it with normal distribution,
## then it uses NWE to blur the distorted image with different kernel-functions and
## different bandwith. The results are saved in an directory named output
## 
## John Gerick , Fred Brockstedt 2013 hu-berlin.de
##


##load library
if (!"doMC" %in% installed.packages()) install.packages("doMC", repos='http://cran.us.r-project.org')
library("doMC")
registerDoMC(4)

library("EBImage")


## source needed code
source("NWE.R")


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

imageComb <- combine(imageComb,imageWithDistortion)


GaussKern <- function(x) {
## compute gaussian kernel function
## 
## input:
##	x - real
## 
## output:
##	real
##
  return(exp((x*x)/(-2))/sqrt(2*pi)) 
  }
  

SquareKern <- function(x) {
## compute square kernel function
## 
## input:
##	x - real
## 
## output:
##	real
##
  if (abs(x)<=1) {return(0.5)}
  else {return(0)}
  }


TriangleKern <- function(x) {
## compute triangle kernel function
## 
## input:
##	x - real
## 
## output:
##	real
##
  if (abs(x)<=1) {return(1-abs(x))}
  else {return(0)}
  }


EpanechnikovKern <- function(x) {
## compute epanechnikov kernel function
## 
## input:
##	x - real
## 
## output:
##	real
##
  if (abs(x)<=1) {return((3-3*(x*x))/4)}
  else {return(0)}
  }


## creates smothed pictures

bw <- c(0.5,1,2,5)

for (bandWidth in bw){
  foreach (kern = c("GaussKern", "SquareKern", "TriangleKern", "EpanechnikovKern")) %dopar% {
    funKern <- get(kern)
    blurImage <- imageWithDistortion
    for (color in 1:3){
      blurImage[,,color] <- NWE(imageWithDistortion[,,color],funKern,bandWidth)
    }
## save the pictures in directory "output"
    writeImage(blurImage,sprintf("output/%s-%3.5f.jpg", kern, bandWidth))
    print(sprintf("Output saved as: output/%s-%3.5f.jpg", kern, bandWidth))
  }
}




##show Images

display(imageComb)
