# 
# John Gerick , Fred Brockstedt 2013 hu-berlin.de
#
source("ProgressBar.R")


##load library
library("EBImage")

##read JPG Image
katze <- readImage("katze.jpg")

## change Image to Greyscale
colorMode(katze) <- "Grayscale"

## use the first Greyscale generated
katze <- katze[,,1]

## show the Image
##display(katze)

kaComb <- katze
for (k in 1:50) {
ProgressBar(100*k/50)

##normalverteiltes rauschen erzeugen
standardDiv <- k/50
stoer <- matrix(rnorm((nrow(katze)*ncol(katze)),mean=0,sd=standardDiv),nrow=nrow(katze))

##Bild stoeren
katzeMS <- katze+stoer
kaComb <- combine(kaComb,katzeMS)
}


cat("\n")
##show all Images

display(kaComb)








##GaussKern
GaussKern <- function(x) {return exp((x*x)/(-2))/sqrt(2*pi) }
