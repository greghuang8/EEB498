install.packages('adespatial')
install.packages("tripack")
library(adespatial)
library(tripack)
insect_data <- read.csv("Insect.csv")
plot.default(insect_data$x_meters,insect_data$y_meters)
str(insect_data)
summary((insect_data))
set.seed(3)
xyir <- matrix(runif(20), 10, 2)
plot(xyir, pch = 20, main = "Irregular sampling with 10 sites")
delaunay <- tri.mesh(insect_data$x_meters,insect_data$y_meters)
plot(delaunay)
delaunay
 
numbers <- c(5,2,6,83435,54)
x <- 5:9
firstNum <- head(numbers,4)
rgStripes <- c("red", "grey")

