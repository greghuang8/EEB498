pd_included <- read.csv("pd_included.csv")
bin <- c(0, 500, 1000, 2000, 5000, 150000)
pd_histogram <- hist(pd_included$pdSTD, breaks = bin)
pd_histogram$counts
str(pd_histogram)
