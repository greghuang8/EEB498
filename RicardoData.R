#Ricardo's data plots
#Greg Huang

#Set working directory
setwd("C:/Users/Greg/Documents/4th/EEB498/EEB498_Project")

#load in data for creeper
creeper_data <- read.csv("Creeper_data.csv", header = TRUE)
str(creeper_data)

#load in data for woodpecker
woodpecker_data <- read.csv("Woodpecker_data.csv", header = TRUE)
str(woodpecker_data)

#load in data for marten
marten_data <- read.csv("Marten_data.csv", header = TRUE)
str(marten_data)

##########Plot creeper data for Number of Home Ranges##########
index <- c(1:19)
y <- creeper_data$SROBurton_AVG_NHR
xlabel <- "Years"
ylabel <- "Number of Home Ranges (mean+/-S.E.)"
y2label <- "Home Range Density (mean+/-S.E.)"
avg_SRO <- creeper_data$SROBurton_AVG_NHR
err_SRO <- creeper_data$SROBurtonERROR_NHR
avg_SQ <- creeper_data$X1SQFireAVG_NHR
err_SQ <- creeper_data$X1SQFireERROR_NHR
avg_FIRE <- creeper_data$FireAVG_NHR
err_FIRE <- creeper_data$FireERROR_NHR
#plot SROBURTON_AVG
plot(y,type = "o", col = "blue", xlab = xlabel, ylab = ylabel, pch =0, ylim = c(1000,4300), axes = FALSE, ann = FALSE)
axis(1, at=1:19, lab=c(5,10,15,20,25,30,35,40,45,50,100,150,200,250,300,350,400,450,500))
title(main = "Creeper - Number of Home Ranges (Average)")
title(xlab = xlabel)
title(ylab = ylabel)
box()
#add line for 1SQFIRE_AVG
lines(creeper_data$X1SQFireAVG_NHR, type = "o", pch = 1, lty = 2, col = "red")
#add line for FIRE_AVG
lines(creeper_data$FireAVG_NHR, type = "o", pch = 2, lty = 3, col = "green")
axis(side = 2, at = c(1000,1500,2000,2500,3000,3500,4000,4200))
#add error bars for all three lines
arrows(index, avg_SRO-err_SRO, index, avg_SRO+err_SRO, length = 0.05, angle = 90, code = 3 )
arrows(index, avg_SQ-err_SQ, index, avg_SQ+err_SQ, length = 0.05, angle = 90, code = 3 )
arrows(index, avg_FIRE-err_FIRE, index, avg_FIRE+err_FIRE, length = 0.05, angle = 90, code = 3 )
#add legend
legend("bottomleft",legend =c("SROBurton", "1SQFire", "Fire"), col=c("blue", "red", "green"), lty = 1:3, pch = 0:2, cex = 0.8, bty = "n")

##########plot creeper data for home range density (HRD)##########
y <- creeper_data$SROBurton_AVG_HRD
avg_SRO <- creeper_data$SROBurton_AVG_HRD
err_SRO <- creeper_data$SROBurtonERROR_HRD
avg_SQ <- creeper_data$X1SQFireAVG_HRD
err_SQ <- creeper_data$X1SQFireERROR_HRD
avg_FIRE <- creeper_data$FireAVG_HRD
err_FIRE <- creeper_data$FireERROR_HRD
#plot SROBURTON_AVG
plot(y,type = "o", col = "blue", xlab = xlabel, ylab = ylabel, pch =0, ylim = c(70,95), axes = FALSE, ann = FALSE)
axis(1, at=1:19, lab=c(5,10,15,20,25,30,35,40,45,50,100,150,200,250,300,350,400,450,500))
title(main = "Creeper - Home Range Density (Average)")
title(xlab = xlabel)
title(ylab = y2label)
box()
#add line for 1SQFIRE_AVG
lines(avg_SQ, type = "o", pch = 1, lty = 2, col = "red")
#add line for FIRE_AVG
lines(avg_FIRE, type = "o", pch = 2, lty = 3, col = "green")
axis(side = 2, at = c(70,75,80,85,90,95))
#add error bars for all three lines
arrows(index, avg_SRO-err_SRO, index, avg_SRO+err_SRO, length = 0.05, angle = 90, code = 3 )
arrows(index, avg_SQ-err_SQ, index, avg_SQ+err_SQ, length = 0.05, angle = 90, code = 3 )
arrows(index, avg_FIRE-err_FIRE, index, avg_FIRE+err_FIRE, length = 0.05, angle = 90, code = 3 )
#add legend
legend("bottomleft",legend =c("SROBurton", "1SQFire", "Fire"), col=c("blue", "red", "green"), lty = 1:3, pch = 0:2, cex = 0.8, bty = "n")

##########plot woodpecker data for number of home ranges (NHR)##########
y <- woodpecker_data$SROBurton_AVG_NHR
avg_SRO <- woodpecker_data$SROBurton_AVG_NHR
err_SRO <- woodpecker_data$SROBurtonERROR_NHR
avg_SQ <- woodpecker_data$X1SQFireAVG_NHR
err_SQ <- woodpecker_data$X1SQFireERROR_NHR
avg_FIRE <- woodpecker_data$FireAVG_NHR
err_FIRE <- woodpecker_data$FireERROR_NHR
#plot SROBURTON_AVG
plot(y,type = "o", col = "blue", xlab = xlabel, ylab = ylabel, pch =0, ylim = c(0,50), axes = FALSE, ann = FALSE)
axis(1, at=1:19, lab=c(5,10,15,20,25,30,35,40,45,50,100,150,200,250,300,350,400,450,500))
title(main = "Woodpeckers - Number of Home Ranges (Average)")
title(xlab = xlabel)
title(ylab = ylabel)
box()
#add line for 1SQFIRE_AVG
lines(avg_SQ, type = "o", pch = 1, lty = 2, col = "red")
#add line for FIRE_AVG
lines(avg_FIRE, type = "o", pch = 2, lty = 3, col = "green")
axis(side = 2, at = c(0,10,20,30,40))
#add error bars for all three lines
arrows(index, avg_SRO-err_SRO, index, avg_SRO+err_SRO, length = 0.05, angle = 90, code = 3 )
arrows(index, avg_SQ-err_SQ, index, avg_SQ+err_SQ, length = 0.05, angle = 90, code = 3 )
arrows(index, avg_FIRE-err_FIRE, index, avg_FIRE+err_FIRE, length = 0.05, angle = 90, code = 3 )
#add legend
legend("bottomleft",legend =c("SROBurton", "1SQFire", "Fire"), col=c("blue", "red", "green"), lty = 1:3, pch = 0:2, cex = 0.8, bty = "n")

##########plot woodpecker data for home range density (HRD)##########
y <- woodpecker_data$SROBurton_AVG_HRD
avg_SRO <- woodpecker_data$SROBurton_AVG_HRD
err_SRO <- woodpecker_data$SROBurtonERROR_HRD
avg_SQ <- woodpecker_data$X1SQFireAVG_HRD
err_SQ <- woodpecker_data$X1SQFireERROR_HRD
avg_FIRE <- woodpecker_data$FireAVG_HRD
err_FIRE <- woodpecker_data$FireERROR_HRD
#plot SROBURTON_AVG
plot(y,type = "o", col = "blue", xlab = xlabel, ylab = ylabel, pch =0, ylim = c(0,90), axes = FALSE, ann = FALSE)
axis(1, at=1:19, lab=c(5,10,15,20,25,30,35,40,45,50,100,150,200,250,300,350,400,450,500))
title(main = "Woodpecker - Home Range Density (Average)")
title(xlab = xlabel)
title(ylab = y2label)
box()
#add line for 1SQFIRE_AVG
lines(avg_SQ, type = "o", pch = 1, lty = 2, col = "red")
#add line for FIRE_AVG
lines(avg_FIRE, type = "o", pch = 2, lty = 3, col = "green")
#format y-axis
axis(side = 2, at = c(0,20,40,60,80))
#add error bars for all three lines
arrows(index, avg_SRO-err_SRO, index, avg_SRO+err_SRO, length = 0.05, angle = 90, code = 3 )
arrows(index, avg_SQ-err_SQ, index, avg_SQ+err_SQ, length = 0.05, angle = 90, code = 3 )
arrows(index, avg_FIRE-err_FIRE, index, avg_FIRE+err_FIRE, length = 0.05, angle = 90, code = 3 )
#add legend
legend("bottomleft",legend =c("SROBurton", "1SQFire", "Fire"), col=c("blue", "red", "green"), lty = 1:3, pch = 0:2, cex = 0.8, bty = "n")

##########plot marten data for number of home ranges (NHR)##########
y <- marten_data$SROBurton_AVG_NHR
avg_SRO <- marten_data$SROBurton_AVG_NHR
err_SRO <- marten_data$SROBurtonERROR_NHR
avg_SQ <- marten_data$X1SQFireAVG_NHR
err_SQ <- marten_data$X1SQFireERROR_NHR
avg_FIRE <- marten_data$FireAVG_NHR
err_FIRE <- marten_data$FireERROR_NHR
#plot SROBURTON_AVG
plot(y,type = "o", col = "blue", xlab = xlabel, ylab = ylabel, pch =0, ylim = c(5,17), axes = FALSE, ann = FALSE)
axis(1, at=1:19, lab=c(5,10,15,20,25,30,35,40,45,50,100,150,200,250,300,350,400,450,500))
title(main = "Marten - Number of Home Ranges (Average)")
title(xlab = xlabel)
title(ylab = ylabel)
box()
#add line for 1SQFIRE_AVG
lines(avg_SQ, type = "o", pch = 1, lty = 2, col = "red")
#add line for FIRE_AVG
lines(avg_FIRE, type = "o", pch = 2, lty = 3, col = "green")
axis(side = 2, at = c(6,8,10,12,14,16))
#add error bars for all three lines
arrows(index, avg_SRO-err_SRO, index, avg_SRO+err_SRO, length = 0.05, angle = 90, code = 3 )
arrows(index, avg_SQ-err_SQ, index, avg_SQ+err_SQ, length = 0.05, angle = 90, code = 3 )
arrows(index, avg_FIRE-err_FIRE, index, avg_FIRE+err_FIRE, length = 0.05, angle = 90, code = 3 )
#add legend
legend("topleft",legend =c("SROBurton", "1SQFire", "Fire"), col=c("blue", "red", "green"), lty = 1:3, pch = 0:2, cex = 0.8, bty = "n")

##########plot marten data for home range density (HRD)##########
y <- marten_data$SROBurton_AVG_HRD
avg_SRO <- marten_data$SROBurton_AVG_HRD
err_SRO <- marten_data$SROBurtonERROR_HRD
avg_SQ <- marten_data$X1SQFireAVG_HRD
err_SQ <- marten_data$X1SQFireERROR_HRD
avg_FIRE <- marten_data$FireAVG_HRD
err_FIRE <- marten_data$FireERROR_HRD
#plot SROBURTON_AVG
plot(y,type = "o", col = "blue", xlab = xlabel, ylab = ylabel, pch =0, ylim = c(65,100), axes = FALSE, ann = FALSE)
axis(1, at=1:19, lab=c(5,10,15,20,25,30,35,40,45,50,100,150,200,250,300,350,400,450,500))
title(main = "Marten - Home Range Density (Average)")
title(xlab = xlabel)
title(ylab = y2label)
box()
#add line for 1SQFIRE_AVG
lines(avg_SQ, type = "o", pch = 1, lty = 2, col = "red")
#add line for FIRE_AVG
lines(avg_FIRE, type = "o", pch = 2, lty = 3, col = "green")
#format y-axis
axis(side = 2, at = c(70,80,90,100))
#add error bars for all three lines
arrows(index, avg_SRO-err_SRO, index, avg_SRO+err_SRO, length = 0.05, angle = 90, code = 3 )
arrows(index, avg_SQ-err_SQ, index, avg_SQ+err_SQ, length = 0.05, angle = 90, code = 3 )
arrows(index, avg_FIRE-err_FIRE, index, avg_FIRE+err_FIRE, length = 0.05, angle = 90, code = 3 )
#add legend
legend("topleft",legend =c("SROBurton", "1SQFire", "Fire"), col=c("blue", "red", "green"), lty = 1:3, pch = 0:2, cex = 0.8, bty = "n")