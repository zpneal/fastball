#### Import functions ####
rm(list=ls())
set.seed(1)
Rcpp::sourceCpp("fastball.cpp") #Compile fastball function
Rcpp::sourceCpp("curveball.cpp") #Compile curveball function

#### Swap speed ####
swap.speed <- data.frame(cols = 0, curve = 0, fast = 0)
for (cols in c(100, 1000, 10000, 100000, 1000000, 10000000)) {
for (i in 1:10) {
  print(paste0("Col: ", cols))
  B <- matrix(rbinom(2*cols,1,0.5),2,cols)  #Generate a two-row random matrix
  Blist <- apply(B==1, 1, which, simplify = FALSE)  #Convert to adjacency list
    
  #Perform 100 trades using curveball
  curve.start <- Sys.time()
  Blist_ <- curveball_cpp(Blist, 100)
  curve.end <- Sys.time()
    
  #Perform 100 trades using fastball
  fast.start <- Sys.time()
  Blist_ <- fastball_cpp(Blist, 100)
  fast.end <- Sys.time()
    
  #Post results
  swap.speed <- rbind(swap.speed, c(cols,
                                    as.numeric(difftime(curve.end, curve.start, units = "secs")),
                                    as.numeric(difftime(fast.end, fast.start, units = "secs"))))
}
}
swap.speed <- swap.speed[-1,]
write.csv(swap.speed, "swap.speed.csv")

#### Plot ####
rm(list=ls())
library(ggplot2)

dat <- read.csv("swap.speed.csv", row.names = 1, header = TRUE)
dat <- aggregate(dat, list(dat$cols), mean)
dat <- dat[,c("cols", "curve", "fast")]
colnames(dat) <- c("cols", "Curveball", "Fastball")
curve7 <- dat$Curveball[6]
fast7 <- dat$Fastball[6]
diff7 <- round(curve7/fast7,1)
curve4 <- dat$Curveball[3]
fast4 <- dat$Fastball[3]
dat <- reshape2::melt(dat, id.vars = c("cols"))
colnames(dat) <- c("Columns", "Algorithm", "Time")

pdf("plot.pdf", height = 3, width = 3*1.613)
ggplot(dat, aes(x=Columns, y=Time, linetype=Algorithm, color=Algorithm)) +
geom_point(shape = 19, size = 2.5) + 
geom_smooth(method = loess, se = FALSE) +
scale_y_continuous(trans = "log10", breaks = c(.001, .01, .1, 1, 10, 100, 1000),
                   labels = c(expression(10^-3), expression(10^-2), expression(10^-1), expression(10^0), expression(10^1), expression(10^2), expression(10^3))) + 
scale_x_continuous(trans = "log10", breaks = c(100, 1000, 10000, 100000, 1000000, 10000000),
                   labels = c(expression(10^2), expression(10^3), expression(10^4), expression(10^5), expression(10^6), expression(10^7))) + 
xlab("Incidence matrix columns") + ylab("Seconds to perform 100 trades") +
annotate("errorbar", x = 14000000, ymin = fast7, ymax = curve7, colour = "black", width = .1, size = .75) +
annotate("text", x = 26000000, y = (fast7 + curve7)/2.6, label = paste0(diff7,"x"), size = 4) +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black"),
      axis.text=element_text(size=10), axis.title=element_text(size=10), legend.key.size = unit(2,"line"),
      legend.position = c(0.25, 0.8), legend.text=element_text(size=10), legend.title=element_blank(), legend.key=element_blank())
dev.off()

#### Example ####
rows <- 10000
swaps <- rows*5
for_curveball <- ((swaps/100)*curve4)
for_fastball <- ((swaps/100)*fast4)
