#### Import functions ####
rm(list=ls())
set.seed(1)
Rcpp::sourceCpp("fastball.cpp") #Compile fastball function
Rcpp::sourceCpp("curveball.cpp") #Compile curveball function

#### Swap speed ####
swap.speed <- data.frame(cols = 0, curve = 0, fast = 0)
for (cols in c(100, 1000, 10000, 100000, 1000000)) {
  for (i in 1:10) {
    print(paste0("Col: ", cols, "  i = ", i))
    B <- matrix(rbinom(2*cols,1,0.5),2,cols)  #Generate a two-row random matrix
    Blist <- apply(B==1, 1, which, simplify = FALSE)  #Convert to adjacency list
    
    #Shuffle rows 1000 times using curveball
    curve.start <- Sys.time()
    Blist_ <- curveball_cpp(Blist, 1000)
    curve.end <- Sys.time()
    
    #Shuffle rows 1000 times using fastball
    fast.start <- Sys.time()
    Blist_ <- fastball_cpp(Blist, 1000)
    fast.end <- Sys.time()
    
    #Post results
    swap.speed <- rbind(swap.speed, c(cols,
                                      as.numeric(difftime(curve.end, curve.start, units = "secs")),
                                      as.numeric(difftime(fast.end, fast.start, units = "secs"))))
  }
}
swap.speed <- swap.speed[-1,]
write.csv(swap.speed, "swap.speed.csv")

#### Sampling speed ####
sample.speed <- data.frame(dim = 0, curve = 0, fast = 0)
for (dim in c(10, 31, 100, 316, 1000, 3162, 10000)) {
  for (i in 1:10) {
    print(paste0("Dim: ", dim, "  i = ", i))
    B <- matrix(rbinom(dim*dim,1,0.5),2,dim)  #Generate a dim-by-dim random matrix
    Blist <- apply(B==1, 1, which, simplify = FALSE)  #Convert to adjacency list
    
    #Sample 100 matrices using curveball
    curve.start <- Sys.time()
    for (n in 1:100) {Blist_ <- curveball_cpp(Blist, 5 * dim)}
    curve.end <- Sys.time()
    
    #Sample 100 matrices using fastball
    fast.start <- Sys.time()
    for (n in 1:100) {Blist_ <- fastball_cpp(Blist, 5 * dim)}
    fast.end <- Sys.time()
    
    #Post results
    sample.speed <- rbind(sample.speed, c(dim, 
                                          as.numeric(difftime(curve.end, curve.start, units = "secs")),
                                          as.numeric(difftime(fast.end, fast.start, units = "secs"))))
  }
}
sample.speed <- sample.speed[-1,]
write.csv(sample.speed, "sample.speed.csv")

#### Plots ####
rm(list=ls())
library(ggplot2)
library(latex2exp)

#Swap Time
dat <- read.csv("swap.speed.csv", row.names = 1, header = TRUE)
dat <- dat[,c("cols", "curve", "fast")]
colnames(dat) <- c("cols", "Curveball", "Fastball")
dat <- reshape2::melt(dat, id.vars = c("cols"))
colnames(dat) <- c("Columns", "Algorithm", "Time")

swap <-
ggplot(dat, aes(x=Columns, y=Time, linetype=Algorithm, color=Algorithm)) +
geom_point(shape = 1) + 
geom_smooth(method = loess, se = FALSE) +
scale_y_continuous(trans = "log10", breaks = c(.001, .01, .1, 1, 10, 100, 1000),
                   labels = c(expression(10^-3), expression(10^-2), expression(10^-1), expression(10^0), expression(10^1), expression(10^2), expression(10^3))) + 
scale_x_continuous(trans = "log10", breaks = c(100, 1000, 10000, 100000, 1000000, 10000000),
                   labels = c(expression(10^2), expression(10^3), expression(10^4), expression(10^5), expression(10^6), expression(10^7))) + 
xlab(TeX("Number of bottom nodes \\textit{m} in \\textbf{G}")) + ylab(expression("Seconds to perform 1000 trades")) +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black"),
      axis.text=element_text(size=10), axis.title=element_text(size=10), legend.key.size = unit(2,"line"),
      legend.position = c(0.25, 0.8), legend.text=element_text(size=10), legend.title=element_blank(), legend.key=element_blank())

#Sample Time
dat <- read.csv("sample.speed.csv", row.names = 1, header = TRUE)
dat <- dat[,c("dim", "curve", "fast")]
colnames(dat) <- c("dim", "Curveball", "Fastball")
dat <- reshape2::melt(dat, id.vars = c("dim"))
colnames(dat) <- c("Dimensions", "Algorithm", "Time")

sample <-
ggplot(dat, aes(x=Dimensions, y=Time, linetype=Algorithm, color=Algorithm)) +
  geom_point(shape = 1) + 
  geom_smooth(method = loess, se = FALSE) +
  scale_y_continuous(trans = "log10", breaks = c(.001, .01, .1, 1, 10, 100, 1000),
                     labels = c(expression(10^-3), expression(10^-2), expression(10^-1), expression(10^0), expression(10^1), expression(10^2), expression(10^3))) + 
  scale_x_continuous(trans = "log10", breaks = c(10, 100, 1000, 10000, 100000, 1000000, 10000000),
                     labels = c(expression(10^1), expression(10^2), expression(10^3), expression(10^4), expression(10^5), expression(10^6), expression(10^7))) + 
  xlab(TeX("Number of nodes \\textit{n + m} in \\textbf{G}")) + ylab(TeX("Seconds to sample 100 random \\textbf{G}\\prime")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=10), axis.title=element_text(size=10), legend.key.size = unit(2,"line"),
        legend.position = c(0.25, 0.8), legend.text=element_text(size=10), legend.title=element_blank(), legend.key=element_blank())

#Compile plots
library(cowplot)
pdf("running_time.pdf", height = 3.5, width = 7)
plot_grid(swap, sample, labels = "AUTO", nrow = 1)
dev.off()
