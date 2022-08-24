#### Import functions ####
rm(list=ls())
set.seed(1)
Rcpp::sourceCpp("fastball.cpp") #Compile fastball function
Rcpp::sourceCpp("curveball.cpp") #Compile curveball function

#### Randomness ####
#This numerical experiment replicates for fastball the analysis originally used by Strona et al (2014) to illustrate that curveball samples randomly.
#There are five binary matrices with row marginals {1,2,1} and column marginals {1,2,1}.
#Starting with each of these five matrices (A - E), the code generates 100000 new random matrices.
#The output (mat) shows that for each starting matrix, there is an equal probability of generating each of the five possible matrices.
#Because the curveball and fastball algorithms differ only in how they perform trades, but not in their outcomes, this result is trivial.
#At the recommendation of reviewers, this result is not presented in the manuscript.

if (FALSE) {  #Change FALSE to TRUE to run this portion of code
A <- matrix(c(0,1,0,1,0,1,0,1,0),3,3)  #Define each of the five possible starting matrices and their adjacency lists
A <- apply(A==1, 1, which, simplify = FALSE)
B <- matrix(c(0,1,0,1,1,0,0,0,1),3,3)
B <- apply(B==1, 1, which, simplify = FALSE)
C <- matrix(c(1,0,0,0,1,1,0,1,0),3,3)
C <- apply(C==1, 1, which, simplify = FALSE)
D <- matrix(c(0,0,1,1,1,0,0,1,0),3,3)
D <- apply(D==1, 1, which, simplify = FALSE)
E <- matrix(c(0,1,0,0,1,1,1,0,0),3,3)
E <- apply(E==1, 1, which, simplify = FALSE)
dat <- data.frame(start = 0, end = 0)
for (i in 1:100000) {
  print(i)
  for (matrix in 1:5) {
    if (matrix==1) {star <- fastball_cpp(A,25)}  #For each starting matrix, generate 100000 random matrices using fastball
    if (matrix==2) {star <- fastball_cpp(B,25)}
    if (matrix==3) {star <- fastball_cpp(C,25)}
    if (matrix==4) {star <- fastball_cpp(D,25)}
    if (matrix==5) {star <- fastball_cpp(E,25)}
    if (identical(A,star)) {dat <- rbind(dat, data.frame(start = matrix, end = 1))}
    if (identical(B,star)) {dat <- rbind(dat, data.frame(start = matrix, end = 2))}
    if (identical(C,star)) {dat <- rbind(dat, data.frame(start = matrix, end = 3))}
    if (identical(D,star)) {dat <- rbind(dat, data.frame(start = matrix, end = 4))}
    if (identical(E,star)) {dat <- rbind(dat, data.frame(start = matrix, end = 5))}
  }
}
dat <- dat[-1,]
dat <- aggregate(list(count=rep(1,nrow(dat))), dat, length)  #Count how many times each type of matrix is generated from each starting matrix
mat <- matrix(dat$count, 5 ,5)
rownames(mat) <- c("A", "B", "C", "D", "E")
colnames(mat) <- c("A", "B", "C", "D", "E")
}

#### Compare running time of curveball and fastball swaps ####
library(microbenchmark)
library(reshape2)
library(ggplot2)

## Experiment
time <- data.frame(cols = 0, curve = 0, fast = 0)
for (cols in c(100, 316, 1000, 3162, 10000, 31622, 100000, 316228, 1000000)) {
  B <- matrix(c(rep(1,cols/2), rep(0,cols/2), rep(0,cols/2), rep(1,cols/2)), nrow=2, ncol=cols, byrow=T)  #Generate 2 x cols matrix
  Blist <- apply(B==1, 1, which, simplify = FALSE)  #Convert to adjacency list
  for (i in 1:10) {
    print(paste0("Col: ", cols))
    curve_time <- (microbenchmark(curveball_cpp(Blist, 100), times = 1)[1,2])/1e+9  #Seconds for 100 curveball trades
    fast_time <- (microbenchmark(fastball_cpp(Blist, 100), times = 1)[1,2])/1e+9  #Seconds for 100 fastball trades
    time <- rbind(time, c(cols, curve_time, fast_time))  #Post times
  }
}
time <- time[-1,]
write.csv(time, "time.csv")

## Plot
dat <- read.csv("time.csv", row.names = 1, header = TRUE)  #Compute mean relative improvement
dat <- aggregate(dat, list(dat$cols), mean)
dat <- dat[,c("cols", "curve", "fast")]
improve100 <- round(dat$curve[1]/dat$fast[1],1)
improve1000 <- round(dat$curve[3]/dat$fast[3],1)
improve10000 <- round(dat$curve[5]/dat$fast[5],1)
improve100000 <- round(dat$curve[7]/dat$fast[7],1)
improve1000000 <- round(dat$curve[9]/dat$fast[9],1)

dat <- read.csv("time.csv", row.names = 1, header = TRUE)  #Import data and reshape for plotting
dat <- dat[,c("cols", "curve", "fast")]
colnames(dat) <- c("cols", "Curveball", "Fastball")
dat <- melt(dat, id.vars = c("cols"))
colnames(dat) <- c("Columns", "Algorithm", "Time")

pdf("time.pdf", height = 3, width = 3*1.613)  #Plot
ggplot(dat, aes(x=Columns, y=Time, linetype=Algorithm, color=Algorithm)) +
  geom_point(shape = 1, size = 1.5, position = position_jitter(width=.1)) + 
  geom_smooth(method = loess, se = F) +
  scale_y_continuous(trans = "log10", breaks = c(.0001, .001, .01, .1, 1, 10),
                     labels = c(expression(10^-4), expression(10^-3), expression(10^-2), expression(10^-1), expression(10^0), expression(10^1))) + 
  scale_x_continuous(trans = "log10", breaks = c(100, 1000, 10000, 100000, 1000000, 10000000),
                     labels = c(expression(10^2), expression(10^3), expression(10^4), expression(10^5), expression(10^6), expression(10^7))) + 
  annotate("text", x = 100, y = .0001, label = paste0(improve100,"x")) +
  annotate("text", x = 1000, y = .0004, label = paste0(improve1000,"x")) +
  annotate("text", x = 10000, y = .004, label = paste0(improve10000,"x")) +
  annotate("text", x = 100000, y = .04, label = paste0(improve100000,"x")) +
  annotate("text", x = 1000000, y = .4, label = paste0(improve1000000,"x\nfaster")) +
  xlab("Number of Bottom Nodes (m)") + ylab("Seconds to perform 100 trades") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=10), axis.title=element_text(size=10), legend.key.size = unit(2,"line"),
        legend.position = c(0.25, 0.8), legend.text=element_text(size=10), legend.title=element_blank(), legend.key=element_blank())
dev.off()

#### Practical application ####
library(incidentally)
library(backbone)
library(igraph)

## Obtain data
senate_bills <- incidence.from.congress(session = 116, types = "s", format = "igraph")
incidence <- as_incidence_matrix(senate_bills)
incidence_list <- apply(incidence==1, 1, which, simplify = FALSE)

## Number of trades required to randomly sample one bipartite graph
trades <- 5 * nrow(incidence)

## Number of samples required for two-tailed test
alpha <- 0.05
two.tailed <- alpha/2
samples <- ceiling((stats::power.prop.test(p1 = two.tailed * 0.95, p2 = two.tailed, sig.level = alpha, power = (1 - alpha), alternative = "one.sided")$n)/2)

## Running time for Cpp curveball
sample <- as.numeric(system.time(curveball_cpp(incidence_list, trades))[3])
curveball.time <- (sample * samples)/60  #In minutes

## Running time for Cpp fastball
sample <- as.numeric(system.time(fastball_cpp(incidence_list, trades))[3])
fastball.time <- (sample * samples)/60  #In minutes

## Ordinary projection
proj <- bipartite_projection(senate_bills, which = "false")
V(proj)$color <- rgb(1,0,0,.5)  #Define the color of Republicans
V(proj)$color[which(V(proj)$party=="D")] <- rgb(0,0,1,.5)  #...of Democrats
V(proj)$color[which(V(proj)$party=="I")] <- rgb(0,1,0,.5)  #...of Independents

## Backbone extraction using fastball-FDSM
bb <- fdsm(senate_bills, signed = TRUE)
bb <- delete_vertices(bb, which(degree(delete_edges(bb, which(E(bb)$weight==-1)))==0))  #Delete all-negative vertices
V(bb)$color <- rgb(1,0,0,.5)  #Define the color of Republicans
V(bb)$color[which(V(bb)$party=="D")] <- rgb(0,0,1,.5)  #...of Democrats
V(bb)$color[which(V(bb)$party=="I")] <- rgb(0,1,0,.5)  #...of Independents
E(bb)$color <- rgb(0,1,0,.5)  #Define color of positive edges
E(bb)$color[which(E(bb)$weight==-1)] <- rgb(1,.5,.5,.01)  #Define color of negative edges
layout <- layout_nicely(delete_edges(bb, which(E(bb)$weight==-1)))  #Get layout based on positive edges

## Plot
pdf("network.pdf", height = 3, width = 3*1.613)
par(mar = c(0,0,4,0), mfrow=c(1,2))
plot(proj, vertex.label = NA, vertex.color = V(proj)$color, vertex.frame.color = NA, vertex.size = 5, edge.color = rgb(.5,.5,.5,.1), main = "Raw Projection")
plot(bb, vertex.label = NA, vertex.color = V(bb)$color, vertex.frame.color = NA, vertex.size = 5, layout = layout, main = "FDSM Backbone")
dev.off()
