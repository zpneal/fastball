#### Import functions ####
rm(list=ls())
set.seed(5)
Rcpp::sourceCpp("fastball.cpp") #Compile fastball function
Rcpp::sourceCpp("curveball.cpp") #Compile curveball function

#### Compare running time of curveball and fastball swaps ####
swap.speed <- data.frame(cols = 0, curve = 0, fast = 0)
for (cols in c(100, 1000, 10000, 100000, 1000000, 10000000)) {
  for (i in 1:10) {
    print(paste0("Col: ", cols))
    B <- matrix(rbinom(20*cols,1,0.5),20,cols)  #Generate a twenty-row random matrix
    Blist <- apply(B==1, 1, which, simplify = FALSE)  #Convert to adjacency list
    
    #Perform 100 trades using curveball
    curve_time <- as.numeric(system.time(curveball_cpp(Blist, 100))[3])

    #Perform 100 trades using fastball
    fast_time <- as.numeric(system.time(fastball_cpp(Blist, 100))[3])

    #Post results
    swap.speed <- rbind(swap.speed, c(cols, curve_time, fast_time))
  }
}
swap.speed <- swap.speed[-1,]
write.csv(swap.speed, "swap.speed.csv")

## Plot
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

pdf("time.pdf", height = 3, width = 3*1.613)
ggplot(dat, aes(x=Columns, y=Time, linetype=Algorithm, color=Algorithm)) +
  geom_point(shape = 19, size = 2.5) + 
  geom_smooth(method = loess, se = FALSE) +
  scale_y_continuous(trans = "log10", breaks = c(.001, .01, .1, 1, 10, 100, 1000),
                     labels = c(expression(10^-3), expression(10^-2), expression(10^-1), expression(10^0), expression(10^1), expression(10^2), expression(10^3))) + 
  scale_x_continuous(trans = "log10", breaks = c(100, 1000, 10000, 100000, 1000000, 10000000),
                     labels = c(expression(10^2), expression(10^3), expression(10^4), expression(10^5), expression(10^6), expression(10^7))) + 
  xlab("Number of Bottom Nodes (m)") + ylab("Seconds to perform 100 trades") +
  annotate("errorbar", x = 14000000, ymin = fast7, ymax = curve7, colour = "black", width = .1, size = .75) +
  annotate("text", x = 26000000, y = (fast7 + curve7)/2.6, label = paste0(diff7,"x"), size = 4) +
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

## Running time for naive R curveball
sample <- as.numeric(system.time(curveball(incidence, trades))[3])
Rcurveball.time <- (sample * samples)/60  #In minutes

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
