
library("flowCore")
library("flowViz")
library("lattice")
library("grid")


source("assets/AllNormFun.R")

load("Gates") #in varable all.gate are the gating information for all the 24 files
p <- getwd()
p <- paste0("/FCSfile/unormdata/")
frames <- read.flowSet(path = "../FCSfile/unormdata/",alter.names=T) #alter.names --> change to r conform names




bead <- colnames(frames[[1]])[c(-1,-2,-5,-8,-10,-11,-12)]
DNA <- colnames(frames[[1]])[10]
chan <- colnames(frames[[1]])[c(-1,-2,-10,-11,-12)]
col <- colnames(frames[[1]])
f <- list()
f.norm <- NULL

#Input FlowSet

#use the fsApply --> is simmilar as lapply for flowFrames but all 24 gates are saved in all.gate
#all.gate <- fsApply(frames, gateExtr, bead, DNA) #gate info already stored

#With GateVIs --> able to check te gates of one file --> last number is the number of the file
gateVis(frames,bead,all.gate,13)

#Transform the data and add beadID vector to data
for(h in 1:length(frames)){
  g <- all.gate[[h]]
  f[[h]] <- beadIdentification(frames[[h]],bead,chan,DNA, g)
}

#normalize all files
f.norm <- normalize.fs(f,bead,DNA,chan)


##Input FlowFrame
data <- frames[[1]]
ga1 <- gateExtr(data,bead,DNA)
gateVis(data,bead,ga1)
framesID <- beadIdentification(frames[[1]],bead,chan,DNA,ga1)
f1.norm <- normalize.fs(framesID,bead,DNA,chan)



#Create the median values of each file for all the bead cannels and plot them
n.m <- NULL
m <- NULL
m.u <- NULL
e <- list()
e.u <- list()

#extract the normalized beads from the data
for(i in 1:length(f.norm)){
  v <- exprs(f.norm[[i]][,length(col)])
  e[[i]] <- subset(exprs(f.norm[[i]][,-c(1,2,5,8,10,11,12,13)]),as.logical(v))
  colnames(e[[i]]) <- bead
}

#calculate the median value after normalization for each file and each bead channel
for(i in 1:length(e)){
  n <- e[[i]]
  m <- rbind(m,apply(n,2,median))
}

#extract the unnormalized beads
for(i in 1:length(f)){
  v <- exprs(f[[i]][,length(col)])
  e.u[[i]] <- subset(exprs(f[[i]][,-c(1,2,5,8,10,11,12,13)]),as.logical(v))
  colnames(e.u[[i]]) <- bead
}
#calculate the median value before normalization for each file and each bead channel
for(i in 1:length(e.u)){
  n <- e.u[[i]]
  m.u <- rbind(m.u,apply(n,2,median))
}

par(mfrow=c(1,1))

matplot(m.u,col=c("blue","green","red","cyan","magenta"),type="l",cex=1,lty=1,ylab="Median Bead Intensity",
        ylim= c (5.5,10),xlab="file",main="Before Normalization")

matplot(m,col=c("blue","green","red","cyan","magenta"),type="l",cex=1,lty=1,ylab="Median Bead Intensity",
        ylim= c (5.5,10),xlab="file",main="After Normalization")





