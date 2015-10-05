
#gateExtr --> User define the gate in which the beads are contained. Just defined for a flow frame

gateExtr <- function(data,beads,DNA){
  #data--> FlowFrame
  #beads--> character vector with the names of the bead channel
  #DNA --> String with name of DNA channel

  #function produces plots for each bead channel against DNA. The user can then choose the border for the bead gates.
  #Store the points from the function read integer in this gate list
  gate <- list()
  col <- colnames(data)

  #function to ask for user input
  readinteger <- function(DNA)
  {

    print("Select per mouseclick the starting cutoff value where the intensity value of the beads and DNA are set")
    p1 <- locator(1)
    print("Select per mouseclick the ending cutoff value where the intensity value of the beads and DNA are set")
    p2 <- locator(1)
    xs <- c(p1$x,p2$x)
    ys <- c(p1$y,p2$y)
    p <- c(sort(xs), sort(ys))

  }

  #Transformation of the data

  asinhTrans <- arcsinhTransform(transformationId="arcsinh-transformation", a=1, b=1, c=1)
  t <- transformList(c(beads,DNA),asinhTrans)

  data <- transform(data,t)

  #produce the plots
  for(j in 1:length(beads)){

    temp.b <- exprs(data[,which(col==beads[j])])
    temp.DNA <- exprs(data[,which(col==DNA)])

    #sample if the number of events is very high and produce the plot with the sampled points
    s <- sample(nrow(data), min(50000, nrow(data)))

    plot(temp.b[s,1],temp.DNA[s,1],xlab = paste(beads[j], "bead", j, sep="/"),pch = 19,cex = .2,
         ylab=paste(DNA, "DNA", sep="/"),main = "Bead Identification")


    #After each plot the user define the gates and just the events which are contained in every gate are identified as beadevents
    value <- readinteger(temp.DNA)

    val <- matrix(c(value[1],value[2], value[3],value[4]),nrow = 2)
    colnames(val) <- c(beads[j],DNA)
    gate[j] <- rectangleGate(filterId = "Fluorescence Region",val)


  }
  return(gate)
}

#Function for visualization of the different gates

#This function give you the possibility to look at your gates

gateVis <- function(data,beads,gate,n = 1){
  #data --> FlowFrame
  #beads --> vector with the altered bead channel names
  #gate--> result of the function gateExtr

  l <- length(data)
  memory.size(max=F)
  if(l!=1){
    data <- data[[n]]
    gate <- gate[[n]]
  }
  pl <- NULL

  #Transformation of the data
  asinhTrans <- arcsinhTransform(transformationId="arcsinh-transformation", a=1, b=1, c=1)
  t <- transformList(c(bead,DNA),asinhTrans)

  data <- transform(data,t)


  #produce the plots
  for(j in 1:length(beads)){

    #sample if the number of events is very high and produce the plot with the sampled points
    s <- sample(nrow(data), min(50000, nrow(data)))

    p = paste0(DNA," ~ ", beads[j])
    pl[[j]] <- xyplot(as.formula(p),data = data[s,], xlab = paste(beads[j], "bead", j, sep="/"),ylab = "DNA",main = "Bead Identification",
                      filter = gate[[j]],checkName = F,smooth = F)

  }
  multiplot(pl) #--> function downloaded but works just with ggplot...
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#Function beadIdentification transform the data and indtroduce a column with the identification for the beads

beadIdentification <- function(data, beads,chan,DNA,gate){

  #in this function the data is arcsinh transformed and one column called bead is added for bead identification
  #data --> A flowFrame
  #beads --> Character vector with the names for the beads
  #DNA --> String with the name for the DNA channel
  #chan--> all the channels used for downstream analysis
  #gate --> list with the gate information for the data to identify the beads

  asinhTrans <- arcsinhTransform(transformationId="arcsinh-transformation", a=1, b=1, c=1)
  t <- transformList(c(beads,DNA,chan),asinhTrans)
  col <- colnames(data)

  data <- transform(data,t)
  sub.data <- data
  g <- NULL

  IDvec <- function(data, beadData){
    #data-->flowFrame
    #beadData -->Subset of the FlowFrame with just the bead events
    col <- colnames(data)
    td <- exprs(data[,which(col=="Time")])
    tb <- exprs(beadData[, which(col=="Time")])

    #v is the 0/1 vector which shows a 1 if an event was identified as a bead event and shows a 0 if it's not a bead event
    as.integer(td %in% tb)
  }

  g <- gate[[1]]
  for (j in 2:length(gate)){
      g <- g & gate[[j]]
  }

  col <- c(col,"beads")
  filter.res <- filter(data,g)
  sub.data <- Subset(data,filter.res)
  vec <- as.integer(IDvec(data,sub.data))
  data <- cbind2(exprs(data),vec)
  colnames(data) <- col

  data <- flowFrame(data)

}


normalize.fs <- function(data,beads, DNA,chan){

  #data --> either flowCore or FlowSet object
  #DNA --> selected DNA channel
  #beads-->character vector with the bead channels
  #chan --> selected channels for normalization

  n <- length(data)
  fs <- data

  norm.dat <- list()
  g.m <- matrix(0,nrow=length(beads),ncol=n)
  s.bead.m <- list()
  time.beads <- list()
  time.tot <- list()


  #do the regression
  slope.extr <- function(data,g.m){
    slopes <- rep(NA,length(data[,1]))
    for(i in 1:length(data[,1])){
      slope <- lm(g.m~0+data[i,])$coefficient
      slopes[i] <- slope
    }
    return(slopes)
  }

  for(i in 1:n){
    if(n != 1){
      fs <- data[[i]]
    }
    col <- colnames(fs)
    if(col[length(col)]!="beads"){
      g <- gateExtr(fs,bead,DNA,chan)
      fs <- beadIdentification(data,bead,chan,DNA,g)
      col <- colnames(fs)
    }
    fs <- exprs(fs)
    norm.dat[[i]] <- fs

    #subset the data according to the beads
    b <- as.logical(fs[,length(col)])
    e <- subset(fs,b)

    #smooth the beads
    bead.m <- e[,col %in% bead]
    options(warn = -1)
    s.bead.m[[i]] <- apply(bead.m,2,function(x)runmed(x,k = 501,endrule="constant"))
    options(warn = 0)
    g.mean <- apply(s.bead.m[[i]], 2, mean)
    g.m[,i] <- g.mean

    time.beads[[i]] <- e[,which(col=="Time")]
    time.tot[[i]] <- fs[,which(col=="Time")]
  }
  #calculate the global mean for the regression
  g.m <- apply(g.m,1,mean)

  for(i in 1:n){
    s<- slope.extr(s.bead.m[[i]],g.m)

    #make a linear interpolation to apply the function to the whole dataset
    inter.slope  <- approx(time.beads[[i]],s,time.tot[[i]],yleft = s[1] ,yright = s[length(s)])

    ch.n <- c(which(col %in% chan == T))

    for(j in 1:length(ch.n)){
      norm.dat[[i]][,ch.n[j]] <- norm.dat[[i]][,ch.n[j]]*inter.slope$y
    }
  }
  if(length(norm.dat) == 1){
    norm.dat=norm.dat[[1]]
    norm.dat <- flowFrame(norm.dat)
  }
  else{
    norm.dat <- lapply(norm.dat,flowFrame)
      }
  return(norm.dat)
}
