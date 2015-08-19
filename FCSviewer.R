x <- read.FCS("BM3_1.fcs")

e <- exprs(x)
head(e[,-c(1:2)])

asinh.scale <- 5
et <- e
et[,-c(1:2)] <- asinh(et[,-c(1:2)] / asinh.scale)

p <- colorRampPalette(c("blue", "orange", "red"),space = "Lab")
   smoothScatter( e[,cl], asinh(e[,dc[i]]), nrpoints=0,
                  xlab="Event length", ylab=dc[i], colramp=p)
   points( e[k,cl], asinh(e[k,dc[i]]), pch=".", col="white")


nm <- as.character(x@parameters@data$desc)
dna <- grep("DNA",nm)
bead <- grep("bead",nm)

qs <- round(t(apply(et,2,quantile, p=c(.001,.999) )),3)

s <- sample( nrow(et), 5e4)

pdf("a.pdf",w=(length(dna)+1)*5,h=5);
par(mfrow=c(1,3))
smoothScatter( et[s,"Time"], jitter(et[s,"Event_length"],amount=.3),
              xlab="Time", ylab="Event_length", nrpoints=0, colramp=p);
for(i in dna)
 smoothScatter( jitter(et[s,"Event_length"], amount=.3), et[s,i],
                pch=".", nrpoints=0, colramp=p, ylim=qs[i,],
                xlab="Event_length", ylab=paste0(colnames(et)[i], " // ", nm[i]))
dev.off()
