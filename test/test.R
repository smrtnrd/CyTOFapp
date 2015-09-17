 library(flowCore)
library(ggplot2)
x <- read.FCS("data/fcs/ALL_patient_a_panel_1_unnormalized.fcs")
e <- exprs(x)
y <- asinh( e[,10] / 5 )
s <- sample(nrow(e),10000)
smoothScatter( e[s,2], y[s] )

x <- asinh(e[s,7]/5)
y <- asinh(e[s,8]/5)
df <- data.frame(x = x, y =  y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))))


p <- ggplot(df, aes(x=x, y=y)) +
  geom_jitter(position = "jitter", alpha = 0.1, ) +
  theme_bw()
print(p)



df <- data.frame(x = rnorm(5000),y=rnorm(5000))
ggplot(df,aes(x=x,y=y)) + stat_binhex()

gate()