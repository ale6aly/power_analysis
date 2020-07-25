#------------------------------------------------------------------------------
# Code By: Fahad Almsned
# Affiliation: NINDS/NIH
#------------------------------------------------------------------------------

# main source -->https://www.statmethods.net/stats/power.html

library(dplyr)
library(pwr)

#-------------------------------------------------------------------------------
#  Sample Size vs. effect size (d)
#-------------------------------------------------------------------------------

#range of effect size (d)
d = seq(.1,.8,.01)
nd = length(d)

# power values
p <- seq(.1,.9,.1)
np <- length(p)

# obtain sample sizes
samsize <- array(numeric(nd*np), dim=c(nd,np))
for (i in 1:np){
  for (j in 1:nd){
    result <- pwr.t.test(n = NULL, d = d[j],sig.level = .05, power = p[i], type = "two.sample")
    samsize[j,i] <- ceiling(result$n)
  }
}


# set up graph
xrange <- range(d)
yrange <- round(range(samsize))
colors <- rainbow(length(p))
plot(xrange, yrange, type="n",
     xlab="Effect size (d)",
     ylab="Sample Size (n)" )

# add power curves
for (i in 1:np){
  lines(d, samsize[,i], type="l", lwd=2, col=colors[i])
}

# add annotation (grid lines, title, legend)
abline(v=0, h=seq(0,yrange[2],50), lty=2, col="grey89")
abline(h=0, v=seq(xrange[1],xrange[2],.02), lty=2,
       col="grey89")
title("Sample Size Estimation \n
  Sig=0.05 (Two-tailed)")
legend("topright", title="Power", as.character(p),
       fill=colors)

