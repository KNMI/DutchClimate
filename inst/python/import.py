## python import data and plot

import rpy2.robjects as rpy
from rpy2.robjects.packages import importr
dc = importr("DutchClimate")

#kisdata = dc.KIS("TG", '260_a', '2015')

kisdata = rpy.r.rnorm(100)

rpy.r('hist(%s, xlab = "x", main = "hist(x)")' %kisdata.r_repr())


