## python import data and plot

import rpy2.robjects as rpy
from rpy2.robjects.packages import importr
from rpy2.robjects import pandas2ri
import matplotlib.pyplot as plt
import numpy as np
pandas2ri.activate()
dc = importr("DutchClimate")

kisdata = pandas2ri.ri2py(dc.KIS("TG", '260_a', '2015'))

# aggregated = python.wrapp(dc.aggregate(kisdata))
#
# plotfile = python(dc.plotdata(aggregated))
# rpy.r('hist(%s, xlab = "x", main = "hist(x)")' %kisdata.r_repr())

# plt.hist(kisdata['var'])
plt.hist(np.asarray(kisdata['var']))
plt.show()



