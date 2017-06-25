library(jsonlite)

# https://cdn.knmi.nl/knmi/json/page/weer/waarschuwingen_verwachtingen/ensemble/iPluim/260_99999.json

tmp <- fromJSON("https://cdn.knmi.nl/knmi/json/page/weer/waarschuwingen_verwachtingen/ensemble/iPluim/260_99999.json")

names(tmp)

library(tidyr)
library(data.table)

tmp2 <- as.data.frame(tmp$series)
tmp3 <- tmp2[1, ]
tmp4 <- as.data.frame(tmp3$data)
str(tmp2[1, ][["data"]])



library(lubridate)




# we need to know the origin, and make it into a Date object
origin <- as.Date("1970-01-01")

tmp4$date <-  origin + days_passed

library(jsonlite)
library(ggplot2)
library(data.table)
```

# ```{r, echo=TRUE}
# tmp <- fromJSON("https://cdn.knmi.nl/knmi/json/page/weer/waarschuwingen_verwachtingen/ensemble/iPluim/260_99999.json")
# ```
#
# ```{r, echo=FALSE}
# tmp <- as.data.table(tmp[["series"]][1, "data"])
# setnames(tmp, c("date", "verwachting"))
# tmp[, date := as.POSIXct(date/1000, origin = "1970-01-01", tz = "UTC")]
# tmp[, hour := factor(hour(date), ordered = TRUE)]
#
# ggplot(tmp, aes(date, verwachting, col = rev(hour))) +
#   geom_point(show.legend = FALSE) +
#   xlab("") + ylab("tg verwachting")
