#' Obtains ECWMF forecast from pluim
#'
#' @export
ObtainForecast <- function() {
  url <- "https://cdn.knmi.nl/knmi/json/page/weer/waarschuwingen_verwachtingen/ensemble/iPluim/260_99999.json"
  tmp <- fromJSON(url)
  tmp <- as.data.frame(tmp$series)
  tmp <- tmp[1, ]
  tmp <- as.data.frame(tmp$data)
  origin <- as.Date("1970-01-01")
  days_passed <- tmp[, 1] / 1000 /60 / 60 / 24
  tmp$date <- origin + days_passed
  as.data.table(tmp)[, .(date, tg = X2)]
}
