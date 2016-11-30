#' Extract data from KIS
#' @param var Variable to extract (plural?)
#' @param geoIdentifier Station identifier (, spatial point, spatial area ..., plural?)
#' @param period Either numeric, timeBased or ISO-8601 style (see \code{\link[xts]{.subset.xts}})
#' @return data.table
#' @export
KIS <- function(var, geoIdentifier, period) {
  InternalOnly()
  flog.debug("Started downloading data from KIS")
  flog.debug("var={%s}", paste(var))
  flog.debug("geoIdentifier has name={%s} and class={%s}",
             paste(substitute(geoIdentifier)),
             paste(class(geoIdentifier)[1]))
  flog.debug("period={%s}", paste(period))
  assertChoice(var, "TG")
  assertChoice(geoIdentifier, c("260_H", "310_H"))
  tryCatch(xts::.parseISO8601(period),
           warning = function(cond) {
             stop("period does not seem to be suitable")
           },
           error = function(cond) {
             stop("period does not seem to be suitable")
           })
  locationID <- geoIdentifier
  recipeName <- WriteKISRecipe(var, locationID, period)
  result <- ExecuteKISRecipe(recipeName, period)
  return(result) #FIX: Timezone is 240000 this day or the next?
}

#' Python test template
#' @param var variable
#' @param geoIdentifier location
#' @param period period
#' @export
KIStemplate <- function(var, geoIdentifier, period) {
  data.table(date = as.Date(0 : 19, origin = as.Date("2015-01-01")),
             loc  = geoIdentifier,
             var = rnorm(20))
}

WriteKISRecipe <- function(var, locationID, period) {
  # period is not yet used in the recipe
  # max results does not seem to have
  recipeName <- "KIStable.txt"
  recipe     <- 'recipe=' %>%
    paste0('{"datasetserieselements":[{"datasetseries":"REH1","element":"TG","unit":"graad C"}],') %>%
    paste0('"datasetseriesnames":["REH1"],') %>%
    paste0('"datasourcecodes":["', locationID, '"],') %>%
    paste0('"intervalids":[],') %>%
    paste0('"elementgroupnames":[],') %>%
    paste0('"unitsettings":[{"unit":"graad C","scale":"true","conversionfunction":"NONE"}],') %>%
    paste0('"starttime":"20160115_000000_000000",') %>%
    paste0('"endtime":"20160916_000000_000000",') %>%
    paste0('"maxresults":1000,') %>%
    paste0('"countsettings":{"count":false,"period":"DAY","countconditionbyelement":[{"element":"TG","condition":"AMOUNT","value":null}]},') %>%
    paste0('"displaysettings":{"showMetaData":false,"sort":"DateStationTime"}}') %>%
    str_replace_all('%', '%25')
  writeLines(recipe, recipeName)
  return(recipeName)
}

CorrectDataFormat <- function(xtsObject) {
  format(xtsObject, "%Y%m%d_%H%M%S")
}

ExecuteKISRecipe <- function(recipeName, period) {
  parsedPeriod <- .parseISO8601(period)
  url <- 'http://kisapp.knmi.nl:8080/servlet/download/table/'
  url <- paste0(url, CorrectDataFormat(parsedPeriod$first.time+1),
                '/', CorrectDataFormat(parsedPeriod$last.time+1), '/CSV')
  destFile <- 'KIStable.csv'

  flog.info("Start data download.")
  download.file(url, destFile, method = "wget", quiet = T,
                extra = c('--header="Content-Type:application/x-www-form-urlencoded"',
                          paste0('--post-file="', recipeName, '"')))
  flog.info("Download finished.")

  result <- tryCatch(fread(destFile),
                     warning = function(cond) {
                       message("URL caused a warning")
                       return(NULL)
                     },
                     error = function(cond) {
                       message("Download failed")
                       return(NULL)
                       },
                     finally = {
                       file.remove(recipeName)
                       file.remove(destFile)
                     })
  return(result)
}


