#' Extract data from KIS
#' @param var Variable to extract (plural?)
#' @param geoIdentifier Station identifier (, spatial point, spatial area ..., plural?)
#' @param period Either numeric, timeBased or ISO-8601 style (see \code{\link[xts]{.subset.xts}})
#' @return data.table
#' @export
KIS <- function(var, geoIdentifier, period) {
  assertChoice(var, "TG")
  assertChoice(geoIdentifier, c("260_H", "310_H"))
  tryCatch(xts::.parseISO8601(period),
           warning = function(cond) {
             stop("period does not seem to be suitable")
           },
           error = function(cond) {
             stop("period does not seem to be suitable")
           })
  recipeName <- WriteKISRecipe(var, geoIdentifier, period)
  result <- ExecuteKISRecipe(recipeName, period)
  return(result) #FIX: Timezone is 240000 this day or the next?
}

WriteKISRecipe <- function(var, geoIdentifier, period) {
  # period is not yet used in the recipe
  # max results does not seem to have
  recipeName <- "KIStable.txt"
  recipe     <- 'recipe='
  recipe     <- paste0(recipe, '{"datasetserieselements":[{"datasetseries":"REH1","element":"TG","unit":"graad C"}],')
  recipe     <- paste0(recipe, '"datasetseriesnames":["REH1"],')
  recipe     <- paste0(recipe, '"datasourcecodes":["', geoIdentifier, '"],')
  recipe     <- paste0(recipe, '"intervalids":[],"elementgroupnames":[],"unitsettings":[{"unit":"graad C","scale":"true","conversionfunction":"NONE"}],"starttime":"20160115_000000_000000","endtime":"20160916_000000_000000","maxresults":1000,"countsettings":{"count":false,"period":"DAY","countconditionbyelement":[{"element":"TG","condition":"AMOUNT","value":null}]},"displaysettings":{"showMetaData":false,"sort":"DateStationTime"}}')
  recipe     <- str_replace_all(recipe, '%', '%25')
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


