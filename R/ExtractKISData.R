#' Extract data from KIS
#' @param var Variable to extract (plural?)
#' @param geoIdentifier Station identifier (, spatial point, spatial area ..., plural?)
#' @param period Xts
#' @return data.table
#' @export
KIS <- function(var, geoIdentifier, period) {
  assertChoice(var, "TG")
  assertChoice(geoIdentifier, "260_H")
  assertChoice(period, '2016')
  recipeName <- WriteKISRecipe(var, geoIdentifier, period)
  ExecuteKISRecipe(recipeName, period)
}

WriteKISRecipe <- function(var, geoIdentifier, period) {
  recipeName <- "KIStable.txt"
  recipe     <- 'recipe={"datasetserieselements":[{"datasetseries":"REH1","element":"TG","unit":"graad C"}],"datasetseriesnames":["REH1"],"datasourcecodes":["260_H"],"intervalids":[],"elementgroupnames":[],"unitsettings":[{"unit":"graad C","scale":"true","conversionfunction":"NONE"}],"starttime":"20160915_000000_000000","endtime":"20160916_000000_000000","maxresults":100,"countsettings":{"count":false,"period":"DAY","countconditionbyelement":[{"element":"TG","condition":"AMOUNT","value":null}]},"displaysettings":{"showMetaData":false,"sort":"DateStationTime"}}'
  writeLines(recipe, recipeName)
  return(recipeName)
}

ExecuteKISRecipe <- function(recipeName, period) {
  url <- 'http://bhlkisdev.knmi.nl:8080/kis/kis/download/table/20110401_000001/20110531_000000/CSV'
  destFile <- 'KIStable.csv'
  download.file(url, destfile, method = "wget",
                extra = '–header=“Content-Type:application/x-www-form-urlencoded” –post-file=“KIStabel.txt”')
  fread(destFile)
  #stop("ExecuteKISRecipe not implemented")
}

