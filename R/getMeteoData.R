#' Load meteo data from xlsx files
#'
#' This function loads meteo data.
#' @importFrom magrittr "%>%"
#' @export
getMeteoData <- function(dataPath, fileName) {
  sheetNames <- openxlsx::getSheetNames(dataPath)
  data <- list()
  for(sheetName in sheetNames[-1]) {
    colName <- stringr::str_replace_all(sheetName, " ", ".")
    data[[colName]] <- openxlsx::read.xlsx(dataPath, sheetName, startRow = 4) %>%
      tidyr::gather("den", value, c(-rok, -měsíc)) %>%
      dplyr::mutate(den = stringr::str_replace(den, "\\.", ""),
             den = stringr::str_pad(den, 2, pad="0"),
             date = sprintf("%s-%s-%s", rok, měsíc, den)) %>%
      dplyr::mutate("{colName}" := value) %>%
      dplyr::select(-den, -měsíc, -rok, -value)
  }
  result <- data[[1]]
  for(sheet in data[-1]) {
    result <- dplyr::full_join(result, sheet, by=dplyr::join_by(date))
  }
  colnames(result) <- c('date', names(data))
  file <- basename(fileName)
  return(result %>%
           dplyr::mutate(file = {file}) %>%
           dplyr::select(date, file, dplyr::everything()))
}
