#' Remove diacritics
#'
#' This function removes diacritics.
#' @importFrom magrittr "%>%"
#' @export
accentless <- function( s ) {
  chartr(
    "ěščřžýáíéúůťóďňĚŠČŘŽÝÁÍÉÚŮŤÓĎŇ",
    "escrzyaieuutodnESCRZYAIEUUTODN",
    s );
}
