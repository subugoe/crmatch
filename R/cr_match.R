#' Match free-form references to DOIs
#'
#' Retrieve DOIs by posting free-form references to Crossref.
#'
#' @param refs character, references to be matched.
#'
#' @examples \dontrun{
#' cr_match("Kleinbölting N, Huep G, Weisshaar B. Enhancing the GABI-Kat Arabidopsis thaliana T-DNA insertion mutant database by incorporating Araport11 annotation. Plant and Cell Physiology. 2017;58(1): e7.")
#' }
#' @export
cr_match <- function(refs = NULL) {
  if (length(refs) > 10) {
    # loop
    out <- data.frame()
    for(i in seq(1, length(refs), by = 10)) {
      tt <- refs[i:(i+9)]
      tt <- tt[!is.na(tt)]
      out_tmp <- cr_match_(refs = tt)
      out <- dplyr::bind_rows(out, out_tmp)
      out
    }
  } else {
    out <- cr_match_(refs)
  }
  return(dplyr::as_data_frame(out))
}

#' Post one request to match many free-form reference to DOIs
#'
#' In general, use \link{cr_match} instead. It calls this function,
#' posting free-form references to Crossref to retrieve DOIS for every match.
#'
#' @param refs character, references to be matched
#' @export

cr_match_ <- function(refs = NULL) {
  if(is.null(refs))
    stop("Nothing to match")
  refs <- as.list(refs)
  resp <- httr::POST(url = "http://search.crossref.org/",
                     path = "links",
                     body = refs,
                     encode = "json",
                     httr::accept_json(),
                     httr::content_type_json()
  )
  if (httr::http_type(resp) != "application/json") {
    stop("Ups, something went wrong, because API did not return json", call. = FALSE)
  }
  jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))$results %>%
    dplyr::as_data_frame()
}
