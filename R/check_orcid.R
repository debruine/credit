#' Check validity of ORCiD
#'
#' @param orcid a 16-character ORCiD in bare or URL format
#'
#' @return a formatted 16-character ORCiD or FALSE
#' @export
#'
#' @examples
#' check_orcid("0000-0002-7523-5539")
#' check_orcid("0000-0002-0247-239X")
#' check_orcid("https://orcid.org/0000-0002-0247-239X")
#' check_orcid("0000-0002-0247-2394") # incorrect, return FALSE
check_orcid <- function(orcid) {
  baseDigits <- gsub("[^0-9X]", "", orcid)

  if (nchar(baseDigits) != 16) {
    if (sv_opts("verbose")) {
      warning("The ORCiD ", orcid, " is not valid.")
    }
    return(FALSE)
  }

  total <- 0
  for (i in 1:(nchar(baseDigits)-1)) {
    digit <- substr(baseDigits, i, i) %>% as.integer()
    total <- (total + digit) * 2
  }
  remainder <- total %% 11;
  result <- (12 - remainder) %% 11;
  result <- ifelse(result == 10, "X", result)

  if (result == substr(baseDigits, 16, 16)) {
    paste(substr(baseDigits, 1, 4),
          substr(baseDigits, 5, 8),
          substr(baseDigits, 9, 12),
          substr(baseDigits, 13, 16),
          sep = "-")
  } else {
    if (sv_opts("verbose")) {
      warning("The ORCiD ", orcid, " is not valid.")
    }
    return(FALSE)
  }
}



#' Get ORCiD from Name
#'
#' If there is more than one match for the name, the function will return a list of potential matches with more info. Set `info_from` to the indices to return. The default is 1:5 because the lookup can take a few seconds for each potential match, but you can set this to a larger number. If you set `info_from` to a single index, you will only get the ORCiD returned.
#'
#' @param family The family (last) name to search for
#' @param given An optional given (first) name to search for. Initials will be converted from, e.g., L M to L\* M\*
#' @param info_from if there is more than one match, which indices to get info from
#'
#' @return A matching ORCiD or a list of potential author matches
#' @export
#'
#' @examples
#' get_orcid("DeBruine", "Lisa")
#'
get_orcid <- function(family, given = "*", info_from = 1:5) {
  if (is.null(family) || trimws(family) == "") {
    stop("You must include a family name")
  }

  if (is.null(given) || trimws(given) == "") {
    given <- "*"
  }

  query <- "https://pub.orcid.org/v3.0/search/?q=family-name:%s+AND+given-names:%s"

  given2 <- given %>%
    trimws() %>%
    gsub("^(\\w)\\.?$", "\\1\\*", .) %>% # single initial
    gsub("^(.)\\.?\\s", "\\1\\* ", .) %>% # initial initial
    gsub("\\s(.)\\.?$", " \\1\\*", .) %>% # ending initial
    gsub("\\s(.)\\.?\\s", " \\1\\* ", .) %>% # internal initial
    utils::URLencode()

  family2 <- trimws(family) %>% utils::URLencode()
  url <- sprintf(query, family2, given2) %>% url("rb")
  on.exit(close(url))

  xml <- tryCatch(xml2::read_xml(url), error = function(e) {
    warning("You might not have an internet connection")
    return(list())
  })
  l <- xml2::as_list(xml)

  orcids <- sapply(l$search, function(res) {
    res$`orcid-identifier`$path
  }) %>% unlist() %>% unname()

  n <- length(orcids)
  if (n == 0) {
    message("No ORCID found for ", given, " ", family)
    return(NULL)
  } else if (n >= 1) {
    if (length(info_from) == 1 && info_from %in% 1:n) {
      return(orcids[info_from])
    }
    sprintf("%d ORCIDs found for %s %s",
            n, given, family) %>%
      message()
    if (!all(1:n %in% info_from)) {
      isect <- intersect(1:n, info_from)
      if (length(isect) == 0) {
        sprintf("No ORCiD for %s %s with index %d",
                given, family, info_from) %>%
          stop(call. = FALSE)
      }
      orcids <- orcids[isect]
      message("Retrieving info on ORCiDs ",
              paste(isect, collapse = ", "), "...")
    } else {
      message("Retrieving info...")
    }

    info <- lapply(orcids, orcid_info)
    return(info)
  }
}

#' Get Info from ORCiD
#'
#' Looks up info on https://pub.orcid.org and returns any info on the author name, location (country only), emails, URLs and keywords. This is useful for disambiguation.
#'
#' @param orcid a 16-character ORCiD in bare or URL format
#'
#' @return a list of info
#' @export
#'
orcid_info <- function(orcid) {
  orcid <- check_orcid(orcid)
  if (isFALSE(orcid)) { stop() }

  url <- sprintf("https://pub.orcid.org/v3.0/%s/person", orcid) %>% url("rb")
  on.exit(close(url))

  xml <- tryCatch(xml2::read_xml(url), error = function(e) {
    warning("You might not have an internet connection")
    return(list())
  })
  info <- xml2::as_list(xml)

  # function to handle duplicate list item names
  pluck_multi <- function(list, name, subname) {
    list[which(names(list) %in% name)] %>%
      sapply(`[[`, subname) %>%
      unlist() %>%
      unname()
  }

  # get info
  a <- author(
    surname = info$person$name$`family-name`[[1]],
    given = info$person$name$`given-names`[[1]],
    orcid = orcid,
    email = pluck_multi(info$person$emails, "email", "email"),
    country = pluck_multi(info$person$addresses, "address", "country"),
    url = pluck_multi(info$person$`researcher-urls`, "researcher-url", "url"),
    keywords = pluck_multi(info$person$keywords, "keyword", "content")
  )

  # remove NULL items
  a <- a[!sapply(a, is.null)]
  class(a) <- c("scivrs_author", "list")
  a
}



