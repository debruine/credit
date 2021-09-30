make_orcid_match_boxes <- function(matches) {
  lapply(seq_along(matches), function(i) {
    match <- matches[[i]]

    info <- list()
    info$name <- tags$li(glue("{match$name$given} {match$name$surname}"))

    if (!is.null(match$url)) {
      info$url <- lapply(match$url, function(url) {
        tags$li(tags$a(href =url, url, target = "_blank"))
      })
    }

    if (!is.null(match$email)) {
      info$email <- tags$li(match$email)
    }

    if (!is.null(match$country)) {
      info$country <- tags$li(match$country)
    }

    if (!is.null(match$keywords)) {
      kw <- paste(match$keywords, collapse = ", ")
      info$country <- tags$li(kw)
    }

    title <- tagList(
      tags$a(href = glue("https://orcid.org/{match$orcid}"),
             match$orcid, target = "_blank"),
      tags$button("Add", id = i)
    )

    box(width = 12,
        title = title,
        tags$ul(info)
    )
  })
}
