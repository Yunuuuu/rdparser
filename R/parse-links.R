# If single, need to look up alias to find file name and package
#' @export
rd_parse.tag_link <- function(docs, ..., autolink = TRUE,
                              parser = rd_parser()) {
    # \link[opt]{in_braces}
    in_braces <- as.character(docs)
    if (autolink) {
        opt <- attr(docs, "Rd_option")
        if (is.null(opt)) {
            # \link{topic}
            href <- downlit::href_topic(in_braces)
        } else if (substr(opt, 1L, 1L) == "=") {
            # \link[=dest]{name}
            href <- downlit::href_topic(substr(opt, 2L, nchar(opt)))
        } else {
            match <- regexec("^([^:]+)(?:|:(.*))$", opt)
            parts <- regmatches(opt, match)[[1L]][-1L]
            if (parts[[2L]] == "") {
                # \link[pkg]{foo}
                href <- downlit::href_topic(in_braces, opt)
            } else {
                # \link[pkg:bar]{foo}
                href <- downlit::href_topic(parts[[2]], parts[[1]])
            }
        }
        if (is.na(href)) href <- NULL
    } else {
        href <- NULL
    }
    rd_postparser(parser$href(in_braces, href = href))
}

#' @export
rd_parse.tag_linkS4class <- function(docs, ..., autolink = TRUE,
                                     parser = rd_parser()) {
    if (length(docs) != 1L) {
        rd_stop_bad_tag("linkS4class")
    }
    text <- rd_flatten_text(
        .subset2(docs, 1L), ...,
        autolink = autolink, parser = parser
    )
    if (autolink) {
        href <- downlit::href_topic(paste0(text, "-class"))
        if (is.na(href)) href <- NULL
    } else {
        href <- NULL
    }
    rd_postparser(parser$href(text, href))
}
