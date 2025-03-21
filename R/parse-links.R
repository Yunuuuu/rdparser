#' @export
rd_parse.tag_url <- function(docs, ..., parser = rd_parser()) {
    if (length(docs) != 1L) {
        if (length(docs) == 0L) {
            msg <- "Check for empty \\url{{}} tags."
        } else {
            msg <- "This may be caused by a \\url tag that spans a line break."
        }
        rd_stop_bad_tag("url", msg)
    }
    text <- rd_flatten_text(.subset2(docs, 1L), ..., parser = parser)
    paste(parser$href(text, text), collapse = "\n")
}

#' @export
rd_parse.tag_href <- function(docs, ..., parser = rd_parser()) {
    out <- parser$href(
        rd_flatten_text(.subset2(docs, 2L), ..., parser = parser),
        rd_flatten_text(.subset2(docs, 1L), ..., parser = parser)
    )
    paste(out, collapse = "\n")
}

#' @export
rd_parse.tag_email <- function(docs, ..., parser = rd_parser()) {
    if (length(docs) != 1L) {
        rd_stop_bad_tag("email", "empty {}")
    }
    out <- parser$email(rd_flatten_text(.subset2(docs, 1L)))
    paste(out, collapse = "\n")
}

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
    paste(parser$href(in_braces, href = href), collapse = "\n")
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
    paste(parser$href(text, href), collapse = "\n")
}

# Figures -----------------------------------------------------------------
# Figures
#' @export
rd_parse.tag_figure <- function(docs, ..., parser = rd_parser()) {
    n <- length(docs)
    path <- rd_flatten_text(.subset2(docs, 1L))
    options <- alt <- NULL
    if (n == 2L) {
        opt <- paste(
            trimws(rd_flatten_text(.subset2(docs, 2L))),
            collapse = " "
        )
        if (substr(opt, 1L, 9L) == "options: ") {
            options <- substr(opt, 9L, nchar(opt))
        } else {
            alt <- opt
        }
    }
    paste(parser$image(path, alt, options), collapse = "\n")
}
