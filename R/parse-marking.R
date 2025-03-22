# https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Marking-text
#' @export
rd_parse.tag_emph <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    paste(parser$emph(text), collapse = "\n")
}

#' @export
rd_parse.tag_bold <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    paste(parser$bold(text), collapse = "\n")
}

#' @export
rd_parse.tag_strong <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    paste(parser$strong(text), collapse = "\n")
}

#' @export
rd_parse.tag_sQuote <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    paste(parser$sQuote(text), collapse = "\n")
}

#' @export
rd_parse.tag_dQuote <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    paste(parser$dQuote(text), collapse = "\n")
}

#' @export
rd_parse.tag_code <- function(docs, ..., autolink = TRUE,
                              parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., autolink = autolink, parser = parser)
    if (autolink) {
        href <- downlit::autolink_url(text)
        if (!is.na(href)) {
            text <- parser$href(parser$code(text), href = href)
            return(paste(text, collapse = "\n"))
        }
    }
    paste(parser$code(text), collapse = "\n")
}

#' @export
rd_parse.tag_preformatted <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    paste(parser$codeblock(text), collapse = "\n")
}

#' @export
rd_parse.tag_kbd <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    paste(parser$kbd(text), collapse = "\n")
}

#' @export
rd_parse.tag_samp <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    paste(parser$samp(text), collapse = "\n")
}

#' @export
rd_parse.tag_verb <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    paste(parser$verb(text), collapse = "\n")
}

#' @export
rd_parse.tag_pkg <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    paste(parser$pkg(text), collapse = "\n")
}

#' @export
rd_parse.tag_file <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    paste(parser$file(text), collapse = "\n")
}

#' @export
rd_parse.tag_email <- function(docs, ..., parser = rd_parser()) {
    if (length(docs) != 1L) {
        rd_stop_bad_tag("email", "empty {}")
    }
    text <- parser$email(rd_flatten_text(.subset2(docs, 1L)))
    paste(text, collapse = "\n")
}

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
    text <- parser$href(
        rd_flatten_text(.subset2(docs, 2L), ..., parser = parser),
        rd_flatten_text(.subset2(docs, 1L), ..., parser = parser)
    )
    paste(text, collapse = "\n")
}

#' @export
rd_parse.tag_var <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    paste(parser$var(text), collapse = "\n")
}

#' @export
rd_parse.tag_env <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    paste(parser$env(text), collapse = "\n")
}

#' @export
rd_parse.tag_option <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    paste(parser$option(text), collapse = "\n")
}

#' @export
rd_parse.tag_command <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    paste(parser$command(text), collapse = "\n")
}

#' @export
rd_parse.tag_dfn <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    paste(parser$dfn(text), collapse = "\n")
}

#' @export
rd_parse.tag_cite <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    paste(parser$cite(text), collapse = "\n")
}

#' @export
rd_parse.tag_acronym <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    paste(parser$acronym(text), collapse = "\n")
}

#' @export
rd_parse.tag_abbr <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    paste(parser$abbr(text), collapse = "\n")
}
