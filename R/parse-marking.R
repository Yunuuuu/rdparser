# https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Marking-text
#' @export
rd_parse.tag_emph <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    rd_postparser(parser$emph(text))
}

#' @export
rd_parse.tag_bold <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    rd_postparser(parser$bold(text))
}

#' @export
rd_parse.tag_strong <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    rd_postparser(parser$strong(text))
}

#' @export
rd_parse.tag_sQuote <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    rd_postparser(parser$sQuote(text))
}

#' @export
rd_parse.tag_dQuote <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    rd_postparser(parser$dQuote(text))
}

#' @export
rd_parse.tag_code <- function(docs, ..., autolink = TRUE,
                              parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., autolink = autolink, parser = parser)
    if (autolink) {
        href <- downlit::autolink_url(text)
        if (!is.na(href)) {
            text <- rd_postparser(parser$code(text))
            return(rd_postparser(parser$href(text, href = href)))
        }
    }
    rd_postparser(parser$code(text))
}

#' @export
rd_parse.tag_preformatted <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    rd_postparser(parser$codeblock(text))
}

#' @export
rd_parse.tag_kbd <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    rd_postparser(parser$kbd(text))
}

#' @export
rd_parse.tag_samp <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    rd_postparser(parser$samp(text))
}

#' @export
rd_parse.tag_verb <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    rd_postparser(parser$verb(text))
}

#' @export
rd_parse.tag_pkg <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    rd_postparser(parser$pkg(text))
}

#' @export
rd_parse.tag_file <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    rd_postparser(parser$file(text))
}

#' @export
rd_parse.tag_email <- function(docs, ..., parser = rd_parser()) {
    if (length(docs) != 1L) {
        rd_stop_bad_tag("email", "empty {}")
    }
    text <- rd_flatten_text(.subset2(docs, 1L))
    rd_postparser(parser$email(text))
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
    rd_postparser(parser$href(text, text))
}

#' @export
rd_parse.tag_href <- function(docs, ..., parser = rd_parser()) {
    text <- parser$href(
        rd_flatten_text(.subset2(docs, 2L), ..., parser = parser),
        rd_flatten_text(.subset2(docs, 1L), ..., parser = parser)
    )
    rd_postparser(text)
}

#' @export
rd_parse.tag_var <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    rd_postparser(parser$var(text))
}

#' @export
rd_parse.tag_env <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    rd_postparser(parser$env(text))
}

#' @export
rd_parse.tag_option <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    rd_postparser(parser$option(text))
}

#' @export
rd_parse.tag_command <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    rd_postparser(parser$command(text))
}

#' @export
rd_parse.tag_dfn <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    rd_postparser(parser$dfn(text))
}

#' @export
rd_parse.tag_cite <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    rd_postparser(parser$cite(text))
}

#' @export
rd_parse.tag_acronym <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    rd_postparser(parser$acronym(text))
}

#' @export
rd_parse.tag_abbr <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    rd_postparser(parser$abbr(text))
}
