# Marking text --------------------------------------------------------------
# https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Marking-text
#' @export
rd_parse.tag_emph <- function(docs, ..., parser = rd_parser()) {
    parser$emph(rd_flatten_text(docs, ..., parser = parser))
}

#' @export
rd_parse.tag_strong <- function(docs, ..., parser = rd_parser()) {
    parser$strong(rd_flatten_text(docs, ..., parser = parser))
}

#' @export
rd_parse.tag_bold <- function(docs, ..., parser = rd_parser()) {
    parser$bold(rd_flatten_text(docs, ..., parser = parser))
}

#' @export
rd_parse.tag_dQuote <- function(docs, ..., parser = rd_parser()) {
    parser$dQuote(rd_flatten_text(docs, ..., parser = parser))
}

#' @export
rd_parse.tag_sQuote <- function(docs, ..., parser = rd_parser()) {
    parser$sQuote(rd_flatten_text(docs, ..., parser = parser))
}

#' @export
rd_parse.tag_code <- function(docs, ..., autolink = TRUE,
                              parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., autolink = autolink, parser = parser)
    if (autolink) {
        href <- downlit::autolink_url(text)
        if (!is.na(href)) {
            return(parser$href(parser$code(text), href = href))
        }
    }
    parser$code(text)
}

#' @export
rd_parse.tag_kbd <- function(docs, ..., parser = rd_parser()) {
    parser$kbd(rd_flatten_text(docs, ..., parser = parser))
}

#' @export
rd_parse.tag_samp <- function(docs, ..., parser = rd_parser()) {
    parser$samp(rd_flatten_text(docs, ..., parser = parser))
}

#' @export
rd_parse.tag_verb <- function(docs, ..., parser = rd_parser()) {
    parser$verb(rd_flatten_text(docs, ..., parser = parser))
}

#' @export
rd_parse.tag_pkg <- function(docs, ..., parser = rd_parser()) {
    parser$pkg(rd_flatten_text(docs, ..., parser = parser))
}

#' @export
rd_parse.tag_file <- function(docs, ..., parser = rd_parser()) {
    parser$file(rd_flatten_text(docs, ..., parser = parser))
}

#' @export
rd_parse.tag_var <- function(docs, ..., parser = rd_parser()) {
    parser$var(rd_flatten_text(docs, ..., parser = parser))
}

#' @export
rd_parse.tag_env <- function(docs, ..., parser = rd_parser()) {
    parser$env(rd_flatten_text(docs, ..., parser = parser))
}

#' @export
rd_parse.tag_option <- function(docs, ..., parser = rd_parser()) {
    parser$option(rd_flatten_text(docs, ..., parser = parser))
}

#' @export
rd_parse.tag_command <- function(docs, ..., parser = rd_parser()) {
    parser$command(rd_flatten_text(docs, ..., parser = parser))
}

#' @export
rd_parse.tag_dfn <- function(docs, ..., parser = rd_parser()) {
    parser$dfn(rd_flatten_text(docs, ..., parser = parser))
}

#' @export
rd_parse.tag_cite <- function(docs, ..., parser = rd_parser()) {
    parser$cite(rd_flatten_text(docs, ..., parser = parser))
}

#' @export
rd_parse.tag_acronym <- function(docs, ..., parser = rd_parser()) {
    parser$acronym(rd_flatten_text(docs, ..., parser = parser))
}

#' @export
rd_parse.tag_abbr <- function(docs, ..., parser = rd_parser()) {
    parser$abbr(rd_flatten_text(docs, ..., parser = parser))
}

#' @export
rd_parse.tag_out <- function(docs, ..., parser = rd_parser()) {
    parser$out(rd_flatten_text(docs, ..., parser = parser))
}
