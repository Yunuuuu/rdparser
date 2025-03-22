#' @export
rd_parse.tag_examples <- function(docs, ..., level = 1L, parser = rd_parser()) {
    level <- level + 1L
    out <- rd_flatten_para(docs, ..., level = level + 1L, parser = parser)
    out <- rd_trim_newline(out)
    out <- parser$rd_preparse(out)
    out <- parser$rd_examples(out, level = level)
    paste(parser$rd_postparse(out), "\n")
}
