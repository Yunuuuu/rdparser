#' Rd document parser in pandoc markdown style
#'
#' A [`RdParser`] generator for pandoc markdown style.
#'
#' @importFrom R6 R6Class
#' @export
#' @keywords internal
RdParserMarkdown <- R6::R6Class(
    "RdParserMarkdown",
    inherit = RdParser,
    public = list(
        header = function(text, level) {
            sprintf("%s %s", strrep("#", level), text)
        },
        codeblock = function(text, lang = NULL) {
            c(sprintf("```%s", lang %||% ""), text, "```")
        },
        comment = function(text) sprintf("<!-- %s -->", text),
        ol = function(texts) md_list(seq_along(texts), texts, sep = ". "),
        ul = function(texts) md_list("-", texts, sep = " "),
        dl = function(terms, descriptions) {
            md_list(terms, descriptions, sep = ": ")
        },
        tabular = function(table, align) {
            # here we using `pipe_tables`
            # Rd document has no header concept
            # | |  |
            # |-|--|
            # |1|2 |
            # |3|4 |
            header <- rep_len("    ", length(align))
            align <- ifelse(
                align == "left",
                ":----",
                ifelse(align == "center", ":----:", "----:")
            )
            dims <- dim(table)

            # table cell shouldn't contain next line character
            table <- trimws(gsub("\\r?\\n", "", table))
            dim(table) <- dims
            table <- rbind(header, align, table)

            # ensure the text is well formated
            for (i in seq_len(ncol(table))) {
                table[, i] <- format(
                    table[, i, drop = TRUE],
                    justify = "centre"
                )
            }
            apply(table, 1L, function(row) {
                paste0(paste0("|", row, collapse = ""), "|")
            }, simplify = TRUE)
        },
        href = function(text, href = NULL) {
            if (is.null(href)) {
                text
            } else {
                href <- utils::URLencode(href)
                sprintf("[%s](%s)", text, href)
            }
        },
        image = function(src, alt = NULL, options = NULL) {
            sprintf("![%s](%s)", alt %||% "", src)
        },
        emph = function(text) sprintf("*%s*", text),
        bold = function(text) sprintf("**%s**", text),
        strong = function(text) sprintf("***%s***", text),
        code = function(text) sprintf("`%s`", text),
        eqn = function(text) sprintf("$%s$", text),
        deqn = function(text) sprintf("$$%s$$", text),
        cr = function() "\\\n", # break a line
        showif = function(text) any(text == "html")
    )
)

md_list <- function(terms, descriptions, sep) {
    # 1. item 1
    #    text for item 1
    # 2. item 2
    contents <- strsplit(descriptions, "\\n")
    header <- character(sum(lengths(contents)))
    index <- c(0L, lengths(contents)[-length(contents)]) + 1L
    header[index] <- paste0(format(terms, justify = "right"), sep)
    paste0(
        format(header, justify = "right"),
        unlist(contents, FALSE, FALSE)
    )
}
