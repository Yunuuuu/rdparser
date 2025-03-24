# https://developer.r-project.org/parseRd.pdf
# Table 2: Table of markup macros within sections taking LaTeX-like text.

# Leaves -----------------------------------------
# Non-macro tags include TEXT, RCODE, VERB, COMMENT, UNKNOWN (an unrecognized
# macro), and LIST (in Latex-like mode, a group of tokens in braces).
#
#' @export
rd_parse.TEXT <- function(docs, ...) as.character(docs)

#' @export
rd_parse.RCODE <- rd_parse.TEXT

#' @export
rd_parse.VERB <- rd_parse.TEXT

#' @export
rd_parse.COMMENT <- function(docs, ..., parser = rd_parser()) {
    rd_postparser(parser$comment(as.character(docs)))
}

#' @export
rd_parse.UNKNOWN <- rd_parse.TEXT

#' @export
rd_parse.LIST <- rd_flatten_text

# USERMACRO appears first, followed by the rendered macro
#' @export
rd_parse.USERMACRO <- function(docs, ...) ""

# subsection ---------------------------------------------------------
#' @export
rd_parse.tag_subsection <- function(docs, ..., level = 1L,
                                    parser = rd_parser()) {
    title <- .subset2(docs, 1L)
    text <- .subset2(docs, 2L)
    text <- rd_flatten_para(text, ..., level = level + 1L, parser = parser)
    text <- rd_trim_newline(text)
    title <- rd_flatten_text(title, ..., level = level + 1L, parser = parser)
    rd_postparser(parser$subsection(text, title, level = level))
}

# Methods -----------------------------------------------------------
#' @export
rd_parse.tag_method <- function(docs, ...) method_usage(docs, "S3")

#' @export
rd_parse.tag_S3method <- function(docs, ...) method_usage(docs, "S3")

#' @export
rd_parse.tag_S4method <- function(docs, ...) method_usage(docs, "S4")

method_usage <- function(x, type) {
    # Despite these being called from the as_html() generic, the target isn't
    # actually HTML, but R code, which is turned into HTML by the syntax
    # highlighting in as as_data.tag_usage()
    fun <- rd_flatten_text(.subset2(x, 1L))
    class <- rd_flatten_text(.subset2(x, 2L))

    if (class == "default") {
        method <- sprintf("# Default %s method", type)
    } else {
        method <- sprintf("# %s method for class '%s'", type, class)
    }
    if (fun != make.names(fun)) {
        # for non-syntactic names
        fun <- sprintf("`%s`", fun)
    }
    paste(method, fun, sep = "\n")
}

# Conditionals ----------------------------------------------------
#' @export
rd_parse.tag_if <- function(docs, ..., parser = rd_parser()) {
    if (isTRUE(parser$showif(.subset2(docs, 1L)))) {
        rd_flatten_text(.subset2(docs, 2L), ..., parser = parser)
    } else {
        ""
    }
}

#' @export
rd_parse.tag_ifelse <- function(docs, ..., parser = rd_parser()) {
    if (isTRUE(parser$showif(.subset2(docs, 1L)))) {
        rd_flatten_text(.subset2(docs, 2L), ..., parser = parser)
    } else {
        rd_flatten_text(.subset2(docs, 3L), ..., parser = parser)
    }
}

# Platform-specific documentation --------------------------------
#' @export
`rd_parse.#ifdef` <- function(docs, ...) {
    os <- trimws(rd_flatten_text(.subset2(docs, 1L), ...))
    if (os == "unix") {
        rd_flatten_text(.subset2(docs, 2L), ...)
    } else {
        ""
    }
}

#' @export
`rd_parse.#ifndef` <- function(docs, ...) {
    os <- trimws(rd_flatten_text(.subset2(docs, 1L), ...))
    if (os == "windows") {
        rd_flatten_text(.subset2(docs, 2L), ...)
    } else {
        ""
    }
}

# List --------------------------------------------------------
#' @export
rd_parse.tag_itemize <- function(docs, ...) {
    rd_parse_items(docs, enum = FALSE, ...)
}

#' @export
rd_parse.tag_enumerate <- function(docs, ...) {
    rd_parse_items(docs, enum = TRUE, ...)
}

# The \item macro may only be used within the argument of a list-like macro.
# Within \enumerate{} or \itemize{} it takes no arguments
rd_parse_items <- function(x, enum = FALSE, ..., parser = rd_parser()) {
    x <- lapply(x, as_rd_tag)
    separator <- vapply(x, inherits, logical(1L), "tag_item", USE.NAMES = FALSE)
    groups <- cumsum(separator)
    # Drop anything before first `tag_item`
    keep <- groups != 0L
    # heading <- rd_flatten_text(x[!keep], ..., parser = parser)
    x <- x[keep]
    groups <- groups[keep]
    descriptions <- vapply(
        split(x, groups),
        function(text) {
            gsub(
                "^[ \t\r\n]+|\r?\n\\s*$", "",
                rd_flatten_para(text, ..., parser = parser)
            )
        },
        character(1L),
        USE.NAMES = FALSE
    )
    if (enum) {
        rd_postparser(parser$ol(descriptions))
    } else {
        rd_postparser(parser$ul(descriptions))
    }
}

#' @export
rd_parse.tag_describe <- function(docs, ...) {
    rd_parse_definitions(docs, ...)
}

# within \arguments{}, \value{} and \describe{} it takes two arguments.
rd_parse_definitions <- function(x, ..., parser = rd_parser()) {
    x <- lapply(x, as_rd_tag)
    # for the description list
    terms <- descriptions <- desc <- NULL
    for (piece in x) {
        if (length(piece) == 0L) next
        if (inherits(piece, "tag_item")) {
            term <- rd_flatten_text(
                .subset2(piece, 1L),
                ...,
                parser = parser
            )
            terms <- c(terms, term)

            # For the descriptions
            desc <- rd_flatten_para(
                .subset2(piece, 2L),
                ...,
                parser = parser
            )
            descriptions <- c(descriptions, desc)
        }
    }
    if (is.null(terms)) return("") # styler: off
    rd_postparser(parser$dl(terms, descriptions))
}

# Effectively does nothing: only used by rd_parse_items() to split up
# sequence of tags.
#' @export
rd_parse.tag_item <- function(docs, ...) ""

# Tables --------------------------------------------------------
#' @export
rd_parse.tag_tabular <- function(docs, ..., parser = rd_parser()) {
    align_abbr <- rd_flatten_text(.subset2(docs, 1L), ..., parser = parser)
    align_abbr <- .subset2(strsplit(align_abbr, ""), 1L)
    align_abbr <- align_abbr[!(align_abbr %in% c("|", ""))]
    align <- unname(c("r" = "right", "l" = "left", "c" = "center")[align_abbr])

    contents <- lapply(.subset2(docs, 2L), as_rd_tag)
    classes <- vapply(
        contents,
        function(x) .subset(class(x), 1L),
        character(1L),
        USE.NAMES = FALSE
    )
    sep <- classes %in% c("tag_cr", "tag_tab")
    contents[sep] <- list(new_tag_null()) # remove separator

    # Identify groups in reverse order (preserve empty cells)
    # Negative maintains correct ordering once reversed
    cell_grp <- rev(cumsum(-rev(sep)))

    cells <- unname(split(contents, cell_grp))
    # Remove trailing content (that does not match the dimensions of the table)
    cells <- cells[seq_len(length(cells) - length(cells) %% length(align))]
    cell_contents <- vapply(
        cells,
        rd_flatten_text,
        character(1L),
        ...,
        parser = parser,
        USE.NAMES = FALSE
    )
    table <- matrix(
        cell_contents,
        ncol = length(align),
        byrow = TRUE
    )
    rd_postparser(parser$tabular(table, align))
}

#' @export
rd_parse.tag_tab <- function(docs, ..., parser = rd_parser()) {
    rd_postparser(parser$tab())
}
