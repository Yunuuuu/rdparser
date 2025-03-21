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
    out <- as.character(docs)
    paste(parser$comment(out), collapse = "\n")
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
rd_parse.tag_subsection <- function(
    docs,
    ...,
    level = 1L,
    parser = rd_parser()) {
    title <- .subset2(docs, 1L)
    docs <- .subset2(docs, 2L)
    out <- rd_flatten_text(docs, ..., level = level + 1L, parser = parser)
    out <- rd_trim_newline(out)
    title <- rd_flatten_text(title, ..., level = level + 1L, parser = parser)
    paste(parser$subsection(out, title, level = level), collapse = "\n")
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

# Equations ------------------------------------------------------------------
#' @export
rd_parse.tag_eqn <- function(docs, ..., parser = rd_parser()) {
    out <- rd_flatten_text(.subset2(docs, 1L), ..., parser = parser)
    paste(parser$eqn(out), collapse = "\n")
}

#' @export
rd_parse.tag_deqn <- function(docs, ..., parser = rd_parser()) {
    out <- rd_flatten_text(.subset2(docs, 1L), ..., parser = parser)
    paste(parser$deqn(out), collapse = "\n")
}

# Conditionals and Sexprs ----------------------------------------------------
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

# #' @export
# rd_parse.tag_Sexpr <- function(docs, ...) {
#     browser()
# }

# Used inside a \usage{} Rd tag to prevent the code from being treated as
# regular R syntax, either because it is not valid R, or because its usage
# intentionally deviates from regular R usage. An example of the former is the
# command line documentation, e.g. `R CMD SHLIB`
# (https://github.com/wch/r-source/blob/trunk/src/library/utils/man/SHLIB.Rd):
#
#    \special{R CMD SHLIB [options] [-o dllname] files}
#
# An example of the latter is the documentation shortcut `?`
# (https://github.com/wch/r-source/blob/trunk/src/library/utils/man/Question.Rd):
#
#    \special{?topic}
#
#' @export
rd_parse.tag_special <- function(docs, ...) {
    rd_flatten_text(docs, ...)
}

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

# Tables ---------------------------------------------------------------------
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
    paste(parser$tabular(table, align), collapse = "\n")
}

# List -----------------------------------------------------------------------
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
        function(text) rd_flatten_para(text, ..., parser = parser),
        character(1L),
        USE.NAMES = FALSE
    )
    # for item without argument, there will always be a single space in the
    # heading
    descriptions <- sub("^ ", "", descriptions)
    if (enum) {
        out <- parser$ol(descriptions)
    } else {
        out <- parser$ul(descriptions)
    }
    paste(out, collapse = "\n")
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
    paste(parser$dl(terms, descriptions), collapse = "\n")
}

#' @export
rd_parse.tag_preformatted <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(docs, ..., parser = parser)
    paste(parser$codeblock(text), collapse = "\n")
}

# Effectively does nothing: only used by rd_parse_items() to split up
# sequence of tags.
#' @export
rd_parse.tag_item <- function(docs, ...) ""

# Insertions --------------------------------------------------------------
# markup macros within sections taking no text
#' @export
rd_parse.tag_R <- function(docs, ..., parser = rd_parser()) {
    paste(parser$R(), collapse = "\n")
}

#' @export
rd_parse.tag_dots <- function(docs, ..., parser = rd_parser()) {
    paste(parser$dots(), collapse = "\n")
}

#' @export
rd_parse.tag_ldots <- function(docs, ..., parser = rd_parser()) {
    paste(parser$ldots(), collapse = "\n")
}

#' @export
rd_parse.tag_cr <- function(docs, ..., parser = rd_parser()) {
    paste(parser$cr(), collapse = "\n")
}

#' @export
rd_parse.tag_tab <- function(docs, ..., parser = rd_parser()) {
    paste(parser$tab(), collapse = "\n")
}

# First element of enc is the encoded version (second is the ascii version)
#' @export
rd_parse.tag_enc <- function(docs, ...) {
    if (length(docs) == 2L) {
        rd_flatten_text(.subset2(docs, 2L), ...)
    } else {
        rd_stop_bad_tag("enc")
    }
}

# Elements that don't return anything ----------------------------------------
#' @export
rd_parse.tag_newcommand <- function(docs, ...) ""

#' @export
rd_parse.tag_renewcommand <- function(docs, ...) ""
