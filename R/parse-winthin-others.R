# Table 3: Table of markup macros within sections taking no text, R-like
# text, or verbatim text.
# Insertions -----------------------------------------------------
# https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Insertions
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

# First element of enc is the encoded version (second is the ascii version)
#' @export
rd_parse.tag_enc <- function(docs, ...) {
    if (length(docs) == 2L) {
        rd_flatten_text(.subset2(docs, 2L), ...)
    } else {
        rd_stop_bad_tag("enc")
    }
}

# The \out{literal} macro would usually be used within the text part of
# \if{format}{text}. It causes the renderer to output the literal text exactly,
# with no attempt to escape special characters.
#' @export
rd_parse.tag_out <- function(docs, ..., parser = rd_parser()) {
    text <- parser$out(rd_flatten_text(docs, ..., parser = parser))
    paste(text, collapse = "\n")
}

# To break a line --------------------------------------------------
#' @export
rd_parse.tag_cr <- function(docs, ..., parser = rd_parser()) {
    paste(parser$cr(), collapse = "\n")
}

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

# Equations ------------------------------------------------------------------
#' @export
rd_parse.tag_eqn <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(.subset2(docs, 1L), ..., parser = parser)
    paste(parser$eqn(text), collapse = "\n")
}

#' @export
rd_parse.tag_deqn <- function(docs, ..., parser = rd_parser()) {
    text <- rd_flatten_text(.subset2(docs, 1L), ..., parser = parser)
    paste(parser$deqn(text), collapse = "\n")
}

# Elements that don't return anything ----------------------------------------
#' @export
rd_parse.tag_newcommand <- function(docs, ...) ""

#' @export
rd_parse.tag_renewcommand <- function(docs, ...) ""

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
