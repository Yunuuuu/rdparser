# https://developer.r-project.org/parseRd.pdf
# Table1: Table of sectioning macros.
#' @export
rd_parse.tag_arguments <- function(docs, ..., level = 1L, parser = rd_parser()) {
    level <- level + 1L # tag_tile is the top level, for others, always add one
    out <- rd_describe_contents(docs, ..., level = level + 1L, parser = parser)
    # we removing the beginning and ending string of `"\n"`, in this way
    # `parser$rd_*` methods can hypothesize the input is the text only,
    # user is easy to add text in the beginning and ending
    out <- rd_trim_newline(out)
    out <- parser$rd_preparse(out)
    out <- parser$rd_arguments(out, level = level)
    # Instead of using paste(, collapse = "\n"),
    # we always add an ending `\n`, since we have removed it
    paste(parser$rd_postparse(out), "\n")
}

#' @export
rd_parse.tag_author <- function(docs, ..., level = 1L, parser = rd_parser()) {
    level <- level + 1L
    out <- rd_flatten_para(docs, ..., level = level + 1L, parser = parser)
    out <- rd_trim_newline(out)
    out <- parser$rd_preparse(out)
    out <- parser$rd_author(out, level = level)
    paste(parser$rd_postparse(out), "\n")
}

#' @export
rd_parse.tag_concept <- function(docs, ..., level = 1L, parser = rd_parser()) {
    level <- level + 1L
    out <- rd_flatten_text(docs, ..., level = level + 1L, parser = parser)
    out <- rd_trim_newline(out)
    out <- parser$rd_preparse(out)
    out <- parser$rd_concept(out, level = level)
    paste(parser$rd_postparse(out), "\n")
}

#' @export
rd_parse.tag_description <- function(docs, ..., level = 1L,
                                     parser = rd_parser()) {
    level <- level + 1L
    out <- rd_flatten_para(docs, ..., level = level + 1L, parser = parser)
    out <- rd_trim_newline(out)
    out <- parser$rd_preparse(out)
    out <- parser$rd_description(out, level = level)
    paste(parser$rd_postparse(out), "\n")
}

#' @export
rd_parse.tag_details <- function(docs, ..., level = 1L, parser = rd_parser()) {
    level <- level + 1L
    out <- rd_flatten_para(docs, ..., level = level + 1L, parser = parser)
    out <- rd_trim_newline(out)
    out <- parser$rd_preparse(out)
    out <- parser$rd_details(out, level = level)
    paste(parser$rd_postparse(out), "\n")
}

#' @export
rd_parse.tag_docType <- function(docs, ..., level = 1L, parser = rd_parser()) {
    level <- level + 1L
    out <- rd_flatten_text(docs, ..., level = level + 1L, parser = parser)
    out <- rd_trim_newline(out)
    out <- parser$rd_preparse(out)
    out <- parser$rd_docType(out, level = level)
    paste(parser$rd_postparse(out), "\n")
}

#' @export
rd_parse.tag_encoding <- function(docs, ..., level = 1L, parser = rd_parser()) {
    level <- level + 1L
    out <- rd_flatten_text(docs, ..., level = level + 1L, parser = parser)
    out <- rd_trim_newline(out)
    out <- parser$rd_preparse(out)
    out <- parser$rd_encoding(out, level = level)
    paste(parser$rd_postparse(out), "\n")
}

#' @export
rd_parse.tag_format <- function(docs, ..., level = 1L, parser = rd_parser()) {
    level <- level + 1L
    out <- rd_flatten_para(docs, ..., level = level + 1L, parser = parser)
    out <- rd_trim_newline(out)
    out <- parser$rd_preparse(out)
    out <- parser$rd_format(out, level = level)
    paste(parser$rd_postparse(out), "\n")
}

#' @export
rd_parse.tag_keyword <- function(docs, ..., level = 1L, parser = rd_parser()) {
    level <- level + 1L
    out <- rd_flatten_text(docs, ..., level = level + 1L, parser = parser)
    out <- rd_trim_newline(out)
    out <- parser$rd_preparse(out)
    out <- parser$rd_keyword(out, level = level)
    paste(parser$rd_postparse(out), "\n")
}

#' @export
rd_parse.tag_name <- function(docs, ..., level = 1L, parser = rd_parser()) {
    level <- level + 1L
    out <- rd_flatten_text(docs, ..., level = level + 1L, parser = parser)
    out <- rd_trim_newline(out)
    out <- parser$rd_preparse(out)
    out <- parser$rd_name(out, level = level)
    paste(parser$rd_postparse(out), "\n")
}

#' @export
rd_parse.tag_note <- function(docs, ..., level = 1L, parser = rd_parser()) {
    level <- level + 1L
    out <- rd_flatten_para(docs, ..., level = level + 1L, parser = parser)
    out <- rd_trim_newline(out)
    out <- parser$rd_preparse(out)
    out <- parser$rd_note(out, level = level)
    paste(parser$rd_postparse(out), "\n")
}

#' @export
rd_parse.tag_references <- function(docs, ..., level = 1L,
                                    parser = rd_parser()) {
    level <- level + 1L
    out <- rd_flatten_para(docs, ..., level = level + 1L, parser = parser)
    out <- rd_trim_newline(out)
    out <- parser$rd_preparse(out)
    out <- parser$rd_references(out, level = level)
    paste(parser$rd_postparse(out), "\n")
}

#' @export
rd_parse.tag_section <- function(docs, ..., level = 1L, parser = rd_parser()) {
    level <- level + 1L
    title <- .subset2(docs, 2L)
    contents <- .subset2(docs, 1L)
    out <- rd_flatten_para(contents, ..., level = level + 1L, parser = parser)
    out <- rd_trim_newline(out)
    title <- rd_flatten_text(title, ..., level = level + 1L, parser = parser)
    out <- parser$rd_preparse(out)
    out <- parser$rd_section(out, title, level = level)
    paste(parser$rd_postparse(out), "\n")
}

#' @export
rd_parse.tag_seealso <- function(docs, ..., level = 1L, parser = rd_parser()) {
    level <- level + 1L
    out <- rd_flatten_para(docs, ..., level = level + 1L, parser = parser)
    out <- rd_trim_newline(out)
    out <- parser$rd_preparse(out)
    out <- parser$rd_seealso(out, level = level)
    paste(parser$rd_postparse(out), "\n")
}

#' @export
rd_parse.tag_source <- function(docs, ..., level = 1L, parser = rd_parser()) {
    level <- level + 1L
    out <- rd_flatten_para(docs, ..., level = level + 1L, parser = parser)
    out <- rd_trim_newline(out)
    out <- parser$rd_preparse(out)
    out <- parser$rd_source(out, level = level)
    paste(parser$rd_postparse(out), "\n")
}

#' @export
rd_parse.tag_title <- function(docs, ..., level = 1L, parser = rd_parser()) {
    out <- rd_flatten_text(docs, ..., level = level + 1L, parser = parser)
    out <- rd_trim_newline(out)
    out <- parser$rd_preparse(out)
    out <- parser$rd_title(out, level = level)
    paste(parser$rd_postparse(out), "\n")
}

#' @export
rd_parse.tag_value <- function(docs, ..., level = 1L, parser = rd_parser()) {
    level <- level + 1L
    out <- rd_describe_contents(docs, ..., level = level + 1L, parser = parser)
    out <- rd_trim_newline(out)
    out <- parser$rd_preparse(out)
    out <- parser$rd_value(out, level = level)
    paste(parser$rd_postparse(out), "\n")
}

#' @export
rd_parse.tag_usage <- function(docs, ..., level = 1L, parser = rd_parser()) {
    level <- level + 1L
    out <- rd_flatten_text(docs, ..., level = level + 1L, parser = parser)
    out <- rd_trim_newline(out)
    out <- parser$rd_preparse(out)
    out <- parser$rd_usage(out, level = level)
    paste(parser$rd_postparse(out), "\n")
}

#' @export
rd_parse.tag_alias <- function(docs, ..., level = 1L, parser = rd_parser()) {
    level <- level + 1L
    out <- rd_flatten_text(docs, ..., level = level + 1L, parser = parser)
    out <- rd_trim_newline(out)
    out <- parser$rd_preparse(out)
    out <- parser$rd_alias(out, level = level)
    paste(parser$rd_postparse(out), "\n")
}


#' @export
rd_parse.tag_Rdversion <- function(docs, ..., level = 1L, parser = rd_parser()) {
    level <- level + 1L
    out <- rd_flatten_text(docs, ..., level = level + 1L, parser = parser)
    out <- rd_trim_newline(out)
    out <- parser$rd_preparse(out)
    out <- parser$rd_Rdversion(out, level = level)
    paste(parser$rd_postparse(out), "\n")
}


#' @export
rd_parse.tag_synopsis <- function(docs, ..., level = 1L, parser = rd_parser()) {
    level <- level + 1L
    out <- rd_flatten_text(docs, ..., level = level + 1L, parser = parser)
    out <- rd_trim_newline(out)
    out <- parser$rd_preparse(out)
    out <- parser$rd_synopsis(out, level = level)
    paste(parser$rd_postparse(out), "\n")
}

#' @export
rd_parse.tag_Sexpr <- function(docs, ..., level = 1L, parser = rd_parser()) {
    level <- level + 1L

    out <- rd_flatten_text(docs, ..., level = level + 1L, parser = parser)
    out <- rd_trim_newline(out)
    out <- parser$rd_preparse(out)
    out <- parser$rd_Sexpr(out, level = level)
    paste(parser$rd_postparse(out), "\n")
}

#' @export
rd_parse.tag_RdOpts <- function(docs, ..., level = 1L, parser = rd_parser()) {
    level <- level + 1L
    out <- rd_flatten_text(docs, ..., level = level + 1L, parser = parser)
    out <- rd_trim_newline(out)
    out <- parser$rd_preparse(out)
    out <- parser$rd_RdOpts(out, level = level)
    paste(parser$rd_postparse(out), "\n")
}

rd_describe_contents <- function(docs, ..., parser = rd_parser()) {
    if (length(docs) == 0L) return("") # styler: off
    # Group contiguous \items{}/whitespace into a <dl>; everything else
    # is handled as is
    block_id <- integer(length(docs))
    block_id[[1L]] <- 1L
    cur_block_is_dl <- inherits(docs[[1L]], "tag_item")
    if (length(docs) > 1L) {
        for (i in seq(2, length(docs))) {
            is_item <- inherits(docs[[i]], "tag_item")
            if (cur_block_is_dl) {
                same_type <- is_item || rd_is_empty(docs[[i]])
            } else {
                same_type <- !is_item
            }
            if (same_type) {
                block_id[[i]] <- block_id[[i - 1]]
            } else {
                block_id[[i]] <- block_id[[i - 1]] + 1
                cur_block_is_dl <- !cur_block_is_dl
            }
        }
    }
    out <- lapply(split(docs, block_id), function(block) {
        if (length(block) == 0L) {
            character()
        } else if (any(vapply(block, inherits, logical(1L),      # styler: off
                              "tag_item", USE.NAMES = FALSE))) { # styler: off
            # for the description list
            rd_parse_definitions(block, ..., parser = parser)
        } else {
            rd_flatten_text(block, ..., parser = parser)
        }
    })
    paste(unlist(out, FALSE, FALSE), collapse = "")
}
