#' @return A string
#' @noRd
rd_parse <- function(docs, ...) UseMethod("rd_parse")

#' @export
rd_parse.default <- function(docs, ...) {
    if (is.null(rd_tag(docs))) {
        stop("`docs` must be a valid Rd document", call. = FALSE)
    } else {
        stop(
            sprintf("Cannot deal with Rd tag: %s", rd_tag(docs)),
            call. = FALSE
        )
    }
}

# rd_tag -------------------------------------------
rd_tag <- function(x) attr(x, "Rd_tag")

as_rd_tag <- function(x) {
    if (is.null(tag <- rd_tag(x)) || inherits(x, "rd_tag")) {
        return(x)
    }
    # Rd_tags start with "\\", e.g. "\\description",
    # hence we replace that with "tag_", e.g. "tag_description"
    structure(
        x,
        class = c(sub("\\", "tag_", tag, fixed = TRUE), "rd_tag"),
        Rd_tag = NULL,
        srcref = NULL,
        macros = NULL
    )
}

# Fabricated tag indicate nothing -----------------
#' @export
rd_parse._tag_null <- function(docs, ...) ""

new_tag_null <- function() {
    structure(list(), class = c("_tag_null", "rd_tag"))
}

# Various types of text ------------------------
rd_flatten_text <- function(docs, ...) {
    if (length(docs) == 0L) return("") # styler: off
    if (is.list(docs)) {
        out <- vapply(
            docs,
            function(doc) rd_parse(docs = as_rd_tag(doc), ...),
            character(1L),
            USE.NAMES = FALSE
        )
        paste(out, collapse = "")
    } else if (inherits(docs, "rd_tag")) {
        # For single leave
        as.character(docs)
    } else {
        rd_parse(docs = as_rd_tag(docs), ...)
    }
}

rd_flatten_para <- function(docs, ..., parser = rd_parser()) {
    if (length(docs) == 0L) return("") # styler: off
    parsed <- vapply(docs, function(doc) {
        rd_parse(docs = as_rd_tag(doc), ..., parser = parser)
    }, character(1L), USE.NAMES = FALSE)
    rd_postparser(parser$paragraph(parsed, docs))
}

rd_stop_bad_tag <- function(tag, msg = NULL) {
    msg_abort <- sprintf("Failed to parse tag `%s`.", paste0("\\", tag, "{}"))
    stop(paste(msg_abort, msg), call. = FALSE)
}

# Whitespace helper
rd_is_simple_tag <- function(x) {
    inherits(x, "TEXT") ||
        inherits(x, "RCODE") ||
        inherits(x, "VERB") ||
        inherits(x, "UNKNOWN")
}

rd_is_empty <- function(x) {
    rd_is_simple_tag(x) && grepl("^\\s*$", rd_parse(x))
}

rd_trim_newline <- function(x) gsub("^\\r?\\n|\\r?\\n$", "", x)

rd_is_newline <- function(x, trim = FALSE) {
    if (!rd_is_simple_tag(x)) return(FALSE) # styler: off
    text <- rd_parse(x)
    if (trim) text <- gsub("^[ \t]+|[ \t]+$", "", text)
    identical(text, "\n")
}

rd_trim_empty_nodes <- function(x, side = "both") {
    ws <- vapply(x, rd_is_empty, logical(1L), USE.NAMES = FALSE)
    if (!any(ws)) {
        return(x)
    }
    if (all(ws)) {
        return(x[0])
    }
    which_not <- which(!ws)

    if (side %in% c("left", "both")) {
        start <- which_not[1]
    } else {
        start <- 1
    }

    if (side %in% c("right", "both")) {
        end <- which_not[length(which_not)]
    } else {
        end <- length(x)
    }
    x[start:end]
}
