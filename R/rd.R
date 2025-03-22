#' Read Rd document
#'
#' Reads an Rd document, which can then be translated using [`rd_document()`].
#'
#' @describeIn read_rd_source Read Rd document from a R source package
#' @param source A string of the directory of a R source package.
#' @seealso [`rd_document()`]
#' @return A list of [`Rd`][tools::parse_Rd] object.
#' @export
read_rd_source <- function(source) {
    man_path <- file.path(source, "man")
    if (!dir.exists(man_path)) {
        return(structure(list(), names = character()))
    }
    rd_files <- list.files(man_path,
        pattern = "\\.[Rr]d$",
        full.names = TRUE,
        no.. = TRUE
    )
    names(rd_files) <- basename(rd_files)
    macros <- tools::loadPkgRdMacros(source)
    lapply(rd_files, function(file) {
        tools::parse_Rd(file, macros = macros, encoding = "UTF-8")
    })
}

#' @describeIn read_rd_source Read Rd document from an installed package.
#' @param package A string of the installed package name.
#' @param lib A character vector of directory names of R libraries.  The default
#' value of `NULL` corresponds to all libraries currently known. The specified
#' library trees are used to search for package.
#' @export
read_rd_package <- function(package, lib = NULL) {
    tools::Rd_db(package, lib.loc = lib)
}

#' Read and parse Rd document
#'
#' @describeIn rd_text Read and parse an Rd document from a text string
#'
#' @param text Rd document character. Backslashes must be double-escaped
#' ("\\\\").
#' @inheritParams tools::parse_Rd
#' @inheritDotParams rd_document -rd
#' @examples
#' rd_text("a\n%b\nc")
#' rd_text("a & b")
#' rd_text("\\strong{x}")
#' @seealso [`rd_document()`]
#' @export
rd_text <- function(text, ..., fragment = TRUE) {
    rd_document(rd = rd_from_text(text, fragment), ...)
}

rd_from_text <- function(text, fragment = TRUE) {
    con <- textConnection(text)
    on.exit(close(con), add = TRUE)
    tools::parse_Rd(con, fragment = fragment, encoding = "UTF-8")
}

#' @describeIn rd_text Read and parse an Rd document from a file
#' @param file A string of the Rd document file
#' @export
rd_file <- function(file, ...) {
    rd <- tools::parse_Rd(file, encoding = "UTF-8")
    rd_document(rd = rd, ...)
}

#' Translate an Rd document using a specified parser
#'
#' @param rd An Rd document obtained from [`parse_Rd()`][tools::parse_Rd].
#' @param parser An `RdParser` object.
#' @param autolink A logical value indicating whether to use `downlit` for
#' automatic linking of R code documentation.
#' @param ofile A string specifying the output file path or a writable
#' [`connection`]. If `NULL`, the function returns the parsed document as a
#' character string.
#' @param level An integer defining the starting level of the document title.
#' @return If `ofile` is `NULL`, returns a character string of the parsed
#' document. Otherwise, writes the output to `ofile` and returns
#' `invisible(ofile)`.
#' @seealso
#'  - [`read_rd_source()`]/[`read_rd_package()`]
#'  - [`rd_text()`]/[`rd_file()`]
#' @export
rd_document <- function(rd, parser = rd_parser(), autolink = FALSE,
                        ofile = NULL, level = 1L) {
    if (!inherits(rd, "Rd")) {
        stop("`rd` must be a `Rd` object", call. = FALSE)
    }
    if (!inherits(parser, "RdParser")) {
        stop("`parser` must be a `RdParser` object", call. = FALSE)
    }
    level <- as.integer(level)
    if (level <= 0L) {
        stop("`level` must be a positive integer", call. = FALSE)
    }
    if (!is.logical(autolink) || length(autolink) != 1L) {
        stop("`autolink` must be a single boolean value", call. = FALSE)
    }
    if (autolink && !is_installed("downlit")) {
        msg <- "`downlit` must be installed to use `autolink`"
        if (interactive()) {
            question <- c(msg, "would you like to install it?")
            cat(question, sep = ", ")
            cat("\n")
            if (utils::menu(c("Yes", "No")) == 1) {
                install_pkgs("downlit")
                invokeRestart("abort")
            }
        }
        stop(msg, call. = FALSE)
    }
    if (is.null(ofile)) {
        con <- file(open = "w+")
        on.exit(close(con), add = TRUE)
    } else if (is.character(ofile)) {
        if (length(ofile) != 1L || !nzchar(ofile)) {
            stop("`ofile` must be a single string of file path", call. = FALSE)
        }
        if (ofile == "clipboard") {
            stop("`ofile` cannot use clipboard", call. = FALSE)
        }
        con <- file(ofile, open = "w+")
        on.exit(close(con), add = TRUE)
    } else if (inherits(ofile, "connection")) {
        if (isOpen(ofile, "write")) {
            con <- ofile
        } else {
            stop("`ofile` is not writable", call. = FALSE)
        }
    } else {
        stop(
            "`ofile` must be a single string of file path or a `connection`",
            call. = FALSE
        )
    }
    for (docs in rd) {
        cat(
            rd_parse(
                docs = as_rd_tag(docs),
                level = level,
                autolink = autolink,
                parser = parser
            ),
            file = con,
            sep = ""
        )
    }
    if (is.null(ofile)) readLines(con) else invisible(ofile)
}

cat_line <- function(..., file = "") {
    cat(
        paste0(..., collapse = "\n"), "\n",
        sep = "", file = file, append = TRUE
    )
}
