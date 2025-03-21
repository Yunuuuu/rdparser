#' Initialize a `RdParser` object
#'
#' @param ... Additional arguments for initializing the [`RdParser`].
#' @param parser An `R6ClassGenerator` object used to initialize the
#' [`RdParser`], or a string specifying the internal parser. Currently, only the
#' `"markdown"` parser is available.
#' @return A `RdParser` object.
#' @export
rd_parser <- function(..., parser = "markdown") {
    if (is.character(parser)) {
        if (length(parser) != 1L) {
            stop("`parser` must be a single string", call. = FALSE)
        }
        generator <- switch(parser,
            markdown = RdParserMarkdown,
            stop(sprintf("No `%s` parser found", parser), call. = FALSE)
        )
        generator$new(...)
    } else if (!inherits(parser, "R6ClassGenerator")) {
        stop(
            "`parser` must a single string or a `R6ClassGenerator` object",
            call. = FALSE
        )
    } else {
        out <- parser$new(...)
        if (!inherits(out, "RdParser")) {
            stop("`parser` must initialzie a `RdParser` object", call. = FALSE)
        }
        out
    }
}

#' Rd Document Parser
#'
#' @description
#' This R6 class provides methods for parsing and formatting Rd documentation
#' files. It includes functions for applying markup, structuring content, and
#' inserting special formatting. The class is exported to allow developers to
#' extend and customize the parser with additional formatting styles.
#'
#' @details
#' For Rd document sections (methods beginning with `rd_*`), the internal logic
#' trims leading and trailing newline characters (`"\\r?\\n"`). Therefore, the
#' input for these methods consists only of the content. These methods return a
#' character vector, where each element corresponds to a line. Finally, a
#' newline character (`"\n"`) is always added at the end.
#'
#' The `tabular`, `ol`, `ul`, and `dl` methods accept text in a specific format
#' and return a character vector, where each element corresponds to a line.
#'
#' The `subsection` Rd markup macro accepts a single-element character vector
#' and can also return a character vector (since it is very similar to the
#' `rd_section` method), where each element corresponds to a line. But note for
#' some document, it's not possible to add text after a `subsection` for current
#' section, like markdown.
#'
#' For other Rd markup macros (methods not beginning with `"rd_"`),
#' they always accept a single-element character vector and should return a
#' character vector, where each element corresponds to a line.
#'
#' @param text A one-element character representing the input text.
#' @param texts A character vector of multiple text elements.
#' @param href A string specifying the target URL.
#' @param level An integer representing the header level.
#' @param lang The language of the code block. Usually `R`.
#' @param table A matrix of the table content.
#' @param align An character of `"right"`, `"center"`, `"left"` indicate
#' the alignment for each column in the table.
#' @param terms A character vector of terms.
#' @param descriptions A character vector of term definitions.
#' @references
#' - <https://developer.r-project.org/parseRd.pdf>
#' - <https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Rd-format>
#' @importFrom R6 R6Class
#' @export
RdParser <- R6Class(
    "RdParser",
    public = list(
        # format and style -----------------------------------------
        #' @description Indicate a section header
        #' @return A string of formatted text.
        header = function(text, level) stop_not_implement(self, "header"),

        # It seems Rd document doesn't use this
        # blockquote = function(text) {
        #     stop_not_implement(self, "blockquote")
        # },
        #' @description Add a code block
        #' @return A string of formatted text.
        codeblock = function(text, lang = NULL) {
            stop_not_implement(self, "codeblock")
        },

        #' @description Add a comment block
        #' @return A string of formatted text.
        comment = function(text) stop_not_implement(self, "comment"),

        #  Lists and tables ----------------------------------------
        #' @description Create an ordered list
        #' @return A character of each line for the list.
        ol = function(texts) stop_not_implement(self, "ol"),

        #' @description Create an unordered list
        #' @return A character of each line for the list.
        ul = function(texts) stop_not_implement(self, "ul"),

        #' @description Create a definition list
        #' @return A character of each line for the list.
        dl = function(terms, descriptions) {
            stop_not_implement(self, "dl")
        },

        #' @description Create a tabular representation
        #' @return A character of each row for the table.
        tabular = function(table, align) stop_not_implement(self, "tabular"),

        # links ----------------------------------------------------
        #' @description Create a hyperlink
        #' @return A string of formatted text.
        href = function(text, href = NULL) stop_not_implement(self, "href"),

        #' @description Create an email link
        #' @return A string of formatted text.
        email = function(text) {
            self$href(text, paste0("mailto:", text))
        },

        #' @description Insert an image
        #' @param src The image source path or URL.
        #' @param alt Optional. Alternative text for the image.
        #' @param options Optional. Additional formatting options.
        #' @return A string of formatted text.
        image = function(src, alt = NULL, options = NULL) {
            stop_not_implement(self, "image")
        },

        # Marking text --------------------------------------------
        # https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Marking-text
        #' @description Apply italic formatting
        #' @return A string of formatted text.
        emph = function(text) stop_not_implement(self, "emph"),

        #' @description Apply bold formatting
        #' @return A string of formatted text.
        bold = function(text) stop_not_implement(self, "bold"),

        #' @description Apply italic and bold formatting
        #' @return A string of formatted text.
        strong = function(text) stop_not_implement(self, "strong"),

        #' @description Apply double quotes
        #' @return A string of formatted text.
        dQuote = function(text) sprintf("\"%s\"", text),

        #' @description Apply single quotes
        #' @return A string of formatted text.
        sQuote = function(text) sprintf("'%s'", text),

        #' @description Format a fragment of R code or the name of an R object
        #' @return A string of formatted text.
        code = function(text) stop_not_implement(self, "code"),

        #' @description Format text as keyboard input
        #' @return A string of formatted text.
        kbd = function(text) text,

        #' @description Indicate verbatim word-wrapped text
        #' @return A string of formatted text.
        samp = function(text) text,

        #' @description Indicate verbatim text (no wrapping or reformatting)
        #' @return A string of formatted text.
        verb = function(text) text,

        #' @description Indicate the name of an R package
        #' @return A string of formatted text.
        pkg = function(text) text,

        #' @description Indicate a file path
        #' @return A string of formatted text.
        file = function(text) file,

        #' @description Indicate a metasyntactic variable
        #' @return A string of formatted text.
        var = function(text) self$code(text),

        #' @description Indicate environment variable
        #' @return A string of formatted text.
        env = function(text) text,

        #' @description Indicate a command-line option
        #' @return A string of formatted text.
        option = function(text) text,

        #' @description Indicate the name of a command
        #' @return A string of formatted text.
        command = function(text) text,

        #' @description Indicate the introductory or defining use of a term
        #' @return A string of formatted text.
        dfn = function(text) text,

        #' @description Indicate a reference without a direct cross-reference
        #' @return A string of formatted text.
        cite = function(text) text,

        #' @description Indicate an acronym (an abbreviation written in all
        #' capital letters)
        #' @return A string of formatted text.
        acronym = function(text) text,

        #' @description Indicate abbreviation
        #' @return A string of formatted text.
        abbr = function(text) text,

        # Mathematics ---------------------------------------------
        #' @description Inline equation
        #' @return A string of formatted text.
        eqn = function(text) stop_not_implement(self, "eqn"),

        #' @description Display (block) equation
        #' @return A string of formatted text.
        deqn = function(text) stop_not_implement(self, "deqn"),

        # Insertions ----------------------------------------------
        # https://rstudio.github.io/r-manuals/r-exts/Writing-R-documentation-files.html#FOOT124

        #' @description Insert the R system itself
        #' @return A string.
        R = function() "R",

        #' @description Insert an ellipsis of function argument lists
        #' @return A string.
        dots = function() "...",

        #' @description Insert an ellipsis of ordinary text.
        #' @return A string.
        ldots = function() "...",

        #' @description Insert a line break
        #' @return A string.
        cr = function() "\n", # break a line

        #' @description Insert a tab character
        #' @return A string.
        tab = function() "\t",

        #' @description Indicate literal text
        #' @return A string of formatted text.
        out = function(text) text,

        # Conditional text ----------------------------------------
        #' @description Conditional text
        #' @param text Input format, currently the following formats are
        #' recognized: `example`, `html`, `latex` and `text`.
        #' @return A single boolean value indicates whether the text should be
        #' displayed for current `if` and `ifelse` Rd block.
        showif = function(text) FALSE,

        #' @description Format the subsection content. `subsection` is a markup
        #' macro used within sections. Therefore, `rd_preparse` and
        #' `rd_postparse` won't be applied before or after this method.
        #' @param title A string of the subsection title.
        #' @return A string of formatted text.
        subsection = function(text, title, level) {
            c(self$header(title, level), text)
        },

        # Sectioning -----------------------------------------------
        #' @description Formats the Rd document sectioning. Some sections may
        #' contain multiple paragraphs and nested blocks. This default method
        #' just ensures that each block follows a newline character.
        #' @param parsed_list A list of one-element string of the parsed text.
        #' @param docs A list of the raw Rd document text.
        #' @return The formatted text.
        sectioning = function(parsed_list, docs) {
            # if next document text is a block or not?
            is_block <- FALSE
            tag_blocks <- sprintf("tag_%s", c(
                "preformatted", "itemize", "enumerate",
                "describe", "tabular", "subsection"
            ))
            for (i in rev(seq_along(docs))) {
                # block-level tags
                if (inherits(.subset2(docs, i), tag_blocks)) {
                    is_block <- TRUE
                    next
                    # styler: off
                } else if (is_block &&
                           !grepl("\n$", .subset2(parsed_list, i))) {
                    # styler: on
                    # the next text is a block
                    # we always ensure every block is in the next line
                    parsed_list[i] <- list(
                        paste0(.subset2(parsed_list, i), "\n")
                    )
                }
                is_block <- FALSE
            }
            paste0(unlist(parsed_list, FALSE, FALSE), collapse = "")
        },

        #' @description A hook of command to be run before parsing the Rd
        #' section
        #' @return A string of formatted text.
        rd_preparse = function(text) text,

        #' @description A hook of command to be run after parsing the Rd section
        #' @return A string of formatted text.
        rd_postparse = function(text) text,

        #' @description Format the argument setction
        #' @return The formatted text.
        rd_arguments = function(text, level) {
            c(self$header("Arguments", level), text)
        },

        #' @description Format the author setction
        #' @return The formatted text.
        rd_author = function(text, level) {
            c(self$header("Author", level), text)
        },

        #' @description Format the concept setction
        #' @return The formatted text.
        rd_concept = function(text, level) {
            c(self$header("Concept", level), text)
        },

        #' @description Format the description setction
        #' @return The formatted text.
        rd_description = function(text, level) {
            c(self$header("Description", level), text)
        },

        #' @description Format the details setction
        #' @return The formatted text.
        rd_details = function(text, level) {
            c(self$header("Details", level), text)
        },

        #' @description Format the docType setction
        #' @return The formatted text.
        rd_docType = function(text, level) {
            c(self$header("docType", level), text)
        },

        #' @description Format the encoding setction
        #' @return The formatted text.
        rd_encoding = function(text, level) {
            c(self$header("Encoding", level), text)
        },

        #' @description Style the format setction
        #' @return The formatted text.
        rd_format = function(text, level) {
            c(self$header("Format", level), text)
        },

        #' @description Format the keyword setction
        #' @return The formatted text.
        rd_keyword = function(text, level) text,

        #' @description Format the name setction
        #' @return The formatted text.
        rd_name = function(text, level) text,

        #' @description Format the note setction
        #' @return The formatted text.
        rd_note = function(text, level) {
            c(self$header("Note", level), text)
        },

        #' @description Format the references setction
        #' @return The formatted text.
        rd_references = function(text, level) {
            c(self$header("References", level), text)
        },

        #' @description Format the section content
        #' @param title A string of the section title.
        #' @return The formatted text.
        rd_section = function(text, title, level) {
            c(self$header(title, level), text)
        },

        #' @description Format the seealso section
        #' @return The formatted text.
        rd_seealso = function(text, level) {
            c(self$header("Seealso", level), text)
        },

        #' @description Format the source section
        #' @return The formatted text.
        rd_source = function(text, level) {
            c(self$header("Source", level), text)
        },

        #' @description Format the title section
        #' @return The formatted text.
        rd_title = function(text, level) self$header(text, level),

        #' @description Format the value section
        #' @return The formatted text.
        rd_value = function(text, level) {
            c(self$header("Return", level), text)
        },

        #' @description Format the examples section
        #' @return The formatted text.
        rd_examples = function(text, level) {
            c(self$header("Examples", level), text)
        },

        #' @description Format the usage section
        #' @return The formatted text.
        rd_usage = function(text, level) {
            c(self$header("Usage", level), self$codeblock(text, lang = "r"))
        },

        #' @description Format the alias section
        #' @return The formatted text.
        rd_alias = function(text, level) text,

        #' @description Format the Rdversion section
        #' @return The formatted text.
        rd_Rdversion = function(text, level) text,

        #' @description Format the synopsis section
        #' @return The formatted text.
        rd_synopsis = function(text, level) text,

        #' @description Format the Sexpr section
        #' @return The formatted text.
        rd_Sexpr = function(text, level) text,

        #' @description Format the RdOpts section
        #' @return The formatted text.
        rd_RdOpts = function(text, level) text
    )
)

stop_not_implement <- function(self, method) {
    stop(
        sprintf("`%s` didn't implement `%s()` method"),
        .subset(class(self), 1L), method
    )
}
