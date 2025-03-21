test_that("simple tags translated to known good values", {
    # Simple insertions
    expect_equal(rd_text("\\ldots"), "...")
    expect_equal(rd_text("\\dots"), "...")
    expect_equal(rd_text("\\R"), "R")
    expect_snapshot(cat_line(rd_text("\\cr")))

    "Macros"
    expect_equal(rd_text("\\newcommand{\\f}{'f'}\\f{}"), "'f'")
    expect_equal(rd_text("\\renewcommand{\\f}{'f'}\\f{}"), "'f'")
})

test_that("comments converted to html", {
    expect_equal(rd_text("a\n%b\nc"), c("a", "<!-- %b -->", "c"))
})

test_that("simple wrappers work as expected", {
    expect_equal(rd_text("\\strong{x}"), "***x***")
    expect_equal(rd_text("\\strong{\\emph{x}}"), "****x****")
})

test_that("subsection generate header", {
    expect_snapshot(cat_line(rd_text("\\subsection{A}{B}")))
    expect_snapshot(cat_line(rd_text(
        "\\subsection{A}{
    p1

    p2
  }"
    )))
})

test_that("nested subsection works well", {
    expect_snapshot(cat_line(rd_text(
        "\\subsection{H1}{\\subsection{H2}{}}"
    )))
})

test_that("if generates Rd", {
    expect_equal(rd_text("\\if{html}{\\bold{a}}"), "**a**")
    expect_equal(rd_text("\\if{latex}{\\bold{a}}"), "")
})

test_that("ifelse generates Rd", {
    expect_equal(rd_text("\\ifelse{html}{\\bold{a}}{x}"), "**a**")
    expect_equal(rd_text("\\ifelse{latex}{x}{\\bold{a}}"), "**a**")
})

test_that("out is for raw Rd", {
    expect_equal(rd_text("\\out{<hr />}"), "<hr />")
})

test_that("support platform specific code", {
    os_specific <- function(command, os, output) {
        rd_text(paste0(
            "#",
            command,
            " ",
            os,
            "\n",
            output,
            "\n",
            "#endif"
        ))
    }

    expect_equal(os_specific("ifdef", "windows", "X"), character())
    expect_equal(os_specific("ifdef", "unix", "X"), "X")
    expect_equal(os_specific("ifndef", "windows", "X"), "X")
    expect_equal(os_specific("ifndef", "unix", "X"), character())
})


# tables ------------------------------------------------------------------
test_that("tabular: generates complete table html", {
    table <- "\\tabular{ll}{a \\tab b \\cr}"
    expect_snapshot(cat_line(rd_text(table)))
})

test_that("tabular: internal \\crs are stripped", {
    table <- "\\tabular{l}{a \\cr b \\cr c \\cr}"
    expect_snapshot(cat_line(rd_text(table)))
})

test_that("tabular: can convert single row", {
    expect_snapshot(
        cat_line(rd_text("\\tabular{lll}{A \\tab B \\tab C \\cr}"))
    )
    expect_snapshot(
        cat_line(rd_text("\\tabular{lll}{\\tab\\tab C\\cr}"))
    )
    expect_snapshot(
        cat_line(rd_text("\\tabular{lll}{\\tab B \\tab\\cr}"))
    )
    expect_snapshot(
        cat_line(rd_text("\\tabular{lll}{A\\tab\\tab\\cr}"))
    )
    expect_snapshot(
        cat_line(rd_text("\\tabular{lll}{\\tab\\tab\\cr}"))
    )
})

test_that("tabular: can skip trailing \\cr", {
    expect_snapshot(
        cat_line(rd_text("\\tabular{lll}{A \\tab B \\tab C}"))
    )
})

test_that("code blocks in tables render (#978)", {
    expect_snapshot(
        cat_line(rd_text("\\tabular{ll}{a \\tab \\code{b} \\cr foo \\tab bar}"))
    )
})

test_that("tables with tailing \n (#978)", {
    expect_snapshot(
        cat_line(rd_text(
            "\\tabular{ll}{
        a   \\tab     \\cr
        foo \\tab bar
      }
    "
        ))
    )
})

# links -------------------------------------------------------------------

test_that("simple links generate <a>", {
    expect_equal(
        rd_text("\\href{http://bar.com}{BAR}"),
        "[BAR](http://bar.com)"
    )
    expect_equal(
        rd_text("\\email{foo@bar.com}"),
        "[foo@bar.com](mailto:foo@bar.com)"
    )
    expect_equal(
        rd_text("\\url{http://bar.com}"),
        "[http://bar.com](http://bar.com)"
    )
})

test_that("can convert cross links to online documentation url", {
    expect_equal(
        rd_text("\\link[base]{library}", autolink = TRUE),
        "[library](https://rdrr.io/r/base/library.html)"
    )
})

test_that("link to non-existing functions return label", {
    expect_equal(rd_text("\\link[xyzxyz:xyzxyz]{abc}", autolink = TRUE), "abc")
    expect_equal(rd_text("\\link[base:xyzxyz]{abc}", autolink = TRUE), "abc")
})

# lists -------------------------------------------------------------------

test_that("simple lists work", {
    expect_equal(
        rd_text("\\itemize{\\item a}"),
        "- a"
    )
    expect_equal(
        rd_text("\\enumerate{\\item a}"),
        "1. a"
    )
})

test_that("\\describe items can contain multiple paragraphs", {
    out <- rd_text(
        "\\describe{
    \\item{Label 1}{Contents 1}
    \\item{Label 2}{Contents 2}
  }"
    )
    expect_snapshot_output(cat_line(out))
    out <- rd_text(
        "\\describe{
    \\item{Label}{
      Paragraph 1

      Paragraph 2
    }
  }"
    )
    expect_snapshot_output(cat_line(out))
})

test_that("nested item with whitespace parsed correctly", {
    out <- rd_text(
        "
    \\describe{
    \\item{Label}{

      This text is indented in a way pkgdown doesn't like.
  }}"
    )
    expect_snapshot_output(cat_line(out))
})

# Verbatim ----------------------------------------------------------------

test_that("preformatted blocks aren't double escaped", {
    out <- rd_text("\\preformatted{\\%>\\%}")
    expect_snapshot_output(cat_line(out))
})

test_that("newlines are preserved in preformatted blocks", {
    out <- rd_text("\\preformatted{^\n\nb\n\nc}")
    expect_snapshot_output(cat_line(out))
})

# Other -------------------------------------------------------------------

test_that("eqn", {
    out <- rd_text("\\eqn{\\alpha}{alpha}")
    expect_equal(out, "$\\alpha$")
    out <- rd_text("\\eqn{x}")
    expect_equal(out, "$x$")
})

test_that("deqn", {
    out <- rd_text("\\deqn{\\alpha}{alpha}")
    expect_equal(out, "$$\\alpha$$")
    out <- rd_text("\\deqn{x}")
    expect_equal(out, "$$x$$")
})

test_that("special", {
    expect_equal(rd_text("\\special{( \\dots )}"), "( ... )")
})

# figures -----------------------------------------------------------------

test_that("figures are converted to img", {
    expect_equal(rd_text("\\figure{a}"), "![](a)")
    expect_equal(rd_text("\\figure{a}{b}"), "![b](a)")
    expect_equal(
        rd_text("\\figure{a}{options: height=1}"),
        "![](a)"
    )
})
