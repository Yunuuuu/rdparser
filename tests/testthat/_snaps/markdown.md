# simple tags translated to known good values

    Code
      cat_line(rd_text("\\cr"))
    Output
      \
      

# subsection generate header

    Code
      cat_line(rd_text("\\subsection{A}{B}"))
    Output
      # A
      B

---

    Code
      cat_line(rd_text("\\subsection{A}{\n    p1\n\n    p2\n  }"))
    Output
      # A
          p1
      
          p2
        

# nested subsection works well

    Code
      cat_line(rd_text("\\subsection{H1}{\\subsection{H2}{}}"))
    Output
      # H1
      ## H2

# tabular: generates complete table html

    Code
      cat_line(rd_text(table))
    Output
      |     |     |
      |:----|:----|
      |  a  |  b  |

# tabular: internal \crs are stripped

    Code
      cat_line(rd_text(table))
    Output
      |     |
      |:----|
      |  a  |
      |  b  |
      |  c  |

# tabular: can convert single row

    Code
      cat_line(rd_text("\\tabular{lll}{A \\tab B \\tab C \\cr}"))
    Output
      |     |     |     |
      |:----|:----|:----|
      |  A  |  B  |  C  |

---

    Code
      cat_line(rd_text("\\tabular{lll}{\\tab\\tab C\\cr}"))
    Output
      |     |     |     |
      |:----|:----|:----|
      |     |     |  C  |

---

    Code
      cat_line(rd_text("\\tabular{lll}{\\tab B \\tab\\cr}"))
    Output
      |     |     |     |
      |:----|:----|:----|
      |     |  B  |     |

---

    Code
      cat_line(rd_text("\\tabular{lll}{A\\tab\\tab\\cr}"))
    Output
      |     |     |     |
      |:----|:----|:----|
      |  A  |     |     |

---

    Code
      cat_line(rd_text("\\tabular{lll}{\\tab\\tab\\cr}"))
    Output
      |     |     |     |
      |:----|:----|:----|
      |     |     |     |

# tabular: can skip trailing \cr

    Code
      cat_line(rd_text("\\tabular{lll}{A \\tab B \\tab C}"))
    Output
      |     |     |     |
      |:----|:----|:----|
      |  A  |  B  |  C  |

# code blocks in tables render (#978)

    Code
      cat_line(rd_text("\\tabular{ll}{a \\tab \\code{b} \\cr foo \\tab bar}"))
    Output
      |     |     |
      |:----|:----|
      |  a  | `b` |
      | foo | bar |

# tables with tailing 
 (#978)

    Code
      cat_line(rd_text(
        "\\tabular{ll}{\n        a   \\tab     \\cr\n        foo \\tab bar\n      }\n    "))
    Output
      |     |     |
      |:----|:----|
      |  a  |     |
      | foo | bar |
          

# \describe items can contain multiple paragraphs

    Label 1: Contents 1
    Label 2: Contents 2

---

    Label: 
                 Paragraph 1
           
                 Paragraph 2
               

# nested item with whitespace parsed correctly

    
        Label: 
           
                 This text is indented in a way pkgdown doesn't like.
             

# preformatted blocks aren't double escaped

    ```
    %>%
    ```

# newlines are preserved in preformatted blocks

    ```
    ^
    
    b
    
    c
    ```

