# 

# https://stackoverflow.com/questions/45591286/for-r-markdown-how-do-i-display-a-matrix-from-r-variable

# Define a generic method that transforms an object x in a LaTeX string
as_latex = function(x, ...) {
  UseMethod('as_latex', x)
}

# Define a class latex for LaTeX expressions
as_latex.character = function(x) {
  structure(
    paste(x, collapse = ' '), 
    class = c('latex', 'character')
  )
}

# define addition of latex, to be concatenation of strings
`+.latex` <- function(e1, e2) {
  structure(paste0(e1, e2), class = c("latex", "character"))
}


# A character string of class latex is rendered in display mode
# Define a knit_print() method for the latex class
knit_print.latex = function(x, ...) {
  knitr::asis_output(
    paste0('$$', x, '$$')
  )
}

print.latex <- function(x, ...) {
  print.default(x)
}

# Now, define a method as_latex for matrix
as_latex.matrix = function(x, ...) {
  as_latex(c(
    '\\begin{bmatrix}',
    paste(
      t(x),
      rep(c(rep('&', ncol(x) - 1), '\\\\'), nrow(x)),
      collapse = ''
    ),
    '\\end{bmatrix}'
  ))
}

# Indicate to knitr that matrix are rendered as latex
knit_print.matrix = function(x, ...) {
  # only print as latex if no dimension names
  knitr::knit_print(as_latex(x))
}

# Build a knitr inline hook to display inline latex in inline mode
default_inline_hook = knitr::knit_hooks$get('inline')
knitr::knit_hooks$set(inline = function(x) {
  x = paste(gsub('\\$\\$', '$', x))
  default_inline_hook(x)
})

`%times%` = function(x, y) {
  as_latex(sapply(list(x, '\\times', y), as_latex))  
}

`%add%` = function(x, y) {
  as_latex(sapply(list(x, '+', y), as_latex))  
}

# Not sure this is necessary, or what this is doing
# registerS3method("knit_print", "latex", "knit_print.latex")