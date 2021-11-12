TOKENS <- c("DIGIT9PLUS", "DIGIT", "DECIMALPOINT")
LITERALS <-
  unique(c(
    # used with date/time
    "W", "Z", "Q", "W", "T", ":", "-", "+",
    # used with durations
    "P", "Y", "M", "W", "D", "H", "M", "S",
    # used with intervals
    "R", "/"
  ))

# Lexer ####

Lexer <-
  R6::R6Class(
    "Lexer",
    public=list(
      tokens=TOKENS,
      literals=LITERALS,
      t_DIGIT9PLUS="[0-9]{9,}",
      t_DIGIT="[0-9]",
      t_DECIMALPOINT="[\\.,]",
      #t_ignore = " \t",
      t_newline = function(re='\\n+', t) {
        t$lexer$lineno <- t$lexer$lineno + nchar(t$value)
        return(NULL)
      },
      t_error = function(t) {
        cat(sprintf("Illegal character '%s'", t$value[1]))
        t$lexer$skip(1)
        return(t)
      }
    )
  )

lexer <- rly::lex(Lexer)
