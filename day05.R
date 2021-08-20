data05 <- readLines("input/day05.txt")


pat <- paste(c(paste0(letters, LETTERS), paste0(LETTERS, letters)), collapse = ")|(")
pat <- paste0("(", pat, ")")


# part1---------
x <- data05
while (grepl(pat, x)) {
  x <- gsub(pat, "", x)
}
nchar(x)


# part2-------------
improve_poly <- function(k) {
  y <- gsub(paste(letters[k], LETTERS[k], sep = "|"), "", x)
  while (grepl(pat, y)) {
    y <- gsub(pat, "", y)
  }
  nchar(y)
}

min(sapply(seq_along(letters), improve_poly))
