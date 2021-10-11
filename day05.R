data05 <- readLines("input/day05.txt")

aA <- paste0(letters, LETTERS)
Aa <- paste0(LETTERS, letters)

react_polimer <- function(x = data05, let = 27) {
  
  while (TRUE) {
    x_old <- x
    x <- stringi::stri_replace_all_fixed(x, c(aA[-let], Aa[-let]), "", vectorise_all = FALSE)
    if (nchar(x) == nchar(x_old)) break
  }
  return(x)
}

#part1 ------
rp <- react_polimer()  
nchar(rp)

# part2-------------
improve_poly <- function(a) {
  nchar(react_polimer(gsub(a, "", gsub(toupper(a), "", rp)), which(letters == a)))
}

min(sapply(letters, improve_poly))
