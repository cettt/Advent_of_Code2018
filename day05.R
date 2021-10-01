data05 <- readLines("input/day05.txt")

react_polimer <- function(x = data05, let = NULL) {
  while (TRUE) {
    x_old <- x
    for (a in setdiff(letters, let)) {
      x <- gsub(paste0(a, toupper(a)), "", gsub(paste0(toupper(a), a), "", x))
    }
    if (x == x_old) break;
  }
  return(x)
}

#part1 ------
rp <- react_polimer()  
nchar(rp)

# part2-------------
improve_poly <- function(a) {
  nchar(react_polimer(gsub(a, "", gsub(toupper(a), "", rp)), a))
}

min(sapply(letters, improve_poly))
