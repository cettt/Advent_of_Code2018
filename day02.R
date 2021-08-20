data02 <- readLines("input/day02.txt")

#part1-----
prod(rowSums(sapply(data02, function(x) 2:3 %in% table(strsplit(x, "")[[1]]))))

#part2-----
for (k in seq_len(nchar(data02[1]))) {
  x <- sub(paste0("(^.{", k, "})."), "\\1", data02)
  if (any(duplicated(x))) print(x[duplicated(x)][1])
}
