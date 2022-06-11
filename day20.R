data20 <- strsplit(gsub("[\\^\\$]", "", readLines("input/day20.txt")), "")[[1]]
dir <- setNames(c("N" = 1i, "E" = 1, "S" = -1i, "W" = -1)[data20], data20)

n_doors_vec <- 0L
pos_vec <- 0 + 0i
cur_pos <- pos_vec
cur_n_doors <- 0L
pos_end <- complex()
n_doors_end <- integer()

while (length(dir) > 0) {
  
  if (!is.na(dir[1])) {
    dir_vec <- dir[cumsum(is.na(dir)) == 0]
    n_doors_vec <- c(n_doors_vec, cur_n_doors + seq_along(dir_vec))
    cur_n_doors <- cur_n_doors + length(dir_vec)
    pos_vec <- c(pos_vec, cur_pos + cumsum(dir_vec))
    cur_pos <- cur_pos + sum(dir_vec)
    dir <- dir[-seq_along(dir_vec)]
  } 
  if (names(dir[1]) == "|") {
    cur_pos <- pos_end[1]
    cur_n_doors <- n_doors_end[1]
  } else if (names(dir[1]) == "(") {
    pos_end <- c(cur_pos, pos_end)
    n_doors_end <- c(cur_n_doors, n_doors_end)
  } else { 
    pos_end <- pos_end[-1]
    n_doors_end <- n_doors_end[-1]
  }
  dir <- dir[-1]
  
}

n_door <- aggregate(n_doors_vec, by = list(pos_vec), min)[,2]

#part1--------
max(n_door)

#part2------
sum(n_door >= 1000L)

