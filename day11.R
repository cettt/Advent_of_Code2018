id <- as.integer(read.table("input/day11.txt"))

calculate_pow <- function(x, y) {
  z <- floor(((x + 10) * y + id) * (x + 10) / 100)
  z %% 10 - 5
}
pow_field <- outer(1:300, 1:300, calculate_pow)
cur_max <- sum(pow_field[1:3, 1:3])

#part1-----------
find_field <- function(n) {
  cur_max <- sum(pow_field[1:n, 1:n])
  
  for (x in 1:(300- n + 1)) {
    for (y in 1:(300- n + 1)) {
      pow_sq <- sum(pow_field[1:n + x - 1, 1:n + y - 1])
      if (pow_sq > cur_max) {
        cur_max <- pow_sq
        res <- c(x, y)
      }
    }
  }
  return(c(cur_max, res, n))
}
paste(find_field(3)[2:3], collapse = ",")

#part2--------
#TODO elegantere Lösung finden
res_size <- sapply(1:300, find_field)

paste(res_size[2:4 ,which.max(res_size[1, ])], collapse = ",")
#"233,187,13"