data12 <- readLines("input/day12.txt")[-2]

init <- strsplit(gsub("[^\\.#]+", "", data12[1]), "")[[1]]
map1 <- sapply(strsplit(substr(data12[-1], 1, 5), ""), function(x) sum((x == "#") * 2 ^(4:0)))
map <- rep(".", 32)
map[map1[grepl("#$", data12[-1])] + 1] <- "#"

#part1--------

x <- c(rep(".", 22), init, rep(".", 22))

for (i in 1:20) {
  y <- x
  for (k in 3:(length(x) - 2)) y[k] <- map[sum((x[k + 0:4 - 2] == "#") * 2 ^(4:0))+1]
  x <- y
}

sum(which(x == "#") - 23)

#part2-----------

#by observing the patter
#  we can see that if a plant is not close to any other plant
#  (i.e. the distance from one plant to another is greater than 5)
# that plant will just move one step to the right

x <- init
names(x) <- c(seq_along(x) - 1)
gen <- 0

while (any(diff(which(x == "#")) < 6)) { #simulate until all remaining plants are far appart from each other
  gen <- gen + 1
  r <- range(which(x == "#"))
  y <- c(rep(".", 4), x[r[1]:r[2]], rep(".", 4))
  names(y)[1:4] <- as.integer(names(x[r[1]])) - 4:1
  names(y)[names(y) == ""] <- as.integer(names(x[r[2]])) +1:4
  z <- y
  for (k in 3:(length(z) - 2)) z[k] <- map[sum((y[k + 0:4 - 2] == "#") * 2 ^(4:0))+1]
  x <- z
}

print(sum(as.integer(names(x[x == "#"])) + (50000000000 - gen)), 16)
