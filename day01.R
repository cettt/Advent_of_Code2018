data01 <- read.table("input/day01.txt")[,1]

# part1------
sum(data01)

#part2--------
#duplicated entries must have a difference which is a multiple of sum(data01)
temp <- outer(cumsum(data01), cumsum(data01), `-`)
cumsum(data01)[which(temp > 0 & temp %% sum(data01) == 0, arr.ind = TRUE)[1,1]]