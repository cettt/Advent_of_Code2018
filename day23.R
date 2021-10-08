data23 <- gsub("[<pos=> r]", "", readLines("input/day23.txt"))
df <- do.call(rbind, lapply(strsplit(data23, ","), as.numeric))

#part 1--------
rmax <- as.numeric(df[which.max(df[,4]), ])
sum(colSums(abs(t(df[,1:3]) - rmax[1:3])) <= rmax[4])

#part2----------
zoom_square <- function(xyz) {
  disc_grid <- expand.grid(x = xyz[1]*2 + 0:1, y = xyz[2]*2 + 0:1, z = xyz[3]*2 + 0:1)
}

count_bots_zoom <- function(xyz, zoom) {
  
  if (zoom == 1) return(sum(colSums(abs(t(df[,1:3]) - xyz)) <= df[,4]))
  
    newx <- pmax(xyz[1]*zoom, df[ ,1]) - pmax(df[, 1] - (xyz[1] + 1)*zoom, 0)
    newy <- pmax(xyz[2]*zoom, df[ ,2]) - pmax(df[, 2] - (xyz[2] + 1)*zoom, 0)
    newz <- pmax(xyz[3]*zoom, df[ ,3]) - pmax(df[, 3] - (xyz[3] + 1)*zoom, 0)
 
    sum(rowSums(abs(c(newx, newy, newz) - df[,1:3])) <= df[,4])
}
#we discretize the entire space into large cubes and look inside these cubes
#we focus only on cubes which have at least maxbots bots in them
# if no such cubes are found we reduce maxbots by 1
#if such cubes are found, we zoom into the cube and analyse the 8 (2x2x2) cubes inside

for (maxbots in (nrow(df):1)) {
  zoom <- 2^27
  zoomed_grid <- expand.grid(x = 0:2 - 1, y = 0:2 - 1, z = 0:2 - 1)
  zoomed_grid$nbots <- apply(zoomed_grid, 1, count_bots_zoom, zoom = zoom)
  zoomed_grid <- subset(zoomed_grid, nbots >= maxbots)
  
  while (zoom > 1 & nrow(zoomed_grid) > 0) {
    zoom <- zoom / 2
    zoomed_grid <- do.call(rbind, apply(zoomed_grid[,1:3], 1, zoom_square))
    zoomed_grid$nbots <- apply(zoomed_grid, 1, count_bots_zoom, zoom = zoom)
    zoomed_grid <- subset(zoomed_grid, nbots >= maxbots)
  }
  if (zoom == 1) break;
}

unname(min(rowSums(abs(zoomed_grid[,1:3]))))
