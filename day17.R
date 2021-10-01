data17 <- readLines("input/day17.txt")

make_clay <- function(co) {
  tmp <- strsplit(co, ", ")[[1]]
  a <- as.integer(gsub("\\D", "", tmp[1]))
  b <- as.integer(strsplit(substring(tmp[2], 3, 100), "\\.\\.")[[1]])
  b_seq <- seq(b[1], b[2])
  
  if (grepl("^x", tmp[1])) a + b_seq * 1i else b_seq + a*1i
}

clay <- unname(unlist(sapply(data17, make_clay)))

streams <- 500 + 0i
settled_water <- complex()
water_vec <- complex()
null_streams <- complex()

#let the water flow---
while (length(streams) > 0) {
  for (st in streams) {
    x <- c(settled_water, clay)
    new_bound <- min(Im(x[Re(x) == Re(st) & Im(x) > Im(st)]) - 1, max(Im(clay)))
    water_vec <- c(water_vec, Re(st) + seq(Im(st), new_bound)*1i)
    null_streams <- c(null_streams, st)
    st <- Re(st) + (new_bound)*1i
    
    if ((Im(st) >= max(Im(clay)))) { #if the stream crosses the lower bound
      null_streams <- c(null_streams, st)
    } else { #otherwise the water settles or floats to left and right
      
      while (TRUE) {#while loop to simulate settling water
        x <- c(settled_water, clay)
        
        im_c <- c(Re(clay[Im(clay) == Im(st)]), range(Re(clay)) + c(-3, 3)) # fixed horizontal line at y = Im(stream)
        lb <- max(im_c[im_c < Re(st)]) #first clay to the left
        rb <- min(im_c[im_c > Re(st)]) # first clay to the right
        
        im_cp1 <- c(Re(x[Im(x) == Im(st) + 1]), range(Re(clay)) + c(-3, 3)) # fixed horizontal line at y = Im(stream) + 1
        
        check_l <- (seq(Re(st) - 1L, lb + 1L)) %in% im_cp1
        check_r <- (seq(Re(st) + 1L, rb - 1L)) %in% im_cp1
        
        
        if (all(c(check_l, check_r))) { #water settles
          #maybe this step can be made faster by speeding up the rising water
          settled_water <- c(settled_water, seq(lb + 1L, rb - 1L) + Im(st)*1i)
          st <- st - 1*1i
        } else { #if water does not settle
          lb <- max(lb + 1L, Re(st) - which(!check_l)[1], na.rm = TRUE) + Im(st)*1i
          rb <- min(rb - 1L, Re(st) + which(!check_r)[1], na.rm = TRUE) + Im(st)*1i
          
          #if there is no floor to the left and there has not been a stream yet
          if (!all(check_l) & !lb %in% water_vec) streams <- c(streams, lb)
          
          #if there is no floor to the right and there has not been a stream yet
          if (!all(check_r) & !rb %in% water_vec) streams <- c(streams, rb)
          
          water_vec <- c(water_vec, seq(Re(lb), Re(rb)) + Im(st) * 1i)
          null_streams <- c(null_streams, st)
          break
        } 
      }
    }
  }
  streams <- setdiff(streams, null_streams)
}

#part 1---------
length(unique(c(settled_water, water_vec)[Im(c(settled_water, water_vec)) >= min(Im(clay))]))

#part2-------
length(unique(c(settled_water)))
