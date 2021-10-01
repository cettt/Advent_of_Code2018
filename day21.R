data21 <- readLines("input/day21.txt")


num2bin <- function(x) {
  if (x == 0) return(0)
  res <- integer(floor(log2(x)) + 1)
  rem <- x
  p <- floor(log2(x))
  while (p >= 0) {
    if (rem - 2^p >= 0) {
      res[p + 1] <- 1L
      rem <- rem - 2^p
    }
    p <- p-1
  }
  return(res)
}

mybitwAnd <- function(x, y) {
  a <- num2bin(min(x, y))
  b <- num2bin(max(x, y))
  
  if (length(b) > length(a)) {
    a <- c(a, rep(0L, length(b) - length(a)))
  }
  
  sum(2^(which(a == 1 & b == 1) - 1))
  
}

mybitwOr <- function(x, y) {
  a <- num2bin(min(x, y))
  b <- num2bin(max(x, y))
  
  if (length(b) > length(a)) {
    a <- c(a, rep(0L, length(b) - length(a)))
  }
  
  sum(2^(which(a == 1 | b == 1) - 1))
  
}

run_prog <- function(inp, inst, opcode) {
  if (opcode == "addr") { 
    res <- inp[inst[1] + 1L] + inp[inst[2] + 1L]
  } else if (opcode == "addi") { 
    res <- inp[inst[1] + 1L] + inst[2]
  } else if (opcode == "mulr") { 
    res <- inp[inst[1] + 1L] * inp[inst[2] + 1L]
  } else if (opcode == "muli") {
    res <- inp[inst[1] + 1L] * inst[2]
  } else if (opcode == "banr") {
    res <- mybitwAnd(inp[inst[1] + 1L], inp[inst[2] + 1L])
  } else if (opcode == "bani") { 
    res <- mybitwAnd(inp[inst[1] + 1L], inst[2])
  } else if (opcode == "borr") {
    res <- mybitwOr(inp[inst[1] + 1L], inp[inst[2] + 1L])
  } else if (opcode == "bori") { 
    res <- mybitwOr(inp[inst[1] + 1L], inst[2])
  } else if (opcode == "setr") { 
    res <- inp[inst[1] + 1L]
  } else if (opcode == "seti") { 
    res <- inst[1]
  } else if (opcode == "gtir") { 
    res <- (inst[1] > inp[inst[2] + 1L])
  } else if (opcode == "gtri") { 
    res <- (inp[inst[1] + 1L] > inst[2])
  } else if (opcode == "gtrr") { 
    res <- (inp[inst[1] + 1L] > inp[inst[2] + 1L])
  } else if (opcode == "eqir") { 
    res <- inst[1] == inp[inst[2] + 1L]
  } else if (opcode == "eqri") { 
    res <- inp[inst[1] + 1L] == inst[2]
  } else if (opcode == "eqrr") { 
    res <- inp[inst[1] + 1L] == inp[inst[2] + 1L]
  } 
  
  inp[inst[3L] + 1] <- res
  return(inp)
}

oc <- gsub(" \\d+", "", data21[-1])
inst <- lapply(strsplit(gsub("^.{4} ", "", data21[-1]), " "), as.integer)
ip_r <- as.integer(sub("\\D+", "", data21[1]))


#what does the algorithm do?---------

#1 set inp[5] <- 123
#2 set inp[5] <-  bitwAnd(inp[5], 456) = 72
#3 set inp[5] <- inp[5]==72  = 1
#4 set inp[6] <- inp[6] + inp[5] (and skip #5) otherwise we get an infinite loop
#6 set inp[5] <- 0
#outer loop start---------
  #7 set inp[4] <- bitwOr(inp[5], 65536) = 65536
  #8 set inp[5] <- 14464005
  #middle loop start--------
    #9  set inp[3] <- bitwAnd(inp[4], 255) = 0
    #10 set inp[5] <- inp[5] + inp[3] = 14464005
    #11 set inp[5] <- mybitwAnd(inp[5], 16777215) = 14464005
    #12 set inp[5] <- inp[5] * 65899 = 953163465495
    #13 set inp[5] <- mybitwAnd(inp[5], 16777215) 16270103
    #14 set inp[3] <- 256 > inp[4] = 0 #under which circumstances is this true?
    #15 set inp[6] <- inp[6] + inp[3] = inp[6] # and do not skip 16
    #16 set inp[6] <- inp[6]+ 1 and skip 17 (17 would cause a jump to ip = 28)
    #18 set inp[3] <- 0
    #inner loop_start--------
      #19     inp[2] <- inp[3] + 1 = 1
      #20     inp[2] <- inp[2]* 256 = 256
      #21     inp[2] <- inp[2]> inp[4] = 0
      #22     inp[6] <- inp[6] + inp[2]  and not skip #23
      #23     inp[6] <- inp[6]+ 1 (and skip 24; 24 would cause a jump to ip = 26) 
      #25     inp[3] <- inp[3] + 1 
      #26     inp[6] <- 17 and we go back to #19 until  (inp[3] + 1)* 256 > inp[4]
    #inner loop end-------
    #the loop finishes  (inp[3] + 1)* 256 > inp[4] in which case we have
    #19, #20, #21, #22, and then
    #24  inp[6] <- 25
    #27 inp[4] <- inp[3]. Here inp[3] is always equal to ceiling(inp[4] / 256 -1 + 1e-6)
    #28 inp[6] <- 7 causes a jump
  #middle loop end-----
  #once the middle loop ends (i.e #13#14#15#17 inp[6] <- 27)
  #29 inp[3] <- inp[1] == inp[5] (the first time this happens inp[5] = 5745418 )
  #30 inp[6] <- inp[6] + inp[3] #if 29 was false we go to #31 otherwise we are finished
  #31 inp[6] <- 6
#outer loop end------
#part1------------
inp <- numeric(6)
inp[1] <- 1L
ip <- 0L


while (ip < length(oc)) {
  inp[ip_r + 1] <- ip

    if (ip == 18) {
    inp[3] <- ceiling(inp[4] / 256 -1 + 1e-6) #we speed up the inner loop
  }
  
  if (ip == 28) {
    inp[1] <- inp[5] #once we reach ip == 28 for the first time, we modify register 0 which causes the algorithm to stop
  }
  
  inp <- run_prog(inp, inst = inst[[ip + 1]], opcode = oc[ip + 1])
  # if (ip == 18) print(inp)
  ip <- inp[ip_r + 1]
  ip <- ip + 1L
}

inp[1]

#part2------------
inp <- numeric(6)
inp[1] <- 1L
ip <- 0L

res_vec <- numeric()

while (length(res_vec) < 11000) {
  inp[ip_r + 1] <- ip
  
  if (ip == 18) {
    inp[3] <- ceiling(inp[4] / 256 -1 + 1e-6) #we speed up the inner loop
  }
  
  if (ip == 28) {
    res_vec <- c(res_vec, inp[5])
  }
  
  inp <- run_prog(inp, inst = inst[[ip + 1]], opcode = oc[ip + 1])
  ip <- inp[ip_r + 1]  + 1L
}

res_vec[which(duplicated(res_vec))[1] - 1]