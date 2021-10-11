data19 <- readLines("input/day19.txt")

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
    res <- bitwAnd(inp[inst[1] + 1L], inp[inst[2] + 1L])
  } else if (opcode == "bani") { 
    res <- bitwAnd(inp[inst[1] + 1L], inst[2])
  } else if (opcode == "borr") {
    res <- bitwOr(inp[inst[1] + 1L], inp[inst[2] + 1L])
  } else if (opcode == "bori") { 
    res <- bitwOr(inp[inst[1] + 1L], inst[2])
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

oc <- gsub(" \\d+", "", data19[-1])
inst <- lapply(strsplit(gsub("^.{4} ", "", data19[-1]), " "), as.integer)
ip_r <- as.integer(sub("\\D+", "", data19[1]))

#brute force takes way to long, especially for part 2
# a closer inspection of the program shows that after a few iterations the program
# enters a loop:
#ip = c(3, 4, 5, 6, 8, 9, 10, 11) # and repeats with 3, 4, 5 again.
# A close inspection of this program reveals the following algorithm (for part2)
#start with inp = c(0, 10551410, 1, 1, 1, 3) (after 22 iterations this is the starting point)

# Step 1: Check if inp[3]*inp[4] = inp[2] (= 10551410)
# Step 2-no  If not increase inp[3] by one. 
#    If inp[3] is larger then inp[2] go to step 3.
#    Otherwise go to step 1.
# Step 2-yes Add inp[3] to inp[1]
# Step 3: increase inp[4] by one and set inp[3] to 1. 
# Step 4: Check if inp[4] > inp[2]
# Step 5-no: Go back to Step 1
# Step 5-yes: Exit

#So in the end inp[1] is the sum of all factors of 10551410

hack_program <- function(reg0 = 0L) {
  inp <- integer(6)
  inp[1] <- reg0
  ip <- 0L
  
  for (i in seq_len(100L)) {
    inp[ip_r + 1] <- ip
    inp <- run_prog(inp, inst[[ip + 1]], oc[ip + 1])
    ip <- inp[ip_r + 1]
    ip <- ip + 1L
  }
  
  sum(which((inp[2] %% seq_len(inp[2])) == 0))
}

#part1------------
hack_program(0L)

#part2-------------
hack_program(1L)
