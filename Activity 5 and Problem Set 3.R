# Activity 5: Sorting Hat

#1 Students

assign <- function(name) {
  useMethod("assign", name)
}
assign.student <- function(name) {
  courage <- sample(1:100, 1)
  ambition <- sample(1:100, 1)
  intelligence <- sample(1:100, 1)
  effort <- sample(1:100, 1)
  a <- (c(courage, ambition, intelligence, effort))
  class(a) <- "student"
  return(a)
}
assign.student("Cathy")

#2

setMethod("sort", "student",
          function(x) {
            a <- sample(1:100, 4)
            x<- matrix
            finalsort <- t(x) %*% a
            if(max(finalsort) == finalsort[1]) {
              print("G")
            }
            if(max(finalsort) == finalsort[2]) {
              print("S")
            }
            if(max(finalsort) == finalsort[3]) {
              print("R")
            }
            if(max(finalsort) == finalsort[4]) {
              print("H")
            }
          }
          
          sort("Cathy", diag(4))
          
          