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

test = assign.student("Cath")

#2
stuff = matrix(c(1:16), nrow = 4)

sort.student = function(x) {
            stuff = matrix(sample(1:100, 16), nrow=4)
            x = assign.student("x")
            finalsort <- t(stuff) %*% x
            if(max(finalsort) == finalsort[1]) {
              print("GRIFFINDOR")
            }
            if(max(finalsort) == finalsort[2]) {
              print("SLYTHERIN")
            }
            if(max(finalsort) == finalsort[3]) {
              print("RAVENCLAW")
            }
            if(max(finalsort) == finalsort[4]) {
              print("HUFFLEPUFF")
            }
}

sort.student("Harold Potter")
sort.student("Voldemort")
