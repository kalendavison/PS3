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
test

#2
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


#3
sort.student.house = function(x) {
  stuff = matrix(sample(1:100, 16), nrow=4)
  x = assign.student()
  finalsort <- t(stuff) %*% x
  if(max(finalsort) == finalsort[1]) {
    class(x) = c("student", "GRYFFINDOR")
    return(x)
  }
  if(max(finalsort) == finalsort[2]) {
    class(x) = c("student", "SLYTHERIN")
    return(x)
  }
  if(max(finalsort) == finalsort[3]) {
    class(x) = c("student", "RAVENCLAW")
    return(x)
  }
  if(max(finalsort) == finalsort[4]) {
    class(x) = c("student", "HUFFLEPUFF")
    return(x)
  }
}

class(sort.student.house("djf"))
test = sort.student.house("tkjad")
test

#4
"Gryffindor_Tower" = new.env() 
"Black_Lake" = new.env()
"Ravenclaw_Tower" = new.env()
"Basement" = new.env()

curfew = function(x){
  UseMethod("curfew", x)
}


curfew.GRYFFINDOR = function(x){
  Gryffindor_Tower$student <- x
}
  
curfew.SLYTHERIN = function(x){
  Black_Lake$student <- x
}
 
curfew.RAVENCLAW = function(x){
  Ravenclaw_Tower <- x
}

curfew.HUFFLEPUFF = function(x){
  Basement <- x
}

curfew(test)
ls.str(Gryffindor_Tower) #successful
