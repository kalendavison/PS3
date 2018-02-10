# Activity 5: Sorting Hat

#1 Students

assign <- function(name) {
  useMethod("assign", name)
}

assign.student <- function(name) { #takes input name and returns a vector, representing their characteristic scores on a scale of 1-100, of class "student"
  courage <- sample(1:100, 1) 
  ambition <- sample(1:100, 1)
  intelligence <- sample(1:100, 1)
  effort <- sample(1:100, 1) #randomly assigns a score 1-100 to each characteristic
  a <- (c(courage, ambition, intelligence, effort)) 
  class(a) <- "student" #makes the vector of class "student"
  return(a)
}

test = assign.student("Cath") 
test #vector of four numeric values of class "student." 

#2
sort.student = function(x) { #function that takes in object of class student and returns a house name based upon the characteristic that has the highest value
            stuff = matrix(sample(1:100, 16), nrow=4) #random 4x4 matrix with values 1:100
            x = assign.student() #input x is the output of the assign.student function made above. Four value vector of class "student".
            finalsort <- t(stuff) %*% x #transpose matrix
            if(max(finalsort) == finalsort[1]) {
              print("GRYFFINDOR") #if the first value, which refers to courage, is the largest, return "GRYFFINDOR"
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
sort.student("Voldemort") #tests successfully return a random house


#3
sort.student.house = function(x) { #function returns the vector of characteristics of class "student" and "(house name)"
  stuff = matrix(sample(1:100, 16), nrow=4)
  x = assign.student()
  finalsort <- t(stuff) %*% x
  if(max(finalsort) == finalsort[1]) {
    class(x) = c("student", "GRYFFINDOR") 
    return(x) #if the max value in the vector pertains to courage, change the class to "student" and "GRYFFINDOR"
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

class(sort.student.house("Bob")) 
test = sort.student.house("Ron")
test 

#4
"Gryffindor_Tower" = new.env()  
"Black_Lake" = new.env()
"Ravenclaw_Tower" = new.env()
"Basement" = new.env() #created four new environments

curfew = function(x){
  UseMethod("curfew", x) #created a generic curfew function
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
  Basement <- x #created four methods for each class of data. Places the name in the environment that corresponds with class type.
}

curfew(test)
ls.str(Gryffindor_Tower) #successful

##### Problem Set 3 #####

#in S3
choice = as.numeric(sample(1:3, 1))
class(choice) = "door" #choice is a value 1:3 of class "door"
choice

PlayGame = function(x){
  UseMethod("PlayGame", x)
}

PlayGame.door = function(x){ #function for objects of class "door." Returns "Congratulations!" if the right door is chosen and "sorry." if not.
  true <- as.numeric(sample(1:3, 1))
  if (true == choice) {
    print("Congrulations!")
    } else
      print("Sorry.")
}

PlayGame.door(x) #successful. Returns different result each time game is played.

#S4

setClass(Class = "door(S4)", #creates door class in S4, numeric data.
         representation = representation(
           door = "numeric"
         ),
         prototype = prototype(
           door = c()
         )
)

setValidity("door(S4)", function(object){ #checks that door is a numeric value 1:3, returns "@door not valid" if not
  check1 = (object@door ==1 | object@door ==2 | object@door == 3)
  check2 = (length(object@door) == 1)
  if(!check1 | !check2){return("@door not valid")}
}
)

setMethod("initialize", "door(S4)", function(.Object, ...) { #initializes 
  number = callNextMethod()
  validObject(number)
  return(number)
})

new("door(S4)", door= 1)

setGeneric("PlayGameS4", #creates a generic function in S4 that takes objects of class "door(S4)" as arguments
      function(object="door(S4)") {
      standardGeneric("PlayGameS4")
           } )

setMethod("PlayGameS4", "door(S4)", #creates a method for the above generic function for class "door(S4)". 
        function(object){
            true = as.numeric(sample(1:3,1))
            if(object@door == true){
              print("Congratulations!")
            }else{
              print("Sorry.")
            } 
   } )

choiceS4 <- new("door(S4)", door= 3) 
PlayGameS4(choiceS4) #returns "Congratulations!" if the doors match and "Sorry." if they don't.

