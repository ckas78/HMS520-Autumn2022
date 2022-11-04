#######################################
## Problem 1
#######################################

# is_string <-function(x) {
#   if class()=="character" {
#     return('TRUE')
#   } else {
#   return ('FALSE')   
#   }
# }

is_string <-function(x) {
  return (is.character(x) & length(x) == 1) #will automatically return true/false using the & and or
}

## check
# is_string(c("two",2))
# is_string(4)
# is_string("true)

#######################################
## Problem 2
#######################################
# whatever "if" is evaluating needs to be in ()
# whatever it returns needs to be in {}
# for if statements "{}" means "then"
#need to define outside of for loop, because anything in the for loop only exists there
# c() to make empty vector
# but vector() gives you more use
# want the new empty vector, that will be our output vector, to be same length as x
# with empty vector, need to do y[i]=x[i]+y[i-1]
# if did y<-x, could do y[i]+y[i-1]
# best practice for i in something, use i in seq_along

my_cumsum<-function(x){
  if (is.numeric(x)==FALSE){
    stop("x must be a numeric vector") 
  } 
  y<-vector("double",length=length(x)) 
  for (i in seq_along(x)) {
    if (i==1) {
      y[i]=x[i]
    }
    else {
      y[i]=x[i]+y[i-1] 
    }
  }  
  return (y)
}

##check
# my_cumsum(c(1,2,3))
# my_cumsum(c("1","2","3"))

#######################################
## Problem 3
#######################################
#na_rm is a variable we are making here, if user specifies true it ignores NA values in x. default to be true
#na.rm is what is used in functions by other people does same thing.
# writing na_rm=TRUE in function argument defines na_rm and specifying its value to true means that is the default for the function
# If (na_rm) is equivalent to na_rm==TRUE, in any programming language if you write "if (1)" or if (3) or if(TRUE), will return as true
# If you write "if(0)" or if(FALSE) will return as 0
# in R, with "if" statement, don't need else
#x[i] refers to value, at index i, i refers to index
# print means print string out, return means return a calculated value

rmse <- function(x,na_rm=TRUE){
  if (is.numeric(x)==FALSE){
    stop("x must be a numeric vector") 
  } 
  if (na_rm){
    x<-x[!is.na(x)]
  } 
  return (sqrt(sum((x^2))/length(x)))
}

##check
# x<-c(1, 2, 3)
# sqrt(sum((x^2))/length(x))
# rmse(x)

#######################################
## Problem 4
#######################################
#invisible() function 
#past0 - concatenate vectors after converting to character

describe_difference<-function(x,y){
  if (is.character(x) & (length(x) == 1) == FALSE) {stop("x must be a string") }
  else if (is.character(y) & (length(y) == 1) == FALSE) {stop("y must be a string") }   
  invisible(N <- (nchar(x) - nchar(y)))
  Absolute_N <- abs(N)
  if (N > 0) {print(paste0("Your first string is longer by ", N, " characters."))}
  if (N < 0) {print(paste0("Your second string is longer by ", Absolute_N, " characters."))}
  if (N == 0) {print("Your strings are the same length")}
}
  
##check
# x=1
# y=2
# x='twosie'
# y='one'
# x='two'
#describe_difference(x,y)

