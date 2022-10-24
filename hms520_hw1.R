##################
# HMS 520 - Assignment 1
##################

#### Problem 1: Create Variables
hometown <- 'Seattle'
my_name<-'Caroline'

num_push_ups<-4

if (num_push_ups < 5) {
  need_exercise <- TRUE
} else { 
  need_exercise<-FALSE
}
  
#### Problem 2: Create Vectors
vec1<-c(1,2,3,4,5)
vec2<-rep(10,times=100)
vec3<-rep(vec1,times=2)
vec4<-rep(vec1, each=2)
vec5<-seq(0,1,0.01)
  
#### Problem 3: Vector Operations
is.atomic(rivers) #returns TRUE, rivers is an atomic vector
is.list(rivers) #returns FALSE

typeof(rivers) #returns "double" (decimal of at least 2 decimal places)

log_rivers<-sapply(rivers,log)
  
summary<- c(
  length=length(log_rivers)
  ,sum=sum(log_rivers)
  ,mean=mean(log_rivers)
  ,median=median(log_rivers)
  ,min=min(log_rivers)
  ,max=max(log_rivers)
  )
##checking below
#names(summary)
#summary[("sum")]

log_rivers<-sort(log_rivers,decreasing=FALSE)

trimmed_log_rivers<-
  c(
   (head(sort(log_rivers, decreasing=TRUE), 10)) #top 10 largest
   ,(head(log_rivers, 10)) #top 10 smallest
   )
  
  
trimmed_summary<-c(
    length=length(trimmed_log_rivers)
    ,sum=sum(trimmed_log_rivers)
    ,mean=mean(trimmed_log_rivers)
    ,median=median(trimmed_log_rivers)
    ,min=min(trimmed_log_rivers)
    ,max=max(trimmed_log_rivers)
  )


#### Problem 4: Create List
info<-list(
  x = c(5, 6, 7, 8)
  , y = c("a", "b", "c", "d")
)

info[["y"]] <- c(1, 2, 3, 4) 

mean_x<-mean(info$x) #assuming want just a variable
mean_y<-mean(info$y) #assuming want just a variable
mean_both<-list(mean_x=mean_x,mean_y=mean_y) #assuming we want to assign this to a NEW list
##check
#mean_both[["mean_x"]]

info[["log_x"]] = log(info$x)
##check
#info[["x"]]

distance = sqrt(info$x^2 + info$y^2)
info[["distance"]]=distance
     
info[["log_x"]]<-NULL
 
#### Problem 3: String Vector
words<-c("brilliant", "joy", "positivity", "strength", "health", "insight")

words_of_the_day<-paste(words,"is the word of the day!")

a_h_words<-words[substring(words,1,1) %in% letters[1:8]]

h_y_words<-words[substring(words,nchar(words)) %in% letters[8:25]]


