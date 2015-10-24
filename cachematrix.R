######Coursera: R Programming Assignement 2########

## These functions take a square, invertable matrix, and check to see if the inverse is stored in a cache.
## If not stored already they return the inverse and store it in the cache. 

##makeCacheMatrix function creates a list of functions that stores i, the inverse of matrix x 
##in a parent environment
makeCacheMatrix <-function(x = matrix()) { #create object x in parent environment
  i <- NULL # create object i in parent environment
  set <- function(y) { #creates daughter environment new argument y
    x <<- y            #takes the argument y and stores it in the parent environment where x is defined
    i <<- NULL         #makes a place holder for the inverse,i, in same parent environment as x
  }
  get <- function() x  #calls x to verify if in the parent environment
  setinverse <- function(inverse) i <<- inverse #This places solve, the inverse of x, in the parent environment
  getinverse <- function() i #calls i
  list(set = set, get = get, #gives list names the same name as their object identified in this function
       setinverse = setinverse,
       getinverse = getinverse)
 
}

## cacheSolve checks to see if the inverse of x is already cached, if not it solves the matrix

cacheSolve <-function(x,...){
  i <-x$getinverse() #assigns i the values the function getinverse() from the parent x
    if(!is.null(i)){   #if there is a value in i then it will print the message and return i
      message("getting cached data")
      return(i)
    }
  data<-x$get() #assigns data the values of the function get() from the parent x 
  i<-solve(data,...) #assigns i the inverse matrix values stored tempararily in data
  x$setinverse(i) #
  i
  }

## Confirmation that code caches matrix and evaluates matrix
dat<-c(1,2,3,3,2,1,2,1,3) #from Wolfram, square invertible matrix
m <-matrix(dat,nrow=3,ncol=3,byrow=TRUE) 
mat<-makeCacheMatrix(m)
cacheSolve(mat) # should return the matrix if invertible
cacheSolve(mat) #should return "getting cached data" message since matrix m already cached

