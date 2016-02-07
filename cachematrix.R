## Put comments here that give an overall description of what your
## functions do

# It basically returns a list vector 
# This function creates a cache matrix 
#1. set the value of matrix
#2. get the value of matrix
#3.calculate the inverse 
#4. set the inverse

makeCacheMatrix <- function(x = matrix()) {
          Inv <- NULL        
          set<- function(y) {
                 x <<- y
                 Inv <<- NULL
          }
          get<- function() x
          setInverse <- function(Inv) {
              Inv <<- Inv
          }
          getInverse <- function() Inv
          list(set = set,get=get,setInverse= setInverse,getInverse= getInverse)
  
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
        
         Inv<-x$getInverse()
          
         if(!is.null(Inv)){
            message("getting the cached data.")
             return(Inv)
           }
     data <- x$get()
     Inv <- solve(data)
     x$setInverse(Inv)
     Inv
     
}
#how an ouput looks like an example
#> source('~/ProgrammingAssignment2/cachematrix.R')
#> x<-cbind(c(1,2),c(3,4))
#> m<-makeCacheMatrix(x)
#> m$get()
#      [,1] [,2]
#[1,]    1    3
#[2,]    2    4
#> m$getInverse()
#NULL
#> cacheSolve(m)
#       [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> cacheSolve(m)
#getting the cached data.
#       [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> m$getInverse()
#      [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
 
