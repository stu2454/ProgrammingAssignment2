##
## The functions presented here enable you to cache potentially time-consuming computations like inversion of a matrix.
##
## The functions have been written in response to the second assignment in the 
## Introduction to R programming course from Johns Hopkins University (coursera)
##
## GitHub: https://github.com/stu2454/ProgrammingAssignment2
##
## November 27, 2019
##
## makeCacheMatrix will create a 'special' matrix to cache its inverse
##
## cacheSolve computes and returns the inverse of the special matrix. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
##
##  Computing the inverse of a square matrix can be done with the `solve` function in R.


makeCacheMatrix <- function(x = matrix()) {
  #this function will create a 'special' matrix to cache its inverse
  
  invmat <- NULL         #first, initialise to NULL a matrix for the inverted matrix
  
  set <- function(y) {   #define the function 'set' to assign a new value of the matrix, if it already exists, reset to NULL
    x <<- y
    invmat <<- NULL
  }
  get <- function() x    #define the function 'get' to return the value of the matrix argument
  
  setinverse <- function(inverse) invmat <<- inverse   #set and get the inverse matrix when called
  getinverse <- function() invmat
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## Write a short comment describing this function
##
## as above, cacheSolve computes and returns the inverse of the special matrix. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

  #this function will return the matrix that is the inverse of x
  
  invmat <- x$getinverse()
  
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  
  data <- x$get()    #get the matrix
  invmat <- solve(data, ...)   #use the 'solve' function in R to invert the matrix
  x$setinverse(invmat)  #set the inverted matrix
  invmat   #return the inverted matrix
}
