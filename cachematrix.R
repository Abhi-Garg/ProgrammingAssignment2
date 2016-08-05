## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates a matrix and returns the inverse 
## of the matrix with global defination.

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y){
    x <<- y
    m <<- NULL
}
get <- function() x
#calculate te inverse of matrix using solve function
setinverse <- function(solve) m <<- solve
getinverse <- function() m
list(set = set, get = get, setinverse = setinverse, 
     getinverse = getinverse)
}

## cacheSolve function returns the computed inverse of the matrix
## created by the above matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data - Inverse of the matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
