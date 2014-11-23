## Data Science - R Programming
## Programming Assignment 2
##
## The functions makeCacheMatrix and cacheSolve 
## work together to cache the result of inverting a matrix. 
## This saves time from having to do the same computation over again. 
## 
## Function makeCacheMatrix 
##         contains inner functions to cache the inverse of a matrix
## Call example: amatrix <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
## Returns: a list containing the inner functions: 
##          set        : Set the matrix and nullify the cache 
##          get        : Get the matrix
##          setinverse : Set the inverted matrix  
##          getinverse : Get the inverted matrix  
## Call makeCacheMatrix before calling the CacheSolve function
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## Function cacheSolve is called after makeCacheMatrix 
## Call example: cacheSolve(amatrix)
## Returns: The inverted matrix
## It retrieves the latest cached inverted matrix
##   or if no matrix is cached, 
##   it inverts the matrix (using the function: solve),
##   caches the resulting matrix and returns it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

