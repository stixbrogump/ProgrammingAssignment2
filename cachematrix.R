## Put comments here that give an overall description of what your
## functions do
## There are 2 functions here: makeCacheMatrix which will invert a given matrix and cache it
## cacheSolve will retrieve the cache matrix if it exists, or will create and cahce it if it doesnt
##IMPORTANT - only works for scquare matrices!
##IMPORTANT - ONLY works for invertible matrices, see example of how to run functions at bottom of script
## Write a short comment describing this function
## This function sets \ gets the invers of a matrix passsed in as mMatrix

makeCacheMatrix <- function(mMatrix = matrix()) {
  mMatrix2 <- NULL
  set <- function(y) {
    mMatrix <<- y
    mMatrix2 <<- NULL
  }
  get <- function() mMatrix
  setMatrixInverse <- function(solve) mMatrix2 <<- solve
  getMatrixInverse <- function() mMatrix2
  list(set = set, get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}


## Write a short comment describing this function
##This omment retrieves a matrix from cache if it has been set previously
##if not set previously it will invert the matrix and cache it

cacheSolve <- function(x=matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  mMatrix <- x$getMatrixInverse()
  if(!is.null(mMatrix)) {
    message("getting cached data")
    return(mMatrix)
  }
  data <- x$get()
  mMatrix <- solve(data, ...)
  x$setMatrixInverse(mMatrix)
  mMatrix  
}
## sample to use:
##bob <-makeCacheMatrix()
##bob$set(matrix(c(0,1,0,1,0,1,2,1,0),3,3)) 
##cacheSolve(bob)
## run cacheSolve(bob) twice to get teh "getting cached data" message