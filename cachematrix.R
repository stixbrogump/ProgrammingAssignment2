## Put comments here that give an overall description of what your
## functions do
## There are 2 functions here: makeCacheMatrix which will invert a given matrix and cache it
## cacheSolve will retrieve the cache matrix if it exists, or will create and cahce it if it doesnt
##IMPORTANT - only works for scquare matrices!
##IMPORTANT - ONLY works for invertible matrices, see example of how to run functions at bottom of script

##makeCacheMatrix
## This function sets \ gets the inverse of a matrix passsed in as mMatrix

makeCacheMatrix <- function(mMatrix = matrix()) {
  ##Create an empty matrix
  mMatrix2 <- NULL
  set <- function(y) {
    ## Set mMatrix in the global scope
    mMatrix <<- y
    ## Set mMatrix2 in the Global scope
    mMatrix2 <<- NULL
  }
  get <- function() mMatrix
  ## Invert Matrix and store it in the Global scope
  setMatrixInverse <- function(solve) mMatrix2 <<- solve
  getMatrixInverse <- function() mMatrix2
  list(set = set, get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}

##CacheSolve 
##This function retrieves a matrix from cache (stored in the global scope) if it has been set previously
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

## If you wish to run this code, you can use the sample below
##bob <-makeCacheMatrix()
##bob$set(matrix(c(0,1,0,1,0,1,2,1,0),3,3)) 
##cacheSolve(bob)
## run cacheSolve(bob) twice to get teh "getting cached data" message