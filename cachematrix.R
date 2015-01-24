## The makeCacheMatrix() function can be used to generate a new vector
## containing information of an invertible matrix and an indicator stating
## if the matix has been calculated.
## Example 1a (new setting mode): 
## > newVec <- makeCacheMatrix(matrix(1:4,2,2))
## Example 1b (resetting mode):
## > usedVec$set(matrix(1:4,2,2))


makeCacheMatrix <- function(x = matrix()) {
  IMTX <- NULL
  set <- function(y) {
    x <<- y
    IMTX <<- NULL
  }
  
  get <- function() x
  setIMTX <- function(invM) IMTX <<- invM
  getIMTX <- function() IMTX
  list(set = set, get = get,
       setIMTX = setIMTX,
       getIMTX = getIMTX)
}


## The cacheSolve() function can take the vector made by makeCacheMatrix() as
## an argument to calculate the inverse matrix and store the result in the vector.
## Example:
## > cacheSolve(newVec)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

## If the same vector is processed by cacheSolve() again, the stored result will
## show up rather than repeat the calculation.
## Example:
## > cacheSolve(newVec) 'the 2nd time'
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5


cacheSolve <- function(x, ...) {
  IMTX <- x$getIMTX()
  if(!is.null(IMTX)) {
    message("getting cached data")
    return(IMTX)
  }
  data <- x$get()
  IMTX <- solve(data, ...)
  x$setIMTX(IMTX)
  IMTX
## Return a matrix that is the inverse of 'x'
}
