## Functions that cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" 
## object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # On object creation, empty cached values and assign cachedMatrix
  # by  parameter
  cachedMatrix  <<- x
  cachedInvMatrix <<- NULL
  
  #set the cached matrix, empty the cached inverse matric
  setMatrix <- function(x) {
    cachedMatrix <<- x
    cachedInvMatrix <<- NULL
  }
  #get the cached matrix
  getMatrix <- function() cachedMatrix
  
  #set the cached inverse matrix
  setInvMatrix <- function(theInverse) cachedInvMatrix <<- theInverse
  
  #get the cached inverse matrix
  getInvMatrix <- function() cachedInvMatrix
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}


## cacheSolve function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInvMatrix()
  if(!is.null(m)) { 
      ##  retrieve the inverse from the cache
      message("getting cached inv matrix")
      return(m)
  }
  else  {
      ##computes the inverse and cache
      message("Compute matrix and cache the inverse")
      workCachedMatrix <- x$getMatrix() #get the cached Matrix
      m <- solve(workCachedMatrix, ...)
      x$setInvMatrix(m)
      m
  }
}
