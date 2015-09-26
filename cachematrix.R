#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
## these functions are used to cache the result of <<<
## matrix inversion in order to avoid unecessary   <<<
## calculations                                    <<<
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


################################################
## input:  one matrix [x]                      #
## output: a list of 4 functions that can set  #
## or get the input matrix and its inverse,    #
## declared inside the function.               #  
################################################
makeCacheMatrix <- function(x = matrix()) {
  matrixInverse <- NULL
  set <- function(matrix) {
    x <<- matrix
    matrixInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) matrixInverse <<- solve(x)
  getInverse <- function() matrixInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

######################################################
## returns the inverse of the matrix x               #
## if inverse already in cache then the cached value #
## is returned                                       #
######################################################
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message('getting cached data')
    return(inv)
  }
  
  # if there's no cached data,
  # we calculate it and store it
  # in cache before returning it
  inv <- solve(x$get(), ...)
  x$setInverse(inv)
  return(inv)

}
