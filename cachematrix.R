## The following functions are used to create a special object 
## that stores a numeric matrix and caches its inverse.

## This function creates a special "matrix", set the value of the 
## matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Return a list of getter & setter functions: get the value of the 
  ## matrix, get the value of its inverse, set the value of the matrix,
  ## set the value the inverse.
  inver <- NULL
  set <- function(y){
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver
  list(set = set, get = get, setinverse = setinverse, 
       getinverse = getinverse)

}


## This function calculates the inverse of the special "matrix" 
## created with the above function if it has not already been calculated

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inver <- x$getinverse()
  if(!is.null(inver)){
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setinverse(inver)
  inver
}
