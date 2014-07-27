
# Using R environments to create a special cached matrix

## function sets, gets a special cached matrix and its inverse
## using a list

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  } 
  get <- function() x
  setinverse <- function(solve) im <<- solve
  getinverse <- function() im
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}


## function computes the inverse of a special cached matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getinverse()
  if(!is.null(im)) {
    message("getting cached inverse matrix")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setinverse(im)
  im
}
