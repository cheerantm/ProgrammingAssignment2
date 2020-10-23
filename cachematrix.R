# This code is supposed to find the inverse of a matrix. If the input matrix is 
# the same as the previous one, then it doesn't recalculate inverse, but takes
# inverse from the cache

# This function takes sets up the value for the matrix and passes the set, get,
# setinverse and getinverse as a list to its parent environment 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# this function takes object of makeCacheMatrix type and calculates the inverse
# of the matrix if it doesn't already exist and passes the inverse to 
#makeCachematrix through setinverse function. if the inverse already exists then
# it just retrieves it from makeCachematrix through getinverse function 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting inversed matrix data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
