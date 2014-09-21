## This function initializes a list of 4 functions: set(), get(), setinverse(), getinverse() 
## The last 3 functions: get(), setinverse(), getinverse() are supposed to be used by cacheSolve() function only
## User can use the set() function to set a new matrix after initialization
## The set() function checks if the new matrix is identical to the old matrix. If so, the new value will not be set

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    if (!identical(x, y)) {
      x <<- y
      inv <<- NULL
    }
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function first checks if the inverse has been cached earlier. If so, it will return the cached value 
## Or else, it will invoke solve() to calculate the inverse matrix and return the results

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}