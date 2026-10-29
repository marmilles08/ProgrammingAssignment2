## These two functions save the inverse of a matrix, so we don't have
## to calculate it again if the matrix hasn't changed.

## makeCacheMatrix stores a matrix and later its inverse. Changing the matrix
## with set() clears the saved inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve gives us the inverse of that matrix. If it was already
## calculated, it returns the saved one. If not, it calculates it, saves it
## and returns it.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
