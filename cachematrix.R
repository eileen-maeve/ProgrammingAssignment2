## These are two functions for finding the inverse of a matrix. cacheSolve either 
## calculates the inverse of the matrix or retrieves it from a cache if it has already
## been calculated. makeCacheMatrix creates a list of functions used by cacheSolve.

## makeCacheMatrix takes a square invertible matrix as its argument, and returns a list
## of functions that 1. sets the matrix to be inverted (set) 2. gets the matrix to be
## inverted (get) 3. sets the inverse (setinverse) 4. gets the inverse (getinverse)


makeCacheMatrix <- function(x = matrix()) {
  # I have a more complicated statement to initialize inv (formerly m in the example), 
  # because it is now a matrix rather than a number.
  inv <- matrix(data = NULL, nrow = nrow(x), ncol = ncol(x))
  set <- function(y) {
    x <<- y
    inv <<- matrix(data = NULL, nrow = nrow(x), ncol = ncol(x))
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve takes the list returned by makeCacheMatrix as its argument, as well as
## any extra arguments needed by the solve() function. It will calculate the inverse of
## the matrix passed to makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
