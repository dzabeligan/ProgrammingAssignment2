## Caching computed inverse matrices to improve performance of CPu
## Computes the inverse of matrix x

## Creates variables, matrix x and inverse of x in environment
## Returns a list of functions to set and get these variables

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(solve) inv <<- solve
  get_inverse <- function() inv
  list(set = set, get = get, set_inverse = set_inverse, 
       get_inverse = get_inverse)

}


## Computes inverse of matrtix if inv is null and sets inv.
## Otherwise, retrieves value of inv.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inverse()
  if(!is.null(inv)) {
    message('getting cached data')
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inverse(inv)
  inv
}
