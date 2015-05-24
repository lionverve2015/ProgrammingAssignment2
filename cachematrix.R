#Problem Assigngment 2

# createCacheMatrix makes a special matrix object, and then 
# docacheSolve calculates the inverse of the matrix.
# If the matrix inverse has been calculated, it will simply 
# find it in the cache and return it, and not re-calculate it.

createCacheMatrix <- function(x = matrix()) {
  invrs_x <- NULL
  set <- function(y) {
    x <<- y
    invrs_x <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) invrs_x <<-inverse
  getinverse <- function() invrs_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# The function docacheSolve returns the inverse of a matrix A created with
# the above function.
# If the cached inverse is available, docacheSolve retrieves it,but if not, 
# it computes, caches, and returns it.

docacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  invrs_x <- x$getinverse()
  if (!is.null(invrs_x)) {
    message("getting cached inverse matrix")
    return(invrs_x)
  } else {
    invrs_x <- solve(x$get())
    x$setinverse(invrs_x)
    return(invrs_x)
  }
}

math1 <- matrix(data = c(4,2,7,6), nrow = 2, ncol = 2)
math2 <- createCacheMatrix(math1)
docacheSolve(math2)
