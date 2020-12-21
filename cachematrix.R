## The first function, makeCacheMatrix creates a list containing the function to
## set, get, setinverse and getinverse the value of a matrix


makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }


## Calculates the inverse of a matrix, but first it checks if it has already been calculated
## or if it need to solve it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  mat <- x$get()
  i <- solve(mat, ...)
  x$setInverse(i)
  i
}
