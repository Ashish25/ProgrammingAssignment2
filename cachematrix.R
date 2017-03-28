# Caching the Inverse of a Matrix:
# makeCacheMatrix functions that are used to create a special object that 
# stores a matrix and caches its inverse.

# makeCacheMatrix function creates a special "matrix" object, further it can cache given matrix's inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            # use `<<-` to assign a value to an object in an environment 
            # different from the current environment.
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(set = set,
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


# cacheSolve function computes the inverse of the special "matrix" created by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## @x: output of makeCacheMatrix()
      ## return: inverse of the original matrix input to makeCacheMatrix()
      inv <- x$getInverse()
      
      # if the inverse has already been calculated
      if (!is.null(inv)) {
            # get it from the cache and skips the computation. 
            message("getting cached data")
            return(inv)
      }
      
      # otherwise, calculates the inverse 
      mat <- x$get()
      inv <- solve(mat, ...)
      
      # sets the value of the inverse in the cache via the setinv function.
      x$setInverse(inv)
      inv
}
