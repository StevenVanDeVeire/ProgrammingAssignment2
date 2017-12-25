## Computing the inverse of a matrix is usually a costly computation. In order to avoid repeating an identiacal
## computation over and over again, the result of the computation is stored in cache. A special matrix is used
## to facilitate this process.

## The function creates a special "matrix", which is really a list containing a function to:
## 1. Get a matrix
## 2. Set a matrix
## 3. Get the value of the inverse
## 4. Set the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function returns the inverse of a matrix created with the function above.
## First a check is done if the inverse of the matrix is already present in the cache.
## If that is the case, the inverse in the cache is returned (together with a message that the cache was used).
## If that is not the case, the inverse if calculated with the solve function from R.
## The result is then stored in the cache (so it can be used the next time) and returned.
cacheSolve <- function(x, ...) {
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