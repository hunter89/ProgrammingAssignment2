## Writing R code so that the program is able to cache the inverse 
## of a square non-singular matrix.

## Function to create a special "matrix" that contains a 
## list to perform caching operations on matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Function calculates the inverse of the special "matrix"
## Returns the cached value if inverse is already cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  mat <- x$get()
  inverse <- solve(mat, ...)
  x$setinverse(inverse)
  inverse
}
