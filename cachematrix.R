## makeCacheMatrix function stores x and inv in an enclosed environment of the set,get,
## setInverse, getInverse fucntions. 

## In this step, makeCacheMatrix function creates a special “matrix” object that can cache its inverse:

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function() inv <<- solve(x) #calculates the inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
bee <- makeCacheMatrix()
bee$set(matrix(1:4,2))
bee$get()
bee$setInverse()
bee$getInverse()

## In this step, cacheSolve function computes the inverse of the special “matrix” returned 
## by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i

## creating a matrix
bee1 <- matrix(c(1:4),2,2)
## getting the inverse after computation
bee2 <- makeCacheMatrix(bee1)
## getting the inverse after cache
cacheSolve(bee2)
