## These are functions to create a matrix (makeCacheMatrix) which has its inverse cached after first retrieval (and 
## calculation) of the inverse (cacheSolve)

## Creates and returns a list of functions, that can set and get a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Tries to retrieve a cached inverse of matrix x and if no inverse is found, 
## calculates it (using solve()), caches it for future retrieval and finally returns the inverse.
## accepts additional arguments and passes them to solve()

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}
