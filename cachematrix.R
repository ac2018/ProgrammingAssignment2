## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverser = setinverse,
       getinverse = getinverse)

}


## This function is calcuate the matrix inversion if already not done so

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m))
  {
    message(" getting cached data")
    return (m)
    
  }
## as the cache is empty calculate and fill the cache
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
  
        ## Return a matrix that is the inverse of 'x'
}
