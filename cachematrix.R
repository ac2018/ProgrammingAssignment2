## The function introduces a special matrix object which caches its inverse
## Use standard Solve() to calculate the inverse
## Goal is understand <<- operator which can be used to assign a value to an object in an environment
## that is different from the current environment

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL   ##clear the cache
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
# Define function to get the inverse
  getinverse <- function() m
  
# list of above 4 functions
  list(set = set, get = get,
       setinverser = setinverse,
       getinverse = getinverse)

}


## This function is calcuate the matrix inversion if already not done so
# The following function calculates the inverse of the matrix created with the above function.
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse and sets the value of the matrix inversion in the cache via the Setiverse function.

cacheSolve <- function(x) {
  m <- x$getinverse()  # fetch the cached value

# if condition to check if cahce is empty or not
  if (!is.null(m))
  {
    message(" getting cached data")
    return (m)
    
  }
## as the cache is empty calculate and fill the cache
  data <- x$get()
#Inverse the matrix by using Solve
  m <- solve(data)
# now set this in cahce
  x$setInverse(m)
  m  ## Return a matrix that is the inverse of 'x'
}
