## The first function 'makeCacheMatrix' is used to create a cache Matrix, 
## that can then be used as the input of the second function 'cacheSolve'.


## Function 'makeCacheMatrix': takes a square matrix as an input and puts it in a cached matrix
## example of use: 
## myMatrix <- matrix(rnorm(9), 3, 3)
## myCachedMatrix <- makeCacheMatrix(myMatrix)

makeCacheMatrix <- function(x = matrix()) {
  # Basically, the code is similar to the 'makeVector' function of the assignment
  # Except that we use function 'solve' instead of 'mean'
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Function 'cacheSolve': calculates the inverse of a square matrix. 
## If the matrix is already in cache, it does not get calculated again.
## example of use: 
## myMatrix <- matrix(rnorm(9), 3, 3)
## myCachedMatrix <- makeCacheMatrix(myMatrix)
## cacheSolve(myCachedMatrix)
## The second time this 'cacheSolve' function is used in this example, you will get the message "getting cached data"

cacheSolve <- function(x, ...) {
  # Basically, the code is similar to the 'cachemean' function of the assignment
  # Except that we use function 'solve' instead of 'mean'
  m <- x$getsolve()
  if(!is.null(m)) {
    # in case the matrix is already in cache, we use it
    message("getting cached data")
    return(m) #exits the current function, and returns the cached matrix that is the inverse of 'x'
  }
  data <- x$get()
  # in case the matrix is NOT already in cache, we compute its inverse
  m <- solve(data, ...)
  x$setsolve(m)
  m  ## Returns the (calculated) matrix that is the inverse of 'x'
}
