## These two functions work together to cache the inverse of a matrix x
## The first function creates a vector of functions used to store the 
## matrix and its inverse
## The second function calculates the inverse and returns it to the vector
## function for it to be cached

## This function takes in a matrix x and returns a function vector
## The get() function returns the matrix x
## The setinverse() function caches the inverse of x is called by 
## cacheSolve() once it has defined the inverse
## The getinverse() function returns the inverse of x if it is 
## cached, otherwise it returns NULL

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes in a matrix x and returns its inverse
## If the inverse has been cached, it returns the cached solution
## without solving it again. 
## If the inverse has not been cached, it computes the inverse and 
## caches it using the setinverse() function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}