## These two functions cache the inverse of a matrix by creating a special 
## object to store a matrix and then caches its inverse

## Creates a matrix as a special object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
get <- function() x
setinv <- function(inverse) m <<- inverse
getinv <- function() m
list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## Computes the inverse of the matrix created by the above function.
## If the inverse was already calculated it will retrieve the data from the 
## cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinv(m)
  m
}
