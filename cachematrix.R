## Two functions to help you create a matrix with basic functions
## such as get, set, setinverse, and getinverse. It also has
## ability to cache the inverse of the matrix and always checks
## the cache first for efficiency.

## Creates a list of helping functions to help manage the matrix
## state. 

makeCacheMatrix <- function(x = matrix())
{
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the inverse of x by first checking the cache and then
## computing if necessary

cacheSolve <- function(x, ...) 
  ## Return a matrix that is the inverse of 'x'
{
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
}
