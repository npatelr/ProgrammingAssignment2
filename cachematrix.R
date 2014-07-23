## The following function creates a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <-NULL
  set<-function(y){ ## sets the values
    x <<- y 
    m <<- NULL
  }
  get <-function() x ## gets the values
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse, ## sets the values of the inverse function
       getinverse = getinverse) ## gets the values of the inverse function
}


## The following function will calculate the inverse of the vector using the function "solve"

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get() 
  m <- solve(data, ...) ##calculates inverse of matrix data
  x$setinverse(m) ##sets inverse  in cache using the solve function
  m
}