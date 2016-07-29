## provide efficient way to solve (inverse) matrix

## creates a matrix wrapper which can store inverse matrix inside it
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL #this is storing inverse
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get= get, 
        setinverse = setinverse, getinverse = getinverse)
}

## a cache which provides inverse of a givne matrix and store it inside for faster recovery


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i 
}
