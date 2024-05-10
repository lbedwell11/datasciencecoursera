## These functions create the execution of caching the matrix and then returning the inverse

## creates function to cache the inverse
makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y){
    x<<- y
    inver <<- NULL
    
  }
  get <- function() x
  setinverse <- function(solve) inver <<- solve
  getinverse <- function() inver
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## computes/retrieves the inverse of x
cacheSolve <- function(x, ...) {
  inver <- x$getinverse()
  if(!is.null(inver)){
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(inver)
  
}
