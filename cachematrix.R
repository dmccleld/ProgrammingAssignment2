## These functions provide a simple caching operation for taking an n X n matrix and returns the inverse. Due to
## the longer nature of the solve function, these functions cache the inverse answer to the given matrix.

## This matrix function creates a special matrix which can be utilized by cacheSolve to solve the inverse.  Caching
## answers as they are put through the function.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() {x}
  setInv <- function(Inv) {m <<- Inv}
  getInv <- function() {m}
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Returns the inverse of the special matrix.

cacheSolve <- function(x, ...) {
        m<-x$getInv()
        
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data,...)
        x$setInv(m)
        
        m
}
