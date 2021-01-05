## The makeCacheMatrix intially sets and gets the new matrix, that is going to be inversed. 
## The setsolve function then inverts the the matrix "x" and assigns it to m.
## The new value of m is then got by the getsolve function.
## Basically, it works analog to the maekVector function, which was provided as
## as an example.


makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      
      set <- function(y){
        x <<- y
        m <<- NULL
      }
      
      get <- function() x
      setsolve <- function(solve) m <<- solve (x)
      getsolve <- function() m
      
      list(set = set, get = get, setsolve = setsolve, 
           getsolve  = getsolve)
}


## The cacheSolve function first gets m.
## The if loop checks whether m is not null, so the inverse matrix has
## already been assigned to m. If this is the case, it will just return m.
## If  m is equal to null, cacheSolve will get the current value of x,
## calculate its inverse and then set m to the inverse of x, before it returns
## m.


cacheSolve <- function(x, ...) {
  
  m <- x$getsolve()
  
  if(is.null(m)){
    message("getting cached data")
    return(m)
  }
    
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
    
}
