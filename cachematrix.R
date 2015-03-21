
## 
## make/manage a structure for caching
## the inverse of a matrix 'mat'
##
## @return  matrix structure
##

makeCacheMatrix <- function(mat = matrix()) {

  mat_inverse <- NULL
  
  set <- function(y) {
    mat <<- y
    mat_inverse <<- NULL
  }
  get <- function() mat
  setinverse <- function(i) mat_inverse <<- i
  getinverse <- function() mat_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## 
## @return  a matrix that is the inverse of 'x'
##
## usage: 
##  m <- makeCacheMatrix( [matrix])
##  m$set( matrix( c(...), ...)  )
##  inv <- cacheSolve(m)
##
cacheSolve <- function(x, ...) {
       
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


