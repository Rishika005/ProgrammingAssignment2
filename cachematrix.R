##The following functions are a more efficient way to calculate 
##the inverse of a matrix as it is normally a time consuming process 

## This function creates an object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m<- NULL 
    set<- function(y) {
      x<<-y
      m<<- NULL 
    }
    get<- function() x
    setmatrix <- function(matrix) m <<- matrix 
    getmatrix <- function() m
    setmatrixinverse<- function(solve) m <<- solve 
    getmatrixinverse<- function(solve) m 
    list(set = set, get = get,
         setmatrixinverse = setmatrixinverse,
         getmatrixinverse = getmatrixinverse)
}


## This function computes the inverse  returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    m <- x$getmatrixinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data<-x$get()
    m<- solve(data, ...)
    x$setmatrixinverse(m)
    m
        ## Returns a matrix that is the inverse of 'x'
}
