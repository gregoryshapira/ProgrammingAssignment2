## Programming Assignment 2
## Cached matrix inverse


makeCacheMatrix <- function(x = matrix()) {

    matinverse <- NULL
    set <- function(y) {
      x <<- y
      matinverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) matinverse <<- inverse
    getinverse <- function() matinverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
  
  
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

 
    matinverse <- x$getinverse()
    if(!is.null(matinverse)) {
      message("getting cached data")
      return(matinverse)
    }
    data <- x$get()
    matinverse <- solve(data, ...)
    x$setinverse(matinverse)
    matinverse
  
  
  
} 



