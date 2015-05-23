## Programming Assignment #2
## Johns Hopkins Coursera R Programming Class
## Mark Sanchez
##
##
## Function: makeCacheMatrix:
## Function: cacheSolve:

## makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(solve) {
        i <<- solve
    }
    getinverse <- function() {
        i
    }
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## cacheSolve:
## Return a matrix that is the inverse of 'x'
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