## This function creates a special matrix that stores its inverse. 
## You can set and get the matrix and its inverse, which speeds up calculations when you need the inverse multiple times.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL  
    set <- function(y) {
        x <<- y
        m <<- NULL  
    }
    get <- function()        x
    setInverse <- function(inverse)      m <<- inverse
    getInverse <- function()      m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



## The cacheSolve function finds the inverse of a matrix created by makeCacheMatrix. 
## It checks if the inverse is already cached; if so, it uses the cached value. If not, it calculates the inverse, stores it, and then returns it.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}

