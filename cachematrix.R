## The following fuctions cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

    ## i represents the cached inverse
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## gets the original matrix
    get <- function() x
    
    ## sets the inverse to the calculated solve function and assigns the cached inverse to i
    setinverse <- function(solve) i <<- solve
    
    ## gets the cached inverse (i)
    getinverse <- function() i
    
    list(set = set, get = get,
         getinverse = getinverse,
         setinverse = setinverse)
    
}


## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
## If the inverse has already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache (and print "getting cached data" in the process).
cacheSolve <- function(x, ...) {
    ## set i to the inverse function of the matrix x
    i <- x$getinverse()
    
    ## if i is set already (the inverse is cached) return the cached value
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## if the inversed is not cached then calculate it and cache it
    data <- x$get()
    
    i <- solve(data, ...)
    
    ## set the cached inverse to the calculated one above
    x$setinverse(i)
    
    ## return inverse
    i
}
