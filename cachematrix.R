## These two functions create the ablilty to cache the potentially time-consuming
## computation of the inverse of a matrix.  The first funciton returns a list
## of 4 functions that handle the matrix and its inverse.  The second function
## retreives the cached inverse matrix if previously calculated, or calculates the
## inverse if the cache is NULL.

## makeCacheMatrix creates the following:
## set:  used to modify matrix without creating a new env.
## get:  Returns the matrix
## setinv:  Solves for the inverse
## getinv:  Returns the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y       ## push the paramater "y" to the parent value of "x"
                inv <<- NULL  ## Reset the cached inverse "inv" to NULL
        }
        get <- function() x
        setinv <- function(inver) inv <<- solve(x)
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        
}
