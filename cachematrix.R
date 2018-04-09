## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set_mat <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get_mat <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set_mat = set_mat, get_mat = get_mat,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get_mat()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
