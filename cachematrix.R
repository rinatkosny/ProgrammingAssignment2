## Put comments here that give an overall description of what your
## functions do
# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 
# (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.
# makeCacheMatrix: 	This function creates a special "matrix" object that can cache its inverse.
#
# cacheSolve: 		This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
					then the cachesolve should retrieve the inverse from the cache.

# to test
# xm = matrix( c(10,0,0, 0,10,0, 0,0,10),3,3);xm ;solve(xm)
# zm <- makeCacheMatrix (xm);zm
# cacheSolve (zm)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m		
}



xm = matrix( c(10,0,0, 0,10,0, 0,0,10),3,3);xm ;solve(xm)
zm <- makeCacheMatrix (xm);zm
cacheSolve (zm)