# Your assignment is to write a pair of functions that cache the inverse of a matrix.
# 
# Write the following functions:
#       makeCacheMatrix: This function creates a special "matrix"
#               object that can cache its inverse.
#       cacheSolve: This function computes the inverse of the special "matrix"
#               returned by makeCacheMatrix above. If the inverse has already been
#               calculated (and the matrix has not changed), then the cachesolve should retrieve
#               the inverse from the cache.

#First function creates an object(?) made of four functions, as well as variables inv and y.
#An invertible matrix is passed through as an argument x, 
# creating the new object/function makeCacheMatrix. This doesn't create the inverted matrix,
# that is done in cacheSolve(), but it gives the user functions to be used with cacheSolve()
# so that the inverse matrix doesn't need to be calculated more than once for a given invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x', 
# x being an object/variable created by the makeCacheMatrix function.
#If an inverted matrix isn't able to be found in x, cacheSolve 'solves' it and stores it in x.

cacheSolve <- function(x, ...) {
        k <- x$getinverse()
        if(!is.null(k)) {
                message("getting cached inverse matrix")
                return(k)
        }
        data <- x$get()
        k <- solve(data, ...)
        x$setinverse(k)
        k
}
