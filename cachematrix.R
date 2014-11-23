## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than computing it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
## This program contains a pair of functions that cache the inverse of a matrix.
## Created: Nov. 23, 2014 15:54




## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        inverseMat <- NULL
        set <- function(y) {
                x <<- y
                inverseMat <<- NULL
        }
        get <- function() x
        setInverse <- function(y) inverseMat <<- y
        getInverse <- function() inverseMat
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMat <- x$getInverse()
        if(!is.null(inverseMat)) {
                message("getting inverse matrix")
                return(inverseMat)
        }
        originalMat <- x$get()
        inverseMat <- solve(originalMat, ...)
        x$setInverse(inverseMat)
        inverseMat		
}
