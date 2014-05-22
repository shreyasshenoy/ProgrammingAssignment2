## Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly 

## In this code file we introduce the <<- operator which can be used to
## assign a value to an object in an environment that is different from
## the current environment.

## Below are two functions that are used to create a special object
## that stores a matrix and cache's its inverse.

## The first function, makeCacheMatrix creates a special "matrix", which 
## is really a list containing a function to
##      1.set the value of the matrix
##      2.get the value of the matrix
##      3.set the value of the Inverse
##      4.get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(inverse) i <<- inverse
        
        getInverse <- function() i
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        

}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        
        data <- x$get()
        
        i <- solve(data, ...)
        
        x$setInverse(i)
        
        i
}

