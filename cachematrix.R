makeCacheMatrix <- function(x = matrix()) {
     
        matrixinv = NULL
        storematrix = function(CacheMatrix) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                x <<- CacheMatrix
                matrixinv <<- NULL
        }
        retrievematrix = function() x
        storematrixinv = function(inverse) matrixinv <<- inverse 
        retrievematrixinv = function() matrixinv
        list(storematrix=storematrix, retrievematrix=retrievematrix, storematrixinv=storematrixinv, retrievematrixinv=retrievematrixinv)
}

cacheSolve <- function(x, ...) {
                       
        matrixinv = x$retrievematrixinv()
        
        if (!is.null(matrixinv)){
                message("getting cached data")
                return(matrixinv)
        }
        
        matrixinv = solve(x$retrievematrix(), ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$storematrixinv(matrixinv)
        
        return(matrixinv)
} 
