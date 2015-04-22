##  This function creates a special "matrix" object that can cache its inverse.

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

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
    
        
    inv <- x$getinverse()

    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
        
    datamatrix <- x$get()
    
    ## Checks if 'x' is a square matrix
    
    if (nrow(datamatrix)==ncol(datamatrix)) {
    
        inv <- solve(datamatrix, ...)        
        x$setinverse(inv)
        
    }
    
    else {
    
      stop("'x' is not a square matrix")
        
    }
        
    ## Return a matrix that is the inverse of 'x'
    
    inv
}