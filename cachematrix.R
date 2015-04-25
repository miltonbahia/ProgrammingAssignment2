##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
    inv <- NULL
    
    # set the new value of the matrix and clear the 
    # value of the inverse matrix 
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    #  get the value of the matrix
        
    get <- function() x
    
    # set the value of the inverse matrix
        
    setinverse <- function(inverse) inv <<- inverse
    
    # get the value of the inverse matrix
    
    getinverse <- function() inv
        
    # Return a "matrix" object
    
    list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
    
        
    inv <- x$getinverse()

    # Checks whether the inverse of 'x' has been calculated previously 
    # and is cached.
    
    if(!is.null(inv)) {
        
        # Retrieves the cached value
        
        message("getting cached data")
        return(inv)
    }
    
    else {
    
        #  Computes the inverse matrix and caches  
        
        datamatrix <- x$get()
        
        # Checks if 'x' is a square matrix and performs the computation 
        # of the inverse
                
        if (nrow(datamatrix)==ncol(datamatrix)) {
        
            inv <- solve(datamatrix, ...)        
            x$setinverse(inv)
            
        }
        
        else {
        
          stop("'x' is not a square matrix")
            
        }
    
    }
    
    ## Return a matrix that is the inverse of 'x'
    
    inv
}