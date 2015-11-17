@@ -0,0 +1,65 @@
    ## Function "makeCacheMatrix": This function creates a special "matrix" object that can cache its inverse.
    
    ## Function cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
    ## If the inverse has already been calculated (and the matrix has not changed), 
    ## then the cachesolve should retrieve the inverse from the cache.
    
    ## Function that creates a special vector, which is a list containing four functions: 
    ## 1, set up a matrix
    ## 2, get the matrix
    ## 3, set the inverse of the matrix
    ## 4, get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    # function that set matrix x equal to matrix y; m equal to null
    setmatrix <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    #function that return matrix x
    getmatrix <- function() {
        x
    }
    
    #function that set matrix m equal to matrix inverse
    setinverse <- function(inverse){
        m <<- inverse
    }
    
    #function that return matrix inverse
    getinverse <- function() {
        m
    }
    
    #show a list of fucntions
    list(setmatrix = setmatrix, getmatrix = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function return the inverse of matrix returned by makeCacheMatrix above; if id does not exist,
## this function will compute the inverse of matrix  and return the inverse of matrix. 

cacheSolve <- function(x, ...) {
    
    ## set m as the inverse of matrix 'x'
    m <- x$getinverse()
    
    ## If the inverse of matrix 'x' does exist, return the cached inverse
    if(!is.null(m)) {
        message("getting cached inverse of a previous calculated matrix")
        return(m)
    }
    
    ## If the inverse of matrix 'x' does NOT exist, calculate and return the inverse of matrix x
    
    data <- x$getmatrix()
    m <- solve(data) ## solove the inverse of matrix
    
    x$setinverse(m) ##cache the inverse of matrix
    m ## return inverse of matrix
}