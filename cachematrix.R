## These functions perform matrix inverse caching in order to avoid costly computation.
## Solution to this problem assignment is almost completely given by the "Caching the 
## Mean of a Vector" example. I just added a check to avoid inverse recalculation when 
## reassigning the same matrix to the special "matrix" object (if condition on line 18).

## The function 'makeCacheMatrix' creates a special "matrix" object 
## that can cache its inverse. This object contains a list of 
## function to :
##   1.  set the value of the matrix
##   2.  get the value of the matrix
##   3.  set the value of the inverse
##   4.  get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    
    set <- function(y) {        
        if(!identical(x, y) {            
            x <<- y
            i <<- NULL               
        }        
    }    
    get <- function() x
    
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

## The function 'cacheSolve' calculates the inverse of the special 
## "matrix" created by the 'makeCacheMatrix' function. To avoid useless
## computations it checks if the inverse has already been calculated.
## If so, it takes the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of 
## the inverse in the cache via the `setinverse` function.

cacheSolve <- function(x) {
    
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    ## Return a matrix that is the inverse of 'x'
    i
        
}
