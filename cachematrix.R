## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(mtx = matrix()) {
    ## return: a list containing functions to 
    ##            1. set the matrix
    ##            2. get the matrix
    ##            3. set the inverse
    ##            4. get the inverse 
    ##         this list is used as the input to cacheSolve()
    
    inverse <- NULL
    set <- function(x) {
        ## use '<<-' to assign a value to an object in an environment 
        ## different from the current environment. 
        mtx <<- x;
        inverse <<- NULL;
    }
    get <- function() return(mtx);
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() return(inverse);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## If the inverse has already been calculated
## and the matrix has not changed, then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(mtx, ...) {
    ## return: inverse of the original matrix input to makeCacheMatrix()
    inverse <- mtx$getinv()
    
    ## if the inverse has alrady been calculated
    if(!is.null(inverse)) {
        ## get if from the cache and skips the computation 
        message("Getting cached data...")
        return(inverse)
    }
    
    ## otherwise, calculate the inverse 
    mat.data <- mtx$get()
    invserse <- solve(data, ...)
    
    ## set the value of the inverse in the cache via the setinv function
    x$setinv(inverse)
    return(inverse)
}
