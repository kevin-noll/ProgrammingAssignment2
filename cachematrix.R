## makeCacheMatrix takes an input matrix and creates a cache
## where the inverse of the matrix can be stored and retrieved
##
## functions created for the cache are:
##     set
##     get
##     setInverse
##     getInverse
makeCacheMatrix <- function(mat = matrix()) {
    matinv <- NULL
    
    set <- function(p_mat) {
        mat <<- p_mat
        matinv <<- NULL
    }
    
    get <- function() mat
    
    setInverse <- function(p_matinv) {
        matinv <<- p_matinv
        mat_changed <<- FALSE
    }
        
    getInverse <- function() matinv
    
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve takes as input a cached matrix as defined in makeCacheMatrix
## calculates the inverse.  If the matrix has not changed and the inverse
## has already been calculated, the cached inverse is returned.
cacheSolve <- function(x, ...) {
    
    matinv <- x$getInverse()
    
    if (!is.null(matinv)) {
        message("getting cached data")
        return(matinv)
    }
    
    ## Since the inverse was not cached, calculate and cache
    mat <- x$get()
    matinv <- solve(mat)
    x$setInverse(matinv)
    matinv
}
