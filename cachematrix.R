## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(mat = matrix()) {
    matinv <- NULL
    
    set <- function(p_mat) {
        mat <<- p_mat
        matinv <<- NULL
    }
    
    get <- function() mat
    
    setInverse <- function(p_matinv) matinv <<- p_matinv
    
    getInverse <- function() matinv
    
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    matinv <- x$getInverse()
    if (!is.null(matinv)) {
        message("getting cached data")
        return(matinv)
    }
    
    mat <- x$get()
    matinv <- solve(mat)
    x$setInverse(matinv)
    matinv
}
