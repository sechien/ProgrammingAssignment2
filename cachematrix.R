## write a pair of functions that cache the inverse of a matrix.
## first function creates a special "matrix" object 
## that can cache its inverse.
## 2nd function:  computes the inverse of the special "matrix" 
## Returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

##  create a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inverseMatrix <<- inverse
    getInverse <- function() inverseMatrix 
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## this function first check the inverse exists in the cache, 
## if true, return the inverse, if not, get the matrix from the the data
## of the first matrix and calculate the inverse
## input argument function(x) is the object where the makeCacheMatrix 
## is stored

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getInverse()
    if(!is.null(inverseMatrix)) {
        message("getting cached inverse")
        return(inverseMatrix)
    }
    data <- x$get()
    # calculate the inverse
    inverseMatrix <- solve(data, ...)
    x$setInverse(inverseMatrix)
    return(inverseMatrix)
}