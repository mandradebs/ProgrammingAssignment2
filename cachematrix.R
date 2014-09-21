## The firt function allow to create and special matrix object
## that can cache its inverse, the second functions can verified if
## the inverse of a "special matrix" has been calcutated and print out
## the inverse. If the inverse has not been calculated, then this function
## calculate and print the inverse of the matrix.

## This function creates a special "matrix" object that can cache 
## its inverse.
## This function add new features to a matrix obsjet. This features are three
## functions ("get" that return the matrix data, "setinverse" that set the
## inverse of the matrix, and "getinverse" that return the inverse) and 
## one attribute (the inverse of the matix).

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}

## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve 
## retrieve the inverse from the cache, else the inverse is calculated
## and the result is set to the special matrix features (via setinverse)
## and then is printed out.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
