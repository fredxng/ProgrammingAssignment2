## function 'makeCacheMatrix' creates and initializes a matrix
## function 'cacheSolve' calcs and returns the inverse matrix -> pulls data from cache if available

## subfuntion 'set' initializes the matrix
## subfunction 'get' shows the matrix
## subfunctions 'setinvmatrix' and 'getinvmatrix' are called from function 'cacheSolve'
## 'setinvmatrix' saves the inverted matrix result to global environment
## 'getinvmatrix' reads the inverted matrix from the global environment

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinvmatrix <- function(solve) i <<- solve
    getinvmatrix <- function() i 
    
    list(set = set, get = get,
         setinvmatrix = setinvmatrix,
         getinvmatrix = getinvmatrix)
}


## subfunction cacheSolve returns the inverse matrix created by 'makeCacheMatrix'
## if 'cacheSolve' runs for the first time it calculates the inverse matrix and caches the result 
## in the global environment

## in the second run it pulls the inverse matrix result from the global environment 
## with lexical scoping -> message: "getting cached data"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinvmatrix()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinvmatrix(i)
    i
}
