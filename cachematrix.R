## function 'makeCachematrix' creates and initializes a matrix
## function 'cacheSolve' returns the inverse matrix

## subfuntion 'set' initializes the matrix
## subfunction 'get' shows the matrix
## subfunctions 'setinvmatrix' and 'getinvmatrix' are called from function 'cacheSolve'

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
## if 'cacheSolve' runs for the first time it calculates the inverse matrix

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
