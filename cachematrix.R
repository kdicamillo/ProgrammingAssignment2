## These functions take a matrix as input and provide the inverse of the matrix
## Once the inverse of a specific matrix has been calcuated, the results are cached
## so that the calculation does not need to occur for that specific matrix again if the
## inverse is requested

## This function takes a matrix as an input and stores its value for later retrieval
## The function outputs a list containing 4 functions: set, get, setinverse, and getinverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inputval) m <<- inputval
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function takes the output list of makeCacheMatrix as input and 
## returns a matrix that is the inverse of the original input to makeCacheMatrix. 
## The function will indicate whether the inverse has been calculated or has been 
## retrieved from the cache

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
