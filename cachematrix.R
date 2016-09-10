## Caching the Inverse of a Matrix
## Below functions help to create a custom matrix which can cache its inverse

## makeCacheMatrix: Creates a special "matrix" object that can cache its inverse
## Assumption: Input matrix should be a square invertible matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setminv <- function(minv) m <<- minv
        getminv <- function() m
        list(set = set, get = get,
             setminv = setminv,
             getminv = getminv)
}


## cacheSolve: Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getminv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setminv(m)
        m
}
