## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

## Crates a matrix objects that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        # parameter x: square matrix, invertible
        # returns a list of functions: set matrix, get matrix, set inverse, get inverse
        
        invm <- NULL
        set <- function(y) {
                x <<- y
                invm <<- NULL
        }
        get <- function() x
        setinvm <- function(inverse) invm <<- inverse
        getinvm <- function() invm
        list(set = set, get = get,
             setinvm = setinvm,
             getinvm = getinvm)
}


## Computes the inverse of the matrix; if the inverse has already been calculated, retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        # parameter x:  output of function makeCacheMatrix
        # returns the inverse of the matrix returned by makeCacheMatrix
        
        invm <- x$getinvm()
        if(!is.null(invm)) {
                message("getting cached data")  #if the inverse is already calculated, retrieve from cache
                return(invm)
        }
        matrix.data <- x$get()
        invm <- solve(matrix.data, ...)   # solve returns the inverse of the matrix
        x$setinvm(invm)
        invm
}
