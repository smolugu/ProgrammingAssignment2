## makeCacheMatrix and cacheSolve functions work together to calculate inverse of a
## given matrix and store it in cache. The functions return inverse of a matrix from
## cache instead of calculating it each time it is required.

## makeCacheMatrix returns a list of functions that can be used to set a matrix, get
## data in that matrix, store inverse of a matrix in cache and retrieve it

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function()x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns inverse of the matrix returned by makeCacheMatrix function from
## the cache or by calculating inverse if not in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null (inv)) {
                message("getting cached inverse data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
