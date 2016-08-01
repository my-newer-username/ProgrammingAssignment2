## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## function makeCacheMatrix is used to cache the matrix, and matrix inverse.

makeCacheMatrix <- function(x = matrix()) 
{
    MatrixInverse <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setMatrixInverse <- function(input = matrix()) MatrixInverse <<- input
    getMatrixInverse <- function() MatrixInverse
    list(set = set, get = get,
         setMatrixInverse = setMatrixInverse,
         getMatrixInverse = getMatrixInverse)

}


## Write a short comment describing this function
## function cacheSolve is used to get matrix inverse from cache or calculate matrix inverse if not available from cache.

cacheSolve <- function(x, ...) 
{
## Return a matrix that is the inverse of 'x'
    MatrixInverse <- x$getMatrixInverse()
    if(!is.null(MatrixInverse)) 
    {
        message("getting cached data")
        return(MatrixInverse)
    }
    tmp <- x$get()
    MatrixInverse <- solve(tmp, ...)
    x$setMatrixInverse(MatrixInverse)
    MatrixInverse
}
