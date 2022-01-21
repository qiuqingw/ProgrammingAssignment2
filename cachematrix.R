## We write a pair of functions (makeCacheMatrix and cacheSolve) that 
## cache the inverse of a matrix, which follows the basic structure of 
## two examples of makeVector and catchemean functions 
## provided in Coursera Programming Assignment 2 Instructions.


## makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse.



makeCacheMatrix <- function(x = matrix()) {
        inverse_matrix <- NULL
        set <- function(y) {
                x <<- y
                inverse_matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverse_matrix <<- inverse
        getinverse <- function() inverse_matrix 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_matrix <- x$getinverse()
        if(!is.null(inverse_matrix)) {
                message("getting cached data")
                return(inverse_matrix)
        }
        data <- x$get()
        inverse_matrix <- solve(data, ...)
        x$setinverse(inverse_matrix)
        inverse_matrix
}

x <- matrix(c(4,3,3,2), nrow = 2, ncol = 2)
data <- makeCacheMatrix(x)
data
attributes(data)
data$get()
data$getinverse()

cacheSolve(data)

# Getting the inverse matrix from the cache and skipping the computation
# via the setinverse.
cacheSolve(data)