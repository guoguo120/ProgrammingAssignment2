## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## make matrix, provide get and set
## taking inverse of function and cache it
makeCacheMatrix <- function(x = matrix()) {
	mat <- NULL
        set <- function(y) {
                x <<- y
                mat <<- NULL
        }
        get <- function() x
        setInverse <- function(invIn) mat <<- invIn
        getInverse <- function() mat
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Write a short comment describing this function
## if inverse of argument already calculated and cached, retrieve inverse
## otherwise calculate and cache inverse of input matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat <- x$getInverse()
        if(!is.null(mat)) {
                message("getting cached inverse matrix")
                return(mat)
       }        
        data <- x$get()
        mat <- solve(data, ...)
        x$setInverse(mat)
        mat
}

