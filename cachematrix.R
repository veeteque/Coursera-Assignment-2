## There are two functions: makeCacheMatrix() & cacheSolve(). 
## makeCacheMatrix() creates a matrix and returns four sub-functions:
## get() - used to get the values of the (created) matrix
## getinverse() - used to get the values of the inverse matrix
## set() - used to set the values of the matrix to values included in the parameter of set() function
## setinverse() - used to set the values of the inverse matrix to values included in the parameter of setinverse()function

## cacheSolve() - used to calculate the inverse matrix. If the inverse matrix has already been computed
##the cached matrix is returned.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    get <- function() x
    getinverse <- function() inverse
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    setinverse <- function(set_inverse) inverse <<- set_inverse
    
    list(get = get, set = set, getinverse = getinverse, setinverse = setinverse)
}

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)){
        print("The inverse matrix has already been computed. Getting cached matrix.")
        return(inverse)
    }
    input_matrix <- x$get()
    inverse <- solve(input_matrix)
    x$setinverse(inverse)
    inverse
    ## Return a matrix that is the inverse of 'x'
}
