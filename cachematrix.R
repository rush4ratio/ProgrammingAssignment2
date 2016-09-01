## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# 'create' builds a new matrix
# 'get' returns martrix held
# 'getInverse' returns the inverse of the matrix
#  'setInverse' sets the inverse of the matrix to what's provided

makeCacheMatrix <- function(x = matrix()) {
    inverseOfMatrix <- NULL

    create <- function(newMatrix){
        x <<- newMatrix
        inverseOfMatrix <<- NULL
    }

    get <- function() x

    getInverse <- function() inverseOfMatrix

    setInverse <- function(inverse){
        inverseOfMatrix <<- inverse
    }

    list(create= create, get = get, getInverse = getInverse, setInverse = setInverse)
}


## Write a short comment describing this function
    # 'inv_m' holds the inverse of the matrix, it's null if it doesn't exist
    # Next step is to get the matrix's data, if inv_num is null
    # Calculate the inverse then return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_m <- x$getInverse()

        if(!is.null(inv_m)) {
            message("retrieving cached data")
            return(inv_m)
        }

        data <- x$get()

        inv_m <- solve(data, ...)
        x$setInverse(inv_m)

        inv_m
    }
    