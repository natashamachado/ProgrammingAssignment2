## The function makeCacheMatrix returns a list with 4 functions to store the data:
## One function to set the matrix, another to get the value of the matrix, another one
## to set the value of the inverse matrix and the last one to get the inverse matrix.
## The function cacheSolve checks if the inverse matrix was already caculated and if it is yes
## returns the value stored in the cache. If it is null, it gets the matrix stored, calculates 
the inverse and sets the value in the cache.

## This function returns a list with four functions to get and set a matrix
## and its inverse in the cache

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse ,
             getinverse = getinverse )

}


## This function returns the inverse matrix to the matrix in the cache

cacheSolve <- function(x, ...) {
         i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
