## The two functions together will return a inverse matrix of a provided matrix 
## and cache the result once it was calculated 

## This function can cache the inverse of the square matrix. it gives back a list containing:
# set a value for the matrix
# get the value of the matrix
# set a value as the inverse of the matrix
# get the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function (y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inver <<- solve
        getinverse <- function() inver
        list(set = set, get = get, 
             setinverse = setinverse, getinverse = getinverse)
        
}


## this function will calculate and return the inverse of a square matrix 
## if the job has not been done before. 
## Otherwise it will return a catched value. 

cacheSolve <- function(x, ...) {
        inver <- x$getinverse()
        if(!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        data <- x$get()
        inver <- solve(data, ...)
        x$setinverse(inver)
        inver ## Return a matrix that is the inverse of 'x'
}
