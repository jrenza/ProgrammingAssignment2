## Put comments here that give an overall description of what your
## functions do
#
# - create cacheable matrix object
# x <- makeCacheMatrix()
# 
# - initialize object
# x$set(matrix(c(0, 2, 2, 0), 2, 2))
#
# - retrieve matrix object
# x$get()
#
# - check the object inverse
# cacheSolve(x)
#
# - run again to see if you get the "cached" message
# cacheSolve(x)


## Write a short comment describing this function
#
# create a matrix() object that can cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## Write a short comment describing this function
#
# compute inverse of matrix returned by makeCacheMatrix().
# if the inverse has already been calculated and the matrix has not changed
# then return the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    m <- x$getinverse()
    if (!is.null(m)){
        message("getting cached data inversed")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
