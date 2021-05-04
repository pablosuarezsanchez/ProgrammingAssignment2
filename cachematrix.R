## I followed the proccess refered in StackOverflow "catching the mean of a vector in R"
## https://stackoverflow.com/questions/24904683/caching-the-mean-of-a-vector-in-r/47723281#47723281
## https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ##Step 1: Initialize objects
        m <- NULL
        ##Step 2: Define the "behaviors"
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set,                # gives the name 'set' to the set() function defined above
             get = get,                # gives the name 'get' to the get() function defined above
             setinverse = setinverse,  # gives the name 'setinverse' to the setinverse() function defined above
             getinverse = getinverse)  # gives the name 'getinverse' to the getinverse() function defined above
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##      If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}


#Trying the functions

M <- matrix(c(4,7,9,2), nrow = 2, ncol = 2)

cacheSolve(makeCacheMatrix(M))
solve(M)