## Rowan Assignment 2


## MakeCacheMatrix will take a matrix I define and put it into memory 
## fucntion(x = matrix()) defines x as a matrix input
## Set function sets the matrix
## Get function calls the matrix
## Setinverse sets the inverse of the matrix
## Getinverse calls the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Passes in the chached matrix and solves it to get the inverse
## First tries to pass the matrix that is called in the cachesolve()
## If it is NULL then it looks for the cached matrix
## Solves and returns the inverse of the passed/cached matrix

cacheSolve <- function(x, ...) {
                m <- x$getinverse()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                m <- solve(data, ...)
                x$setinverse(m)
                m
}
