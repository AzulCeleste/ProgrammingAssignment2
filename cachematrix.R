##This file stores the following functions: makeCacheMatrix, cacheSolve


## This function creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

## initialize the value of the matrix inverse to NULL
matrixinverse <- NULL
        
## declare another function set where the value will be cached in:
## 1. Matrix is created for the first time.
## 2. changes made to cached matrix
set <- function(y){
        x <<- y
        ## change the value of inverse of the matrix in case the matrix was changed.
        matrixinverse <<- NULL
}

##get is a function that returns the vector x stored in the main function
get <- function() x


##setinverse store the value of the input in a variable m into the main function cacheMatrix
setinverse <- function(inv) matrixinverse <<- inv
        
##getinverse returns the value stored of the inverse of the matrix
getinverse <- function() matrixinverse


## passes the value of the function makeCacheMatrix 
list(get = get, set = set,
     setinverse = setinverse,
     getinverse = getinverse)
        
}


## This function computes the inverse of a matrix contained in cache memory.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x = matrix()) {
        matrixinverse <- x$getinverse()
        if(!is.null(matrixinverse)) {
                message("getting cached data- Inverse of the matrix")
                return(matrixinverse)
        }
        data <- x$get()
        matrixinverse <- solve(data)
        x$setinverse(matrixinverse)
        matrixinverse
}




