## cache the result of the inversion of matrix object and return the cached 
## result for the same object. 
## 'makeCacheMAtrix' - cache the matrix and cache the result.
## 'cacheSolve' - will return the cache resukt if present otherwise calulate
##                the inverse and cache the result .                 

## 'makeCacheMatrix' - creates a special matrix, which is a list  
##  containing a function to:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse of the matrix
## 4) get the value of the cached inverse matrix
## Inputs: matrix
## Outputs: list 
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(minv) inverse <<- minv
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Calculate the inverse of the special cached matrix created with the
## above function. First check if the inverse is already computed. 
## If computed then return from cache, Otherwise, calculate the
## inverse of the matrix and save the resukt in the cache.
## Input: value from 'makeCacheMatrix'
## Output: matrix inverse
cacheSolve <- function(x, ...) {
    matrixInverse <- x$getInverse()
    if(!is.null(matrixInverse)) {
        message("getting cached data")
        return(matrixInverse)
    }
    data <- x$get()
    matrixInverse <- solve(data, ...)
    x$setInverse(matrixInverse)
    matrixInverse 
}

## Test to verify if the matrix is inverted and cached
## Create a invertible 2X2 matrix, cache the matrix and invert the matrix
m = matrix(c(3, -7, 5, 2), 2, 2)
cm = makeCacheMatrix(m)
im = cacheSolve(cm)

## This should give you an identity matrix if the matrix is inverted
m %*% im

## If you compute the inverse again then
## you should see this message - "getting cached data"
im1 = cacheSolve(cm)

