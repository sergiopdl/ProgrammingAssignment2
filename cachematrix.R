## functions for calculating and caching the inverse of a matrix using R's 
## lexical scoping rules. To test, enter following commands:
## a_matrix <- makeCacheMatrix()
## a_matrix$setMatrix(matrix(1:4,2,2))
## b <- a_matrix$getMatrix()
## c <- cacheSolve(a_matrix)
## successfull result: 
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1

## initializes cached value and defines getter/setter functions for new matrix 
makeCacheMatrix <- function(x = matrix()) {
    cached1 <- NULL
    setMatrix <- function(y) {
        matrix1 <<- y
        cached1 <<- NULL
    }
    getMatrix <- function() matrix1
    setInverse <- function(inversed1) cached1 <<- inversed1
    getInverse <- function() cached1
    list(setMatrix = setMatrix, getMatrix = getMatrix,
        setInverse = setInverse,
        getInverse = getInverse)
}

## returns inverse of given matrix using cached value if already set, or
## uses solve() to calculate inverse, set inverse, and return inverse if not 
## yet cached
cacheSolve <- function(x, ...) {
    cached2 <- x$getInverse()
    if(!is.null(cached2)) {
        message("getting cached data")
        return(cached2)
    }
    data <- x$getMatrix()
    cached3 <- solve(data, ...)
    x$setInverse(cached3)
    cached3
}