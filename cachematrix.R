## Shown below are the code needed to create functions 'makeCacheMatrix' and 'cacheSolve', as well as some
## code that tests the functions.

## 1. makeCacheMatrix creates the matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <- y
                inv <- NULL
        }
        get <- function() x
        setInv <- function(solve) inv <<- solve
        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

## 2. cacheSolve computes the inverse of the matrix that was cached in makeCacheMatrix

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- (x$get())
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
}

# 3. Testing makeCacheMatrix and cacheSolve on a 2 x 2 matrix
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
myMatrix_object <- makeCacheMatrix(m1)
cacheSolve(myMatrix_object)