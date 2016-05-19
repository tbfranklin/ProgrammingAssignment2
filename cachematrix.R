
##The makeCacheMatrix function and the cacheSolve function are used to cache the inverse of a matrix

##The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## The makeCacheMatrix function returns a list containing functions to
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse
## 4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                set <- function(y) {
                        x <<- y
                        inv <<- NULL
                        }
                get <- function() x
                setinvmat <- function(inverse) inv <<- inverse
                getinvmat <- function() inv
                list(set = set, get = get,
                       setinvmat = setinvmat,
                       getinvmat = getinvmat)
                }

## the cacheSolve function computes the inverse of the special "matrix returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinvmat()
        if(!is.null(inv)) { #if the inverse has already been calculated
        message("getting cached data") #gets data from the cache and skips computation
        return(inv)
        }
         #if inverse has not been calculated
        data <- x$get()
        inv <- solve(data, ...) #calculates the inverse
        x$setinvmat(inv)
        return(inv)
}