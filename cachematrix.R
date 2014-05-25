## Put comments here that give an overall description of what your
## functions do
## This pair of functions can cache the inverse of a matrix.

## Write a short comment describing this function
## The function, 'makeCacheMatrix', creates a special "matrix",  
## which is really a list containing a function to
## 1. set the content of the matrix
## 2. get the content of the matrix
## 3. set the content of the inverse of the matrix
## 4. get the content of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set.inverse.matrix <- function(solve) m <<- solve
        get.inverse.matrix <- function() m
        list(set = set, get = get,
             set.inverse.matrix = set.inverse.matrix,
             get.inverse.matrix = get.inverse.matrix)
b
}


## Write a short comment describing this function
## "cacheSolve" function calculates the inverse of the special   
## "matrix" created with the above function. However, it first 
## checks to see if the inverse of the matrix has already been 
## calculated. If so, it gets the inversed matrix from the 
## cache and skips the computation. Otherwise, it calculates the 
## inverse of the matrix and sets the inversed matrix in the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get.inverse.matrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set.inverse.matrix(m)
        m
}
