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
}

cacheSolve <- function(x, ...) {
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