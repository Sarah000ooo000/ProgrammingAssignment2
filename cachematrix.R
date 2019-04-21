
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) m <<-Inverse
        getInverse<- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
cachesolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- silve(data, ...)
        x$setInverse(m)
        m
}
