##  Get values stored in cache to reduce recalculation

## create a list to get value in different cases
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse = setinverse, getinverse = getinverse) 
}

## return cache if already calculated, else calculate the value
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)  
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv ## Return a matrix that is the inverse of 'x'
}