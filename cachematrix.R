## Put comments here that give an overall description of what your
## functions do

## This function is meant to convert a matrix into a cached data. In 
## doing so, one can reuse the data which is processed previously.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(x_inv)  inv <<- x_inv
                getinv <- function() inv
                list(set=set, get=get,setinv=setinv,getinv=getinv)

}


## cacheSolve function is meant to process the cached data.

cacheSolve <- function(x, ...) {
       inv<- x$getinv()
        if(!is.null(inv)){
                message("Getting cached data")
                return(inv)
        }
        inv <- solve(x$get())
        x$setinv(inv)
        inv
}
