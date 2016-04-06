##To solve the problem of large data, I wrote two functions to solve the inverse matrix problem .

##This function is meant to convert a matrix into a cached data. In 
## doing so, one can reuse the data which is processed previously.

makeCacheMatrix <- function(x = matrix()) {
 +inv <- NULL
 +        set <- function(y) {
 +                x <<- y
 +                inv <<- NULL
 +        }
 +        get <- function() x
 +        setinv <- function(x_inv)  inv <<- x_inv
 +                getinv <- function() inv
 +                list(set=set, get=get,setinv=setinv,getinv=getinv)
  
  }
  
  
 +## cacheSolve function is meant to process the cached data.
  
  cacheSolve <- function(x, ...) {
 -        ## Return a matrix that is the inverse of 'x'
 +       inv<- x$getinv()
 +        if(!is.null(inv)){
 +                message("Getting cached data")
 +                return(inv)
 +        }
 +        inv <- solve(x$get(),...)
 +        x$setinv(inv)
 +        inv
  }
