## This function can create a special object that stores a matrix and caches its inverse.

## This function creates a special matrix object 'x' that can be used in cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function()x
    setinverse<-function(inverse) m <<- inverse
    getinverse<-function()m
    list(set=set,get=get,
    setinverse=setinverse,
    getinverse=getinverse)
}


## This function computes the inverse of matrix 'x' created by the function makeCacheMatrix. 

cacheSolve <- function(x, ...) {
    m<-x$getinverse()
## if we already get the inverse of 'x', then return the value.
    if(!is.null(m)){
        message ("getting cached data")
        return(m)
    }
    data<-x$get()
    m<-solve(data, ...)
    x$getinverse(m)
    m
    ## Return a matrix that is the inverse of 'x'.
}
