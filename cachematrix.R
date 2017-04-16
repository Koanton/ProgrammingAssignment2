## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a 
## matrix rather than compute it repeatedly. 

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    #initialize the empty matrix
    matrix<-NULL
    
    #assign a value to an object in an environment that is different from the current environment
    set <- function(y){
        x<<-y
        matrix<<-NULL
    }
    
    get<- function() x
    setinverse<- function(inverse) matrix<<-inverse
    getinverse<-function() matrix
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
        
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    #retrieve the matrix from the cache 
    matrix<-x$getinverse()
    
    #if the returned function is not null, this means it has been cached
    if (!is.null(matrix)){
        message("getting cached data")
        return(matrix)
    }
    data<-x$get()
    
    #if no cached version, compute inverse
    matrix<-solve(data, ...)
    x$setinverse(matrix)
    matrix
}
