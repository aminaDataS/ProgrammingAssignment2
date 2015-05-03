## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix function calculate the inverse of a matrix and save it to the cach 
## however if the user  attempt to calcultae the matrix inverse the calculation will not be repeated 
## but the previous value is returned

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        ## define the cache "m"
        m <- NULL
        
        ## we will assign the input matrix to x (parent envirenment)
        ## and reinitize m to null (parent envirenment)
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        
        
        ## we return with get our matrix "x"
        ## setting the cache to the inverse of the matrix "x"
        ## returned the cached inverse of "x"
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
## cacheSolve calculate the inverse of the special "matrix" created with the above and sets the value 
## of the inverse in the cache. 
## it first checks to see if the inverse has been already calculated
## if "OK" its getting the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setinverse(m)
        m
  
}
