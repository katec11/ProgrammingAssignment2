## makeCacheMatrix and cacheSolve functions enble the ability to create 'special' matrices that 
## allow for the inverse to be calculated only one time, and all future times, the inverse result 
## will be pulled from a cached value.  

## makeCacheMatrix returns a list of functions to set and get the value of a matrix, 
## and set and get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL                       #initially no inverse calculated
        set <- function (y){                       #sets value of matrix
                x <<- y
                i <<- NULL
        }
        get <- function () x                       #gets value of matrix
        setinverse <- function(solve) i <<- solve  #sets inverse of matrix
        getinverse <- function () i                #gets inverse of matrix
        list (set = set, get = get,                #lists functions
              setinverse = setinverse, 
              getinverse = getinverse)
}


## cacheSolve returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse")  #tells user not calculating
                return (i)                         #new inverse, returns cache  
        }
        data <- x$get()                            #Since not calculated before
        i <- solve(data, ...)                      #calculates inverse
        x$setinverse(i)
        i                                          #and returns it
}
