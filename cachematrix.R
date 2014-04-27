## This function creates a matrix object which can be specified by the user
# but if not is left empty. This function also contains a list of functions
# That can be used to set the data matrix and display the inverse of the data matrix.

makeCacheMatrix <- function(x = matrix()) { # Matrix creation function which takes a square matrix as an argument or can be left blank.
    m <- NULL # This sets the local mean value to zero on initialization
    set <- function(y) { # takes a square matrix as an argument and Will set the objects to a matrix to the specified matrix
        x <<- y
        m <<- NULL # This sets the cached mean value to zero on initialization
    }
    get <- function() x # This function simply returns the data matrix
    setinverse <- function(inverse) m <<- inverse # This function takes a square matrix as an argument which in turn sets the inverse of the matrix to that value
    getinverse <- function() m # This function returns the inverse of the data matrix
    list(set = set, get = get, # This is a helpful list of commands that users can use when calling a makeCacheMatrix object
         setinverse = setinverse,
         getinverse = getinverse)
}

# This function caches and calculates the inverse of the makeCacheMatrix object.
# it first fetches the present inverse for the makeCacheMatrix object and checks
#to see if it has a cached value or is empty. If it is empty then it computes
#the inverse, if the inverse exists then it returns the cached inverse value

cacheSolve <- function(x, ...) { 
    m <- x$getinverse() 
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
    ## Return a matrix that is the inverse of 'x'
}

## TEST CODE for using the above functions ##

# MAT <- matrix(rnorm(1:25), 5, 5)
# makeCacheMatrix(MAT) -> k
# cacheSolve(k) # Not cached the first time
# cacheSolve(k) # Is cached the second time