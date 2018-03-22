## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {                # take matrix as default argument
        i <- NULL                                          # initialize the value of matrix inverse i as NULL
        set <- function(y) {                               # defind a set function 
          x <<- y                                          # set the value of matrix in the parent environment
          i <<- NULL                                       # set the matrix inverse as NULL
        }
        
        get <- function() x                                # define a get function, which will return the matrix value x
        setinverse <- function(inverse) i <<- inverse      # define function setinverse and assign a value inverse to i
        getinverse <- function() i                         # define function getinverse to get the inverse value i
        
        list(set = set,                        
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)                      # this list can help you to subset the functions
}


## Write a short comment describing this function

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
# should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()                                # call the function getinverse to get the matrix inverse 
        if (!is.null(i)) {                                 # check if the inverse matrix is null, if not then...
           message("getting cached matrix inverse")        # type message: getting cached matrix inverse
           return(i)                                     # return the inverse
        }
        
        matr <- x$get()                                    # get the original matrix
        i <- solve(matr, ...)                              # use solve function to inverse the matrix
        x$setinverse(i)                                    # set the matrix inverse
        i                                                  # return the matrix inverse
}
