##  These functions take a square, invertible matrix as input and 
##  calculate the inverse, storing it in the cache for quick retrieval.

##  makeCacheMatrix
##
##  Args: x is a square, invertible matrix.
##      
##  Return: The function contains four sub-functions as detailed below.
##
##  Actions: Within this function 4 separate functions are created:
##  1. 'set' applies a function to input y. 
##  2. 'get' pulls the value of x out of memory.
##  3. 'setinverse' takes a function called 'inverse' and applies it to m
##  4. 'getinverse' pulls the value of m out of memory
##  The list then adds set, get, setinverse and getinverse as optional
##  arguments to makeCacheMatrix when it is called.

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve
##
##  Args: 'x' is a square, invertible matrix. Further arguments can be passed in
##        from the '...' which will apply to the 'solve' function.
##      
##  Return: The inverse of the matrix 'x', either by calculation or from the cache
##
##  Actions: 
##  1. Set 'm' to the output of the getinverse function on matrix 'x'
##  2. If 'm' is not empty then the inverse has previously been calculated.
##     So print the message "getting cached data" and return the cahced 'm'
##  3. If 'm' is empty then the inverse has not been calculated before.
##     Assign the variable 'data' the matrix 'x' using the 'get' function
##     Find the inverse of 'x' (via 'data') using the solve() function
##     Set the inverse of 'x' to be 'm' and return the value 'm'
##
##  Note that the '...' in the 'solve' function means that extra arguments for 
##  'solve' can be included if required

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
}

