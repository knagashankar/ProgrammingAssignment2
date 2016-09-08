makeCacheMatrix <- function(x = matrix()) {
        ## assuming x is a square invertible matrix
        ## this function returns a list containing functions to
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
        
        inverseMatrix = NULL
        set = function(y) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                x <<- y
                inverseMatrix <<- NULL
        }
        get = function() x
        setinv = function(inverse) inverseMatrix <<- inverse 
        getinv = function() inverseMatrix
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
        ## x is output of makeCacheMatrix() a list of cache values
        ## return: inverse of the original matrix input to makeCacheMatrix()
        
        inverseMatrix = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inverseMatrix)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inverseMatrix)
        }
        
        # otherwise, calculates the inverse 
        cachedMatrix.data = x$get()
        inverseMatrix = solve(cachedMatrix.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inverseMatrix)
        
        return(inverseMatrix)
}
