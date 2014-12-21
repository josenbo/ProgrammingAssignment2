## Functions to wrap a matrix and its cached inverted matrix in an object
## to avoid frequent recalculations of the inverted matrix

## Returns a list of functions to get or set a matrix and its cached inverse
## Parameter x can be any invertible (regular) matrix

makeCacheMatrix <- function(x = matrix()) {
        ## Original and inverted matrix are held in variables
        ## at function level scope
        ## x (function parameter) holds the original matrix
        ## xi (local variable) holds cached inverted matrix
        
        ## there is no chached inverted matrix on first call
        xi <- NULL
        
        ## define function to (re)set the matrix
        ## parameter y is the new matrix
        set <- function(y) {
                x <<- y      ## redefine the parameter to makeCacheMatrix
                xi <<- NULL  ## forget cached value for previous matrix
        }
        
        ## define function to retrieve the matrix
        get <- function() x
        
        ## define function to store/cache the inverted matrix 
        setinv <- function(invertedMatrix) xi <<- invertedMatrix
        
        ## define function to retrieve cached inverted matrix
        getinv <- function() xi
        
        ## return labeled list of functions
        list(setOriginalMatrix = set
            ,getOriginalMatrix = get
            ,setInvertedMatrix = setinv
            ,getInvertedMatrix = getinv)
}


## Uses a cached matrix object to either retrieve an already calculated 
## inverted matrix or calculate one and store it for future use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## check the cached value
        mi <- x$getInvertedMatrix()
        
        ## ... and return that, if set
        if (!is.null(mi)) return(mi) 
        
        ## otherwise, get the original matrix
        mo <- x$getOriginalMatrix()
        
        ## calculate the inverted matrix
        mi <- solve(mo)
        
        ## cache it 
        x$setInvertedMatrix(mi)
        
        ## and return it
        mi
}
