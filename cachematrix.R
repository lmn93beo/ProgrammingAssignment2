## There are 2 functions to help caching the inverse of a matrix

## Function 1: Creates a special "matrix" object, which is a list of 4 functions
## set: set the value of the matrix
## get: get the value of the matrix
## setInverse: set the value of the inverse matrix
## getInverse: get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL #When initialized, value of the inverse is set to NULL
        
        ## 'set' function
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        ## 'get' function
        get <- function() x
        
        ## 'setInverse' function
        setInverse <- function(NewInv) inverse <<- NewInv
        
        ## 'getInverse' function
        getInverse <- function() inverse
        
        ## Returns the list of 4 functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## Function 2: takes 'x' as the special "matrix" object.
## If the Inverse is stored, return the stored inverse.
## Otherwise, calculate the inverse, store, and return

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        StoredInverse <- x$getInverse()
        Matrix <- x$get()
        if(!is.null(StoredInverse)) {
                message("getting cached data")
                return(StoredInverse)
        }
        
        ## Case: matrix is not a square. Print warning.
        else if(dim(Matrix)[1] != dim(Matrix)[2]){
                warning("x is not a square matrix. Cannot find inverse")
                return(StoredInverse)
        }
        
        ## Case: matrix is not invertible. Print warning.
        else if (det(Matrix)==0) {
                warning("x is not invertible")
                return(StoredInverse)
        }
        
        else{
                ## Inverse is not stored
                Matrix <- x$get()
                Inverse <- solve(Matrix)
                x$setInverse(Inverse)
                Inverse
        } 
}
