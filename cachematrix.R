#This script contains two functions: makeCacheMatrix() and cachesolve()

#  makeCacheMatrix() 
#       e.g.    myMatrix <- makeCacheMatrix(matrix(c(10,15,20,25),nrow=2,ncol=2))

#    This function creates a matrix object which also has 4 function elements that
#    allows to perform different operations on the matrix object:
#        - set: 
#            e.g.   myMatrix$set(matrix(1:4,nrow=2,ncol=2))
#            Sets different matrix data (data, rows, columns, etc) to the matrix object.
#            Also resets any previously stored associated inverse matrix calculation
#        - get:
#            e.g.   myMatrix$get()
#            Gets the data content of the matrix object
#        - setinverse: 
#            NOTE: This function should only be called through the use of function
#            cachesolve() to avoid unnecessary computations (see cachesolve() below)
#            Calculates the inverse matrix of the matrix object and stores it in 
#            the matrix object
#        - getinverse:
#            e.g.   myMatrix$getinverse()
#            will display the value currently stored as inverse matrix in the matrix
#            object

## makeCacheMatrix(DATA, nrow, ncol, byrow, dimnames)
## e.g. myMatrix <- makeCacheMatrix(matrix(c(10,15,20,25),nrow=2,ncol=2))

makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
#sets data to new values and resets the stored inverse matrix to NULL
    set <- function(definedMatrix) {   
        x <<- definedMatrix
        invMatrix <<- NULL
    }
#returns the data of the matrix object
    get <- function() x   
#performes solve() and stores inverse matrix value in invMatrix    
    setinverse <- function(solve) invMatrix <<- solve  
#returns the stored inverse matrix value
    getinverse <- function() invMatrix
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

#   cachesolve()
#       e.g.    cachesolve(myMatrix)
#       
#       This function is for use with matrix objects created with the makeCacheMatrix()
#       function. It will calculate an inverse matrix for the matrix object and store
#       it in the object (invMatrix). In the event an inverse matrix was previously
#       calculated and stored in this object, the stored data will be retrieved instead
#       of calculating the inverse matrix again.


## cachesolve(CacheMatrixObject)
## e.g. cachesolve(myMatrix)

cachesolve <- function(x, ...) {

# gets the stored inverse matrix value from the matrix object. If the value is not
# NULL, the value is returned...
    invMatrix <- x$getinverse()
    if(!is.null(invMatrix)) {
        print("Retrieving previously calculated inverse matrix:")
        return(invMatrix)
    }
# ...otherwise the function gets the data content of the matrix object, calculates
# its inverse matrix, stores it in the matrix object and returns the value
    myMatrix <- x$get()
    invMatrix <- solve(myMatrix, ...)
    x$setinverse(invMatrix)
    invMatrix
}
