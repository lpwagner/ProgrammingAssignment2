# initializes an object as a list of functions to create a matrix with 
# a cached inverse

makeCacheMatrix <- function(x = matrix()) {
    Inverse <- NULL

    # set() is used to set the value of the matrix
    set <- function(y) {

        # We check to make sure the input is in fact a square, invertible matrix
        if(is.matrix(y)&&(nrow(y)==ncol(y))&&!(det(y)==0)){
        x <<- y 
        Inverse <<- NULL
        }

        # if the argument is not a square, invertible matrix an error is returned
        else{
            message("error: argument is not a square, invertible matrix.")
        }
    }

    # get() returns the matrix
    get <- function() x

    # setInverse() stores the inverse of the matrix in memory
    # it takes as an argument a square, invertible matrix

    setInverse <- function(inverse) {

        # First make sure the input is actually an inverse matrix
        if(is.matrix(inverse)&&(nrow(inverse)==ncol(inverse))&&!(det(inverse)==0)){
            Inverse <<- inverse
        }

	# if the argument is not a square, invertible matrix an error is returned
	else{
            message("error: argument is not a square, invertible matrix.")
        }
    }

    #return the inverse of the matrix
    getInverse <- function() Inverse
    list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}

#this function returns the inverse function of a cached matrix objects 
#created by makeCacheMatrix. The arguments are an object of type 
#makeCacheMatrix and any arguments passed to solve()

cacheSolve <- function(x, ...) {

    # frist, the object x is checked to see if the inverse matrix has already 
    #been computed and stored
    # if it has, the stored inverse is returned
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    # if the inverse has not been stored, it is computed, stored and returned.
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}