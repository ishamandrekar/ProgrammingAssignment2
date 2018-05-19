## makeCacheMatrix - This function takes in a matrix as input and 
## returns 4 functions as part of the return vector for get, set of the input matrix and the computed inverse of the matrix.
## the set functions, set the value of matrix and its inverse to the parent enviroment, in turn caching those values
## cacheSolve - This function takes in the vector of the type returned by makeCacheMatrix as input 
## It does the actual job of finding the inverse of the matrix using solve,
## but before that checks if the inverse has already been calculated using the getSolve method of the makeCacheMatrix(Accecible to the inout vector)
## If yes returns the cached value, if not computes the inverse using solve and calls the setsolve method of the makeCacheMatrix(Accecible to the input vector)
## to set the computed inverse value to the parent environment of makeCacheMatrix

## This function takes a matrix as input and returns 4 functions as part of the return vector(set,get,setsolve,getsolve)

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
    x <<- y
    i <<- matrix()
    }
	get <- function() x
    setsolve <- function(m_i) i <<- m_i
    getsolve <- function() i
    list( set = set, 
	      get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}

## This function takes in a vector of type which is returned by makeCacheMatrix, checks for cached value of matrix inverse
## if not cached calculates the matrix inverse, sets it to the parent environment and returns it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	
	i <- x$getsolve()
	if(!all(is.null(i))) {
       message("getting cached data")
       return(i)
    }
    data <- x$get()
	i <- solve(data,...)
	x$setsolve(i)
    i

}
