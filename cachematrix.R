## Inverse Matrix Calculation
## 
## The functions below calculate the inverse of a square matrix.
## Calculating the inverse of a square matrix could be costly depending upon
## the size of the matrix. Therefore, once the inverse of the matrix is 
## calculated, it is stored in memory so we don't need to calculate it again 
## if we need to find the inverse of the same square matrix later.
##
## 6/20/2014 - KO - Original Code

## MakeCacheMatrix - This function takes a matrix as an argument and
## returns a list of functions that are necessary to set and get the matrix
## into and from a variable that is outside this environment...essentially
## putting the matrix into the global environment for use later.

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    ## Reset values
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inverseMatrix) im <<- inverseMatrix  ## set the value into the global environment
  getInverseMatrix <- function() im  ## pick up the value from the global environment
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## cacheSolve - This function takes the result of makeCacheMatrix as an argument and 
## returns the inverse of the matrix made in makeCacheMatrix.  
## It uses the makeCacheMatrix getInverseMatrix function to pull the inverse 
## of the matrix from cache if it was already calculated.  Otherwise, it calculates
## the inverse of the matrix and stores the result in cache by using the 
## setInverseMatrix function from makeCacheMatrix.

cacheSolve <- function(x, ...) {
  im <- x$getInverseMatrix() ## see if the inverse is already available in cache
  if(!is.null(im)) {
    message("getting cached data")
    return(im)  ## if it is available then just return it
  }
  ## if it isn't available in cache the compute the inverse and set the result in cache
  matrix <- x$get()
  im <- solve(matrix)  ## compute the inverse
  x$setInverseMatrix(im)
  im
}
