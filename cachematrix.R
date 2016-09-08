## The following functions enable to set an object matrix, compute, store and retrieve its inverse
## when necessary instead of recomputing it 

## makeCacheMatrix is a function that sets a kind of matrix object able to set and return its value, 
## store its inverse in the Inv object, and also the matrix that has been inversed to get this result

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  Inv_matrix <- NULL
  
  ## The set function enables to (re) set the value of the matrix
  set <- function(y) {
    x <<- y
  }
  
  ## The get function returns the value of the matrix
  get <- function() x
  
  ## setInverse and getInverse resp. stores (returns) the inverse of the matrix in (from) the Inv object
  setInverse <- function(Inverse) Inv <<- Inverse
  getInverse <- function() Inv
  
  ## For cases where the matrix has been modified since the last computation of its inverse, setInversedMatrix
  ## and getInversedMatrix resp. stores (returns) the matrix that has been inversed to get the Inv object
  setInversedMatrix <- function(InversedMatrix) Inv_matrix <<- InversedMatrix
  getInversedMatrix <- function() Inv_matrix
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse, 
  setInversedMatrix = setInversedMatrix, getInversedMatrix = getInversedMatrix)
}


## cacheSolve computes and returns the inverse of a matrix x passed as argument
## First it checks if the inverse has already been computed, and if so, also checks if the inverse 
## already computed corresponds to the matrix x passed as argument
## If both conditions are ok, it returns the inverse value cached, otherwise it computes the inverse of x

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  
  ## First get the matrix to be inversed, checks if an inverse value exists in the cache and 
  ## gets the corresponding matrix inversed
  my_data <- x$get()
  Inv <- x$getInverse() 
  Inv_matrix <- x$getInversedMatrix()
  
  ## Checks if a inverse value has been computed and stored AND if the matrix is identical
  if(!is.null(Inv) && identical(Inv_matrix, my_data)) {
    print("retrieving cached matrix") 
    return(Inv) }
  
  ## Otherwise computes the inverse and stores it in Inv using the setInverse
  ## and stores the inversed matrix using the setInversedMatrix function of makeCacheMatrix
  ## and returns the inverse when (re)computed
  Inv <- solve(my_data)
  x$setInverse(Inv)
  x$setInversedMatrix(my_data)
  Inv
}
