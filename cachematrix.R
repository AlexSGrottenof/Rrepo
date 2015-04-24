## These functions take advantage of the R scoping
## rules to minimize costly computations

## This function, makeCacheMatrix creates a matrix 
## and caches its inverse once computed.

makeCacheMatrix <- function(x = matrix()) 
{
  InvMatrix <- matrix()
  set <- function(y = matrix) {
    x <<- y
    InvMatrix <<-  matrix()
  }
  get <- function() x
  setimatrix <- function(solve) InvMatrix <<- solve
  getimatrix <- function() InvMatrix
  data.frame(set = set, get = get,
             setimatrix = setimatrix,
             getimatrix = getimatrix)
}

## This function checks the previous function to determine
## if the inverse of the matrix was already calculated.  
## If not,it calculates the inverse of the matrix.

cacheSolve <- function(x, ...) 
{
  InvMatrix <- x[,getimatrix()]
  if(!is.null(InvMatrix)) {
    message("getting cached data")
    return(InvMatrix)
  }
  data <-  x[,getimatrix()]
  InvMatrix <- solve(data, ...)
  x[,setimatrix](InvMatrix)
  ## Return a matrix that is the inverse of 'x'
  InvMatrix
}
