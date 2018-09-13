## The function creates a special "matrix" that then solves for its inverse and caches the answer

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  ## allows user to set matrix input for the function i.e var$set(matrix(...))
  set <- function(mat){
    x <<- mat  
    invmat <<- NULL
  }
  get <- function() x
  set_invmat <- function(invmat)
  ## solves inverse of matrix using solve()  
    invmat <<- solve(x)                      
  ## Once called get_invmat provides the inverse if calculated
  get_invmat <- function() 
    invmat
  
  list(set = set, get = get, set_invmat = set_invmat, get_invmat = get_invmat) 
  ## Allows for '$' operator to be used when executing commands in the console
}


## This function computes the inverse special "matrix" from the function makeCacheMatrix
## If the inverse of the matrix has already been solved cacheSolve will retrieve the matrix from the cache

cacheSolve <- function(x, ...) {
## Variable is input that was assigned value of makeCacheMatrix() in console
  invmat <- x$get_invmat()
    if(!is.null(invmat)){
      print("Getting cached data...")     ##Retrieves data from cache
      invmat
    }
    data <- x$get()
    invmat <- solve(data,...)
    x$set_invmat(invmat)
    invmat
## Returns inverse matrix of 'x'
}
