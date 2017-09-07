## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  # Initialize the mat_inv into NULL
  # set new value to other func and set mt_inv to NULL
    
  mat_inv<- NULL
   
    
  set <- function(y) {
      x <<- y
      mat_inv <<- NULL
    }
    
  ## get the matrix values
  ## set the inverse matrix value
  ## get the inverse value
    
    get <- function() x
    setinv <- function(invs) mat_inv <<- invs
    getinv <- function() mat_inv
    list(set = set,get = get,setinv = setinv,getinv = getinv)     
}






## This function will calculate inverse , returned by above function
## If that already calculated then it will take from chache

cacheSolve <- function(x, ...) {

      mat_inv <- x$getinv()
      
      if(!is.null(mat_inv)) {
        
        message("Found data in chache")
        
        return(mat_inv)
      }
      
      # Otherwise, compute the inverse matrix, and return value
      
      newvalue <- x$get()
      mat_inv <- solve(newvalue)
      x$setinv(mat_inv)
      mat_inv
}

