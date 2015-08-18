###### To check the functions:
## Create a matrix of a big number of rows and columns, p.e. X <- matrix(rnorm(3000*3000),nrow=3000) 
## mcm <- makecachematrix(X)
## X.inv <- cacheSolve(mcm)
## Iden <- X %*% X.inv
## If everything is fine, then we obtain the indentity matrix (we can check it getting the value of Iden[i,i] that should be 1 and all other values near 0)
######

## makeCacheMatrix make a list of functions (set matrix value,get matrix value,set matrix inverse,get matrix inverse)
## Everytime we introduce a new matrix this function saves the matrix and it's inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    
    #We create the list cotaining the matrix and it inverse 
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function get the matrix from the previous function makeCacheMatrix. It checks if the inverse of the matrix has been already
## calculated comparing the matrix with the vector created in makeCacheMatrix, if it's true then it give us the stored value, if not, then
## the function calculates the inverse matrix and store it in the vector.

cacheSolve <- function(x, ...) {
  
  ## We save the value of the inverse of the matrix (if exist), in "m"
  m <- x$getinverse()
  
  ## If m is NOT empty then the inverse of the matrix has been already calculated and it we get the stored value.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## if m is empty then we have to calculate the inverse of the matrix and store the value to the vector x
  data <- x$get()
  m <- solve(data, ...) # We get the inverse of the matrix with function solve()
  x$setinverse(m)
  m
}
