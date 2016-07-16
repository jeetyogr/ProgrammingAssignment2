## In this function we're defining two functions 'makeCacheMatrix'and 'çacheSolve' to compute the inverse of a square matrix and cache the inverse for future use.
## Computing the inverse is a tedious task and doing so repeatedly incurs a lot of overhead in the code. In this code, we're assuming that the matrix is a square invertible matrix.

## The function 'makeCacheMatrix' creates a special matrix which returns a list of functions to
## 1. set value of matrix
## 2. get value of matrix
## 3. set value of inverse
## 4. get value of inverse

makeCacheMatrix <- function(x = matrix()) {
s <- NULL
  setmatrix <- function(y){                     ## Accepts new matrix as parameter, used to set a new value to matrix
      x <<- y
      s <<- NULL                                ## Erases cached result
  }
  
  getmatrix <- function() x                     ## Returns value of the current matrix
  setinverse <- function(inv) s <<- inv         ## Caches the inverse value returned fom cacheSolve function
  getinverse <- function() s                    ## Gives the inverse of the matrix when called
  list(setmatrix = setmatrix,
  getmatrix = getmatrix,setinverse = setinverse,
  getinverse = getinverse)                      ## Returns function values as a list
}


## The 'cacheSolve' function computes the inverse of the matrix using the solve() function. First we check if the mean is already calculated. If it exists we return the mean else we compute the mean using the solve function.

cacheSolve <- function(x, ...) {
        s <- x$getinverse()                     ## Calling the getinverse() function 
                if(!is.null(s)){                ## we try to find out if inverse is already calculated
                message("getting cached data")  ## if s in Not Null(value is already present) then return the value in s
                return(s)
        }
        mat <- x$getmatrix()
        if(det(mat) == 0){                      ## Inverse of a matrix cannot be calculated if determinant is equal to zero.
                s <- NULL                       ## Though we assume an invertible matrix, I have used the if loop to check determinant
                message("Inverse not found as determinant is zero")
                }
                else {
                s <- solve(mat, ...)            ## Compute the inverse of the matrix
                x$setinverse(s)                 ## setinverse function takes new value of inverse and caches it
                s
                }
}
