## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse. It has the following functionality
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of matrix
## 4. get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
      inverseMatrix <- NULL
      set <- function(y){
            x <<- y
            inverseMatrix <<- NULL
      }
      
      get <- function() x
      
      setInverseMatrix <- function(invMatrix) inverseMatrix <<- invMatrix
      
      getInverseMatrix <- function() inverseMatrix
      
      list ( set = set ,
             get = get ,
             setInverseMatrix = setInverseMatrix ,
             getInverseMatrix = getInverseMatrix)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
## The matrix supplied is always invertible.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverseMatrix <- x$getInverseMatrix()
      
      if(!is.null(inverseMatrix)){
            print('Fetching from cache')
            return(inverseMatrix)
      }
      
      data <- x$get()
      
      inverseMatrix <- solve(data,...)
      x$setInverseMatrix(inverseMatrix)
      inverseMatrix
}

## m<-matrix(1:4,2,2)
## x<-makeCacheMatrix(m)
## x$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## cacheSolve(x)
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## cacheSolve(x)
## Fetchig from cache
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5



## m<-rbind(c(-2,1.5),c(1,-0.5))
## m
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## x<-makeCacheMatrix(m)
## cacheSolve(x)
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## cacheSolve(x)
## [1] "Fetching from cache"
##       [,1] [,2]
## [1,]    1    3
## [2,]    2    

