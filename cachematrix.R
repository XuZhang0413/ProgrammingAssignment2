## The function is used to calculate the inverse of a matrix and return
## the result(inversed matrix). However, it could also restore the last result
## so that if the same inverse calculation happens again, it can save 
## the time and effort and simply retreive the restored data

## Write a short comment describing this function
## To accomplish the job metioned above, we need two functions: makeCacheMatrix() and cacheSolve(). 
## makeCacheMatrix is designed for creating an environment for restore elements such as x, m , get(), set()..

makeCacheMatrix <- function(x = matrix()) { #define x as an empty matrix so no error when selecting x$getinverse in function catchSolve()
  m <- NULL    #set m as NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x            # get x which is the original matrix
  setinverse <- function(dummy) m <<- dummy
  getinverse <- function() m     # get m which is the inversed matrix
  list(set = set, get = get,     # make a list for cacheSovle to use $ selecting elements
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve() function below is used for retrieving the stored result or creating a new result if necessary

cacheSolve <- function(x, ...) {
  m <- x$getinverse()  # retrive the restored m from makeCachMatrix's environment
  if(!is.null(m)){
    message("getting catched data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) # calculate the inversed matrix
  x$setinverse(m)
  m
  
  ## Return a matrix that is the inverse of 'x'
}


## for testing

m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1
myMatrix_object <- makeCacheMatrix(m1)
cacheSolve(myMatrix_object)
cacheSolve(myMatrix_object) #one more time to get the cache value


## thank you very much for grading my assingment!##





