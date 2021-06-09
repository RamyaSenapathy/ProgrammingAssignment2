makeCacheMatrix <- function(x= numeric()){          ##getting values   
  i <- NULL                                         ## i is sit to null
  set <- function(y){ 
          x <<- y        
          i <<- NULL         
        }   
  get <- function() x  
  setinverse <- function(inverse) i <<- inverse        ## changed mean function to inverse function
  getinverse <- function() i 
  
  list(set = set,get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
} 




cachesolve  <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")                 ## if already calculated message will get displayed
    return(i)
  }
  data <- x$get()                                 ## if i is null
  i <- solve(data, ...)                             ##actual function for inversing the matrix
  x$setinverse(i)
  i
}


test <- makeCacheMatrix(matrix(c(1:2, 2:3),nrow=2,ncol=2))      ## testing the code with matrix values
cacheinverse(test)                                             ## run twice :  testing if cache value is displayed 






