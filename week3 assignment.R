makeCacheMatrix <- function(x= numeric()){      
  i <- NULL 
  set <- function(y){ 
          x <<- y        
          i <<- NULL         
        }   
  get <- function() x  
  setinverse <- function(inverse) i <<- inverse     
  getinverse <- function() i 
  
  list(set = set,get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
} 




cacheinverse <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


test <- makeCacheMatrix(matrix(c(1:2, 2:3),nrow=2,ncol=2))


cacheinverse(test)






