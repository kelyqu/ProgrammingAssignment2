## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## set_matrix - set values of a matrix 
## get_matrix - get values of a matrix 
## set_inverse - set values of an inverse 
## get_inverse - get values of an inverse
makeCacheMatrix <- function(x = matrix()) {
  
    i <- NULL     			                        
  
    set_matrix <- function(y) {			 
    
        x <<- y 					                     
        
        i <<- NULL                             
    } 
  
    get_matrix <- function() x 				                
  
    set_inverse <- function(inverse) i <<- inverse 	
  
    get_inverse <- function() i 			          
  
    list(set_matrix = set_matrix, get_matrix = get_matrix, 
      set_inverse = set_inverse, 
      get_inverse = get_inverse)                      
  

}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
## check if the inverse (i) exists in cache, if yes, return it directly
## otherwise, calculate the inverse, and set it into cache using set_inverse, and return it
cacheSolve <- function(x, ...) {

    i <- x$get_inverse()    	
  
    if(!is.null(i)) {					## check if it is in cache
    
        message("getting cached data")			
      
        return(i)             
    } 
  
    data <- x$get_matrix()					
  
    i <- solve(data, ...)			
  
    x$set_inverse(i)					
  
    i 		                    
  
}
