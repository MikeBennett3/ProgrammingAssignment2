#Assignment 2

#These two functions allow us to create a matrix
#object and a cahced copy of its inverse, to prevent
#repeatedly computing the same inverse several times.



makeCacheMatrix <- function(x = matrix())
#Creates a special matrix object in which a cached
#version of its inverse can be stored.
{
     inv <- NULL
     set <- function(y)
     {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     
     set_inverse <- function(inverse) { inv <<- inverse }
     
     get_inverse <- function() { inv }
     
     list(set = set, get = get,
          set_inverse = set_inverse,
          get_inverse = get_inverse)
}


cacheSolve <- function(x, ...)
#Finds the inverse of a cacheMatrix.
#If inverse has been computed previously,
#instead returns the cached inverse.
{
     s <- x$get_inverse()
     
     if(!is.null(s)) 
     {
          message("getting cached data")
          return(s)
     }
     data <- x$get()
     s <- solve(data, ...)
     
     x$set_inverse(s)
     s
}