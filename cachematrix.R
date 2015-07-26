## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Calculating the inverse of a matrix could be a CPU-intense operation
# thus it makes sense to cache the solution, and have it available


# The first function, makeCacheMatrix creates a list containing a function to
#
#set the matrix  (set)
#get the matrix  (get)
#set the inverse matrix (setinverse)
#get the inverse matrix (getinverse)

makeCacheMatrix <- function(x = matrix()) {
# set the matrix and NULL the inverse(invm)
     invm <- NULL

     set <- function(y)

      {
        x<<-y
        invm <<- NULL

	}
# simply return the matrix
	get <- function() x
# way to write and keep the inverse (invm)
      setinverse <-function(inverse) invm <<-inverse
# way to return the inverse 
     getinverse <- function() invm
       list(set=set, get=get, setinverse = setinverse, getinverse=getinverse)

}


## Write a short comment describing this function
# this function gets the inverse, first it checks if it was already computed
# if not, then it calculates it, and sets it, so that next time this function
# is called, the cached data is returned


# assuming the matrix is invertable:

cacheSolve <- function(x, ...) {
# try to get the inverse matrix
           invm <- x$getinverse()
# if it exists then return the inverted matrix that was cached
        if(!is.null(invm)) {
        message("getting the cached inverted matrix")
        return(invm)
	}
# if the inverse did not exist, calculate it, set it, and return the calculated
# inverse matrix

	data <- x$get()
      invm <- solve(data)
        message("calculating inverted matrix")
      x$setinverse (invm)
      invm

}


#  Here is how I tested the code:
#
#  
#
# >x<-rbind(c(1,3,4),c(0.4,1,0),c(4,0,4))

# >matrix=makeCacheMatrix(x)
# >matrix$get()
#     [,1] [,2] [,3]
#[1,]  1.0    3    4
#[2,]  0.4    1    0
#[3,]  4.0    0    4
# when solving the first time, the inverted matrix is calculated:
#> cacheSolve(matrix)
#calculating inverted matrix
#           [,1]       [,2]        [,3]
#[1,] -0.2380952  0.7142857  0.23809524
#[2,]  0.0952381  0.7142857 -0.09523810
#[3,]  0.2380952 -0.7142857  0.01190476
# when solving again, the cached data is used:
# >cacheSolve(matrix)
#getting the cached inverted matrix
#           [,1]       [,2]        [,3]
#[1,] -0.2380952  0.7142857  0.23809524
#[2,]  0.0952381  0.7142857 -0.09523810
#[3,]  0.2380952 -0.7142857  0.01190476
# then I've decided to set the inverse as the matrix
# >matrix$set(cacheSolve(matrix))
# and afterwards solve it again:
# >cacheSolve(matrix)
#calculating inverted matrix
#     [,1] [,2]         [,3]
#[1,]  1.0    3 4.000000e+00
#[2,]  0.4    1 5.551115e-17
#[3,]  4.0    0 4.000000e+00
#
#  SUMMARY: function performs as intended, I also noticed due to limited
# precision in my example, after double inversion, one of the elements
# was not zero, but ~10^-17, which is close enough, but still important feature
# of numerical computations
