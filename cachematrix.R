## The functions below allow us to calculate the inverse of a square matrix and cache that result
## As the assignment states, "assume that the matrix supplied is always invertible" we are only working with square matrices

## makeCacheMatrix creates a special object, which can be used to return the inverse of an already calculated matrix
makeCacheMatrix<-function(x = matrix()) {
    m <- NULL
	#set the value of the vector
    set <- function (y) {
        x <<- y
        m <<- NULL
    }
	#get the value of the vector
    get <-function() x
	#set the value of the inverse
    setmatrix <- function(solve) m<<- solve
	
	#get the value of the inverse
    getmatrix <- function() m
    list (set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}

## the following function calculates the inverse, and sets the value of the inverse in the cache

cacheSolve<-function(x) {
	#check to see if the inverse has already been calculated
    m <- x$getmatrix()
    if(!is.null(m)){
		#if not NULL, return from cache
        message("getting cached data")
        return(m)
    }
	#Else, calculate the inverse of the data
    data <- x$get()
    m <- solve(data)
	#Set the value of the inverse in the cache via the setmatrix function
    x$setmatrix(m)
    m
}
