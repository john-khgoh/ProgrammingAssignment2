## These two functions compute and cache the inverse of a given matrix

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
	#Initializes variable i (which eventually stores the inverse matrix) to null
	i <- NULL
	
	#set function sets variable x (ordinary matrix) to argument y and i to null
	#note that variable y is assumed to be already a matrix itself as the question did not mention y being in other forms (e.g. list)
	set <- function(y)
	{
		x <<- y
		i <<- NULL
	}
	
	#get function returns the value of variable x (ordinary matrix)
	get <- function()
	{
		x
	}
	
	#setinv function sets i (inverse matrix) to argument inv
	#note that variable inv is assumed to be already a matrix itself as the question did not mention inv being in other forms (e.g. list)
	setinv <- function(inv)
	{
		i <<- inv
	}
	
	#getinv function returns i (inverse matrix)
	getinv <- function()
	{
		i
	}
	
	#makes "special matrix" (list) here containing the functions set,get,setinv,getinv
	#there is already a discussion on the coursera discussion forum regarding returning a matrix of functions vs a list of functions 
	#the general consensus is in favor of the list of functions
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## The cacheSolve function computes the inverse of the special matrix returned by makeCacheMatrix.  
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) 
{
    #tries to get inverse matrix
	i <- x$getinv()
	
	#checks if variable i is null. If not, cached value is returned
	if(!is.null(i))
	{
		message("getting cached data")
		return(i)
	}
	
	#get the ordinary (non-inverse) matrix
	mat <- x$get()
	
	#calculates the inverse matrix
	i <- solve(mat, ...)
	
	#sets variable i to the calculated inverse matrix
	x$setinv(i)
	
	#returns inverse matrix
	i
}
