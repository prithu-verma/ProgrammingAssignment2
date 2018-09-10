## makeCacheMatrix: Creates a matrix object to cache its inverse.
## cacheSolve: Retrives inverse stored if available, otherwise calls makeCacheMatrix().
## timetest: Function to check time taken to run the two functions.

## This function creates a special "matrix" object that can cache its inverse.
## Sub-Functions:
## set()
## get()
## setinv()
## getinv()

makeCacheMatrix<-function(x=matrix())
{
	invM<-NULL
	inverse<-NULL
	set<-function(y)
	{
		x<<-y
		invM<<-NULL
	}
	get<-function() {x}
	setinv<-function(inverse) {invM<<-inverse}
	getinv<-function() {inverse}
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix()
## above. If the inverse has already been calculated (and the matrix has not changed), then
## the cacheSolve() should retrieve the inverse from the cache.

cacheSolve <- function(x, ...)
{
	invM<-x$getinv()
	if(!is.null(invM))
	{
		message("Processing Data from Cache...")
		return(invM)
	}
	data<-x$get()
	invM<-solve(data, ...)
	x$setinv(invM)
	invM
}

## This function tests the time taken to run makeCacheMatrix() and cacheSolve().

timetest<-function(matrix)
{
	temp<-makeCacheMatrix(matrix)
	start.time<-Sys.time()
	cacheSolve(temp)
	duration<-Sys.time()-start.time
	print(duration)
}
