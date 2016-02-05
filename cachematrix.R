## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL

	get<-function() x
	set<-function(y){
		x<<-y
		inv<<-NULL
	}

	getinv<-function() inv
	setinv<-function(y) inv<<-y
	list(get=get,set=set,getinv=getinv,setinv=setinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv()

	if(!is.matrix(inv)){
		inv<-solve(x$get())
		x$setinv(inv)}
	else {
		print("Inverse from Cache")}
	inv
}
