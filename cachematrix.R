
## makeCacheMatrix function creates a special type of matrix that can hold its reverse
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


## Return a matrix that is the inverse of 'x'. If already calculated then retrieve the save inverse.
cacheSolve <- function(x, ...) {
        inv<-x$getinv()

	if(!is.matrix(inv)){
		inv<-solve(x$get(), ...)
		x$setinv(inv)}
	else {
		print("Inverse from Cache")}
	inv
}
