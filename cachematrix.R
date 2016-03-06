
##  This function creates a special "matrix" object
##  that can cache its inverse.



makeCacheMatrix <- function(x = matrix()){

    inv <- NULL
    set <- function(y)
      { x<<-y
        m<<-NULL}
    get <- function() x
    setinv <- function(solve) m<<-inv
    getinv <- function() inv
    list ( set=set,get=get,setinv=setinv,getinv=getinv)}


##  This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
##  already been calculated (and the matrix has not changed), then
##  `cacheSolve` should retrieve the inverse from the cache.



cacheSolve <- function(x, ...){
    inv <- x$getsolve
    if ( !is.null(inv))
    {message("getting cached data")
     return(inv)}
    data<-x$get()
    inv<- solve(data)
    x$setinv(inv)
    inv}
 