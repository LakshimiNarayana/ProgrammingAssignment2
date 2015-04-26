Put comments here that give an overall description of what your
makeCacheMatrix <- function(x = matrix()) {
        ##             Will use the Set and get fn’s for  the matrix inversed
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}
cacheSolve <- function(x, ...) {
                ## Inverse of the matrix will be returned
        inv = x$getinv()
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        # In the above method, it calculates the inverse, if not here it calculates the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        x$setinv(inv)
        
        return(inv)
}
test = function(mat){
        ## mat variable helps in the invertible matrix
        temp = makeCacheMatrix(mat)
        start.time = Sys.time()
        cacheSolve(temp)
        dur = Sys.time() - start.time
        print(dur)
        start.time = Sys.time()
        cacheSolve(temp)
        dur = Sys.time() - start.time
        print(dur)
}
