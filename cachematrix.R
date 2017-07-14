## caches the matrix

makeCacheMatrix <- function(x = matrix()) {  ## creates an R object named makeCacheMatrix that stores matrix x
  ##  and its inverse; x initialized as a matrix
  m<-NULL  ##initializes m to be null
  set<-function(y){  ##creates new function of y
    x<<-y  ##sets y to value of x in the parent (makeCacheMatrix) environment
    ## set function assigns the input to x in the parent environment
    m<<-NULL ## sets m to null
    ##this empties the cache so future calls will not retrieve the wrong value
    
  }
  get<-function()x  ## creates new function get that returns x from parent
  ##environment
  setinverse<-function(solve)m<<-solve  ##creates new function 
  ##assigns input argument to m in the parent environment
  getinverse<-function()m ## returns the inverse m from the parent environment
  list(set=set, get=get, ## creates a list of four named functions needs to do this: setting, creating,
       setinverse=setinverse, ## setting inverse value
       getinverse=getinverse) ## retrieving inverse value
  ##this set of functions is what's built by makeCacheMatrix function
  ##this list is returned to the parent environment
  
}
##object makeCacheMatrix is created to be used for cacheSolve, below


## invert, get cached inverse
##requires the stuff returned by makeCacheMatrix to get the cached value stored in the
##makeCacheMatrix object's environment
##cacheSolve contains a copy of the makeCacheMatrix's environment
##which includes the objects defined when makeCacheMatrix was defined
##these include the set, get, setinverse, getinverse, x, and m environments
##so cacheSolve can access them

cacheSolve <- function(x, ...) {  ##x and additional arguments passed to the fxn
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()  ##calls the getinverse function for the list associated 
  ##with x from the makeCacheMatrix function 
  ##can extract functions by name ($) 
  ##because they are explicitly named in the makeCacheMatrix list
  ##defines result as m
  if(!is.null(m)) {  ##if m is not NULL
    message("getting cached matrix inverse")  ##message printed
    return(m)  ##m printed
  }
  data <- x$get()  ##otherwise "data" is set as x and passed to the get function
  m <- solve(data, ...) ##m is set as the inverse of x
  x$setinverse(m)  ##m is passed to the setinverse function to be 
  ##stored as the cached inverse for future calls
  m  ##m printed
}
