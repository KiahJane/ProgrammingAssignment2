################## Assignment 2 - R Programming - Coursera - 07.09.2021 - Jones #####################################

## Function designed to cache Matrices
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinvers <- function(solve) inv <<- solve
  getinvers <- function() inv
  list(set = set, get = get, setinvers = setinvers, getinvers = getinvers)
}

## Function designed to cache Inverse-Matrices
cacheSolve <- function(x, ...) {
  inv <- x$getinvers()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  dat <- x$get()
  inv <- solve(dat, ...)
  x$setinvers(inv)
  inv
}

 

##### Testing ######
# Test 1
m1<- matrix(1:4, nrow = 2, ncol = 2)
myMatrix_object <- makeCacheMatrix(m1)

m1
myMatrix_object$get()

cacheSolve(myMatrix_object)
solve(m1)

# Test 2
m2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
myMatrix_object$set(m2)

m2
myMatrix_object$get()

cacheSolve(myMatrix_object)
solve(m2)












##### Notes #####
#-> answer is very close to example given 
#-> source to help understanding: 
# https://github.com/DanieleP/PA2-clarifying_instructions
# http://adv-r.had.co.nz/Functional-programming.html#closures
# https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprogAssignment2Prototype.md
# http://datasciencespecialization.github.io/
# https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-lexicalScoping.md


### Given assignment example:
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
v <- makeVector(c(1, 2, 3))
cacheMean(v)
cacheMean(v)  # Notice that the mean is returned from cache this time.


### Exmample of a Closure - function storing a list of funcs:
# from https://github.com/DanieleP/PA2-clarifying_instructions
plusFunctions <- function(){                      #-> function of funcs
  plustwo <- function(y){ 
    x <- y + 2
    return(x)}
  plusthree <- function(y){
    x <- y + 3
    return(x)}
  list(plustwo = plustwo, plusthree = plusthree) #-> stores the 2 funcs
}
a <- plusFunctions() 
a$plustwo(5) 
a$plusthree(5)

### Other Closure Examples:
# from http://adv-r.had.co.nz/Functional-programming.html#closures
power <- function(exponent){
  function(x){
    x ^ exponent
  }
}
square <- power(2)
square(2)
square(4)
cube <- power(3)
cube(2)
cube(4)

new_counter <- function(){
  i <- 0
  function(){
    i <<- i + 1              #-> '<<-' preserves 'i' across func calls -- parent (enclosing) environment remains unchanged
    i
  }
}
counter_one <- new_counter()
counter_one()                #-> 1st output is 1; 2nd output is 2; etc.
counter_one()
counter_two <- new_counter()
counter_two()                #-> output begins again at 1
