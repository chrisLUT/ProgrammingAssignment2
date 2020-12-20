## Assignment 2: makeCacheMatrix and cacheSolve rely on
## R's lexical scoping rules to work. The first creates a
## list of functions storing their specific environment.
## Second calculates the inverse if no inverse was stored in
## the object ('NULL'). If a value != NULL exists as an inverse,
## this value will be returned.


## If called, creates object makeCachematrix - a list 
## with 4 functions (get, set, get_inv, set_inv). With 'set'
## a new matrix can be stored. 'Get' retrieves the current 
## matrix. 'Get_inv' retrieves inverse. 'Set_inv' can set 
## the inverse, but is meant to be only used by cacheSolve
## in this concept.


makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function(){
            x
      }
      set_inv <- function(inv_solved){
            inv <<- inv_solved
      } 
      get_inv <- function(){
            inv
      }
      list(set = set,
           get = get,
           set_inv = set_inv,
           get_inv = get_inv)
}

## Retrieves inverse from makeCacheMAtrix object.
## If inverse is not zero, return cached inverse. Else
## calculates inverse and stores it in object.

cacheSolve <- function(MCM, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- MCM$get_inv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- MCM$get()
      inv <- solve(data)
      MCM$set_inv(inv)
      inv
}


### RUN TEST

# Test matrix from forum
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)

# create object
MCMm1 <- makeCacheMatrix(m1)
# test set and get
m2 <- matrix(1:4, 2)
MCMm1$set(m2)
MCMm1$get()
MCMm1$set(m1)
MCMm1$get()

# test cacheSolve on object
cacheSolve(MCMm1) # set inverse
cacheSolve(MCMm1) # get cached inverse

# test get_inv()
MCMm1$get_inv()

