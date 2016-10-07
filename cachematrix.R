## Put comments here that give an overall description of what your
## functions do

# The functionality submitted is closely tied to the template code given in the 
# assignment. To achieve working code only the function calls needed changing
# (i.e., replacing "mean" with "solve".)  Describing what happens is more 
# interesting.
 
# The first function, makeCacheMatrix, is used to instantiate an object in R 
# which is similar to a 'class' in OOP languages. It contains 4 
# behaviors which are analogous to 'methods' of a 'class' under the OOP paradigm.
# One of these stores the input parameter. The others are inert until called, 
# and even if called nothing interesting happens unless cacheSolve has been 
# executed.

# Hence, if we create makeCacheMatrix(x) and initialize "a" as an object of this 
# type with an appropriate matrix as "x" the following commands do these things:
# a$get() --returns the submitted parameter from memory
# a$set() --resets the stored matrix or returns an error is no argument passed
# a$getsolve() --returns NULL until m is initialized by cacheSolve. Thereafter,
#     it returns the inverse matrix of x
# a$setsolve() -- used to cache the inverse matrix

# cacheSolve(x) calls the functions of makeCacheMatrix (read: method the the 
# makeCacheMatrix object to either return m, the inverse of the argument sub-
# mitted to makeCacheMatrix or initialize it and return the resulting value.)

# Note that the solution provided does not handle passing a singular matrix to
# the createCacheMatrix function. This could be handled by the is.singular()
# function available in the matrixcalc package.

# The following code will test the functions after they have been created (the 
# triple ### are intentional comments, the single # lines are meant as active
# code:

# Note that the conditional wrapper is one work-around I found in R to create
# a block comment which is not directly supported... You will need to create the
# functions defined below this code before executing it. Please verify creation
# of the functions in the global environment. By using the conditional wrapper,
# the contents of this file can be executed to create the functions without 
# triggering the test logic. Begin my clearing all objects in the global 
# environment. rm(list = ls()) will do this.
# Then one may execute the test logic inside the conditional wrapper line by 
# line starting at line 54. Nonstandard indentation used because the test code 
# will never execute under the conditional statement given that the condition is
# hard-coded to FALSE. REMINDER: Clearing the Global Environment and recreating 
# the functions is CRITICAL for multiple runs of the test code. Hope you enjoy!

if(FALSE) {
### Reminder: Make sure you have a clear global environment. Then execute the 
### entire page to create the functions.
### The previous command will      
          
### make a (most likely) non-singular matrix     
x <- matrix(rnorm(1:10000),100,100)

### If you want, uncomment the following to get and load the matrixcalc package
### in order to verify that we haven't unluckily got a non-invertable matrix
#install.packages("matrixcalc")
#library(matrixcalc)
#is.singular.matrix(x)
### Show it...
x

### Instantiate makeInverseMatrix
a <- makeCacheMatrix(x)
rm(x)

### Test some methods of a, the createCacheMatrix object
a$get() ### The initial matrix

### Note that at this point the object returned by a$get() was x. Yet this 
### object exists within the scope of 'a' and is not accessible to the global
### environment without reference to 'a'. We can remove x from the global 
### environment and still access the data from the inner scope.
rm(x) 
x # It's gone!!!
a$get() # Still returns data.


### The following is NULL because cacheSolve not yet called.
a$getsolve() ###The inverse matrix 

### Show the difference in performance and create the inverse matrix
### With a fast enough machine the difference may not show. If it doesn't, try 
### editing the initial matrix to make it larger.
system.time(b <- cacheSolve(a))
system.time(b <- cacheSolve(a))

### No longer NULL as cacheSolve has been called and cached the inverse
cacheSolve(a) ###The inverse matrix 

### Show that b really does contain the inverse
b
round(b %*% x, 5) ### Result is the identity matris. round() corrects for 
     ### slight rounding error visible on UNIX type operating systems. 
     ### Error is < 1^e-16
}


## Write a short comment describing this function. 
# Create the function that initiates a makeCacheMatrix object initializing the 
# value returned by <objectName>$get()
makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setsolve <- function(solve) m <<- solve
     getsolve <- function() m
     list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}


## Write a short comment describing this function
# Creates the function that returns the cached inverse matrix, if any, and 
# otherwise calls functions defined within makeCacheMatrix to calculate the 
# inverse and return it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getsolve()
     if(!is.null(m)) {
          #message("getting cached data") #Line is commented so as to not bias
          #performance test. 
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setsolve(m)
     m
}


