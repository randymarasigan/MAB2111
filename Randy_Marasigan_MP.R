#--------------------------------------------------------------------
# Submitted by: Randy A. Marasigan
# MAB-2111
#
# Assignment 02 in R AKA "Machine Problem"
#
#--------------------------------------------------------------------
# Ch0ose 5 Problems as required:
# 1. Define an R function that removes NA values from a vector.
# 2. Define an R function that computes the factorial of given an integer argument. The output should be a vector of length 1.
# 3. Define an R function that computes the determinant of a given matrix. The output should be a vector of length 1.
# 4. Define an R function that sorts a given vector in decreasing order. The output should be a vector of the same length. It should accept both numeric or character vectors.
# 5. Define an R function that accepts a Date (POSIXct) as argument and outputs the day of the week as characters. Use modulo operator.
# 6. Create a function to compute for your net pay at work.
# 7. Create a function that accepts a vector and and integer n and returns nth highest number
# 8. Create a function that computes the compound interest of an investment given the rate, time, and initial amount or principal.
# 9. Create a function isPrime(n) that accepts an integer and outputs a Boolean value (TRUE or FALSE) depending whether the integer is a prime number or not.

#----------------------------------------------------------------------
# My selected 5 problems/Functions
# 2. Define an R function that computes the factorial of given an integer argument. The output should be a vector of length 1.
# 4. Define an R function that sorts a given vector in decreasing order. The output should be a vector of the same length. It should accept both numeric or character vectors.
# 6. REQUIRED: Create a function to compute for your net pay at work 
# 8. Create a function that computes the compound interest of an investment given the rate, time, and initial amount or principal.
# 9. Create a function isPrime(n) that accepts an integer and outputs a Boolean value (TRUE or FALSE) depending whether the integer is a prime number or not.
#----------------------------------------------------------------------
# Not selected for assignment answer but created as part of option
# 1. Define an R function that removes NA values from a vector.
# 5. Define an R function that accepts a Date (POSIXct) as argument and outputs the day of the week as characters. Use modulo operator.
# 7. [added to Sorting] Create a function that accepts a vector and and integer n and returns nth highest number


#---------------------------------------------------------------------
# Assignement code starts here:
#---------------------------------------------------------------------

#No. 1
#----------------------------------------------------------------------
# 2. Define an R function that computes the factorial of given an integer argument. 
# The output should be a vector of length 1.
#--------------------------------------------------------------------
# factorial using for loop
factorial <- function(x){
  # if input is zero, factorial is zero
  if (x == 0) {
    y <- 0
    return(print(paste("Factorial:",y)))
    # no negative numbers please!    
  } else if (x < 0){
    #negative numbers are not allowed
    return("Negative numbers are not allowed for factorial!")
  } else if ((round(x) -x) != 0 ){
    #input should be zero or positive integers; no decimals
    return("Please input only zero or positive integers!")
  }
  #compute factorial 
  y <- 1
  for(i in 1:x){
    y <-y*((1:x)[i])
  }
  #return(y)
  return(print(paste("Factorial:",y)))
}

#function call test cases
factorial(0)
factorial(2)
factorial(5)
factorial(5.2)
factorial(10)
factorial(-2)

#No.2
#----------------------------------------------------------------------------------------
# 4. Define an R function that sorts a given vector in decreasing order. 
# The output should be a vector of the same length. It should accept both numeric or character vectors.
#-------------------------------------------------------------------------------------
# Algorithm = "Selection Sort"
# Arrange vector in decreasing order using Selection sort algorithm:
# 1. Find the largest value in the array and move it to a result array.
# 2. If there is more than 1 value remaining, repeat the above step on the rest.
# 3. The sorted result is Largest to smallest.
#--------------------------------------------------------------------------------------
selectionSort <- function(vec) {
  # Find the smallest value in the list 
  largest <- max(vec)
  rest <- vec[vec != largest]
  
  if (length(rest) > 1) {
    rest <- selectionSort(vec[vec != largest])
  }
  c(largest, rest)
}

#TEST INPUTS
A<-c(18, 16, 8, 7, 6, 3, 11, 9, 15, 1)
B<-c("a", "c", "z", "v", "r", "b","t")
C<-c("az", "ac", "aa", "v", "r", "b","t")
#TEST the function
selectionSort(A)
selectionSort(B)
selectionSort(C)


# ADDITIONAL/BONUS since this is super simple; just call already created
# Sorting function and return the nth value in the vector
# Return the nth highest number
# using insertion sort function
nth_largest <- function(vec,nth){
  return(selectionSort(vec)[nth])
}

nth_largest(A, 4)

# No. 3
#----------------------------------------------------------------------------------------
# 6. REQUIRED: Create a function to compute for your net pay at work 
#-----------------------------------------------------------------------------------
# Compute monthly net pay based on these input parameters:  
# 1. Your monthly basic pay
# 2. Non-taxable allowance
# 3. Taxable allowance
# 4. Guaranteed Months (default is 13)
# Addt'l deductions: Computed values based on Monthly basic: (Pagibig, SSS, PhilHealth)
# Assumptions for this code is that you are employed in a private company
# Not in scope: other deductions or exemptions
#NOTE:
#Run all the functions first before running the Main function which is monthly net
#-----------------------------------------------------------------------------------
PhilHealth <- function(ph){
  # Get contribution of employee share; only either 100, 137.5, or 550
  if (ph < 10000){
    ph = 137.5
  } else if (ph > 10000 && ph < 40000){
    ph = 100
  } else {
    #(ph > 40000)
    ph = 550
  }
  return(ph)
}

SSS <- function(bs){
  # The current SSS contribution rate is 11% of the monthly salary credit not exceeding P16,000 
  # and this is being shared by the employer (7.37%) and the employee (3.63%).
  # Get monthly basepay multiplied by 3.63%
  #This is the simplified computations since the SSS table computation is very long
  bs = bs*0.0363
  return(bs)
}


PagIbig <- function(pbg){
  #The maximum Pag-IBIG contribution is 2% or PHP 100 per month for members 
  #earning PHP 5,000 and above per month
  if (pbg < 5000){
    pbg = pbg * 0.01
    return(pbg)
  } else
    pbg = 100
  return(pbg)
}

# MAIN FUNCTION
monthly_net = function(basic,ntaxable=0,taxable=0,gmonths=13){
  #get annual
  annual = (basic + taxable) * gmonths
  
  # Based on PH TAX table
  if (annual <=  250000){
    #250K and below = None(0%)
    net = annual
  } else if (annual <= 400000) {
    #250K to 400k = 20% of excess over 250k
    net = annual - (annual - 250000) * 0.2
  } else if (annual <= 800000) {
    #400k to 800 = 30k + 25% of excess over 400k
    net = annual - (annual - 400000) * 0.25 - 30000
  } else if (annual <= 2000000) {
    #800k to 2M = 130K + 30% of excess over 800K
    net = annual - (annual - 800000) * 0.30 - 130000
  } else if (annual <= 8000000) {
    #2M to 8M = 490K + 32% of excess over 2M
    net = annual - (annual - 2000000) * 0.32 - 490000
  } else {
    #Above 8M = 2.41M + 35% of excess over 8M
    net = annual - (annual - 8000000) * 0.35 - 2410000
  }
  #get monthly salary
  monthly = net/gmonths
  # add the non taxable
  net_month = monthly + ntaxable
  # get the government decuctions
  govt_deduct <- PhilHealth(basic) + SSS(basic) + PagIbig(basic)
  #get final net 
  net_all <- net_month - govt_deduct
  #print net monthly pay, 2 digits decimal
  print(paste("Your net pay is:", round(net_all, digits=2)))
  #return(net_all)
}

#test the function; input just the basic salary
monthly_net(100000)
#input guide: You can replace the values
monthly_net(basic=100000,ntaxable=1000,taxable=1000)
monthly_net(basic=100000,ntaxable=0,taxable=0)

# No. 4
#------------------------------------------------------------------------------
# 8. Create a function that computes the compound interest of an investment given
# the rate, time, and initial amount or principal.
#------------------------------------------------------------------------------
# parameters: Principal, interest rate, compound_n, time_years)
#Calculate Accrued Amount (Principal + Interest)
#Where A=P(1 + r/n)^nt
# compounding (p,r,n,t)
#Compound Interest Input
# P = the principal investment amount (the initial deposit or loan amount)
# r = the annual interest rate (decimal)
# n = the number of times that interest is compounded per year
# t = the number of years the money is invested or borrowed for

# input your parameter values here: or put it directly to the function call as parameters
P <- 10000 #Principal Amount
r <- 0.1   #Annual Interest rate (in decimal)
n <- 1     #Calculation Period
t <- 20    #Years to grow

#Run the compounding function
compounding <- function(P,r,n,t){
  nt <- (n * t)
  # Amount is equal to 
  A <- (P * ((1 + r/n)^nt))
  # Interest only
  I <- (A - P)
  print(sprintf("Principal: %i", P))
  print(sprintf("Your money in %i years is: %.2f", t,A))
  print(sprintf("Compounding Interest: %.2f", I))
}

#test the function compounding
compounding(P,r,n,t)
compounding(25000,0.1,1,20)

# No. 5
#---------------------------------------------------------------------------------------
# 9. Create a function isPrime(n) that accepts an integer and outputs a Boolean value (TRUE or FALSE)
# depending whether the integer is a prime number or not.
#---------------------------------------------------------------------------------------
# PRIME NUMBER function
# A prime number is a positive integer that has exactly two positive integer factors, 1 and itself.
#Here are the first few prime numbers:
# 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 
# 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 
#179, 181, 191, 193, 197, 199, etc.

# isPrime function
isPrime <- function(n) {
  if (n == 2) {
    TRUE
  } else if (any(n %% 2:(n-1) == 0)) {
    FALSE
  } else { 
    TRUE
  }
}

# Test the function isPrime
isPrime(7) #TRUE
isPrime(0) #FALSE
isPrime(-1) #FALSE
isPrime(-7) #FALSE
isPrime(28) #FALSE
isPrime(101) #TRUE


#ANSWERS TO NOT SELECTED FUNCTIONS 
#----------------------------------------------------------------------------
# Not selected for assignment answer but created as part of option
#-----------------------------------------------------------------------------

# 1. Define an R function that removes NA values from a vector.
# Algorithm: use for loop to travese the orig vector then put non-NAs to the new vector
# Function NAME = removena
# parameter = vec_input
removena <- function(vec_input) {
  #initialize vector
  vec <- c()
  #get length of the vector input
  len <- length(vec_input)
  # for i in 1 to vec_input length
  for (i in 1:len){
    if (!is.na(vec_input[i])){
      vec <- c(vec, vec_input[i])
    }
  }
  return (vec)
}

#test input
v <- c(NA,NA,NA,1,2,3,4,5,NA,45,"a","a",45)

#call removena
removena(v)

#---------------------------------------------------------------------------------------
# 5. Define an R function that accepts a Date (POSIXct) as argument and outputs the day 
# of the week as characters. Use modulo operator.

# Know the day of the week of the date input
# Take note that Epoch day "1970-01-01" is Thursday
# Also taake note of the days based on epoch date
# 0 = Thursday
# 1 = Friday
# 2 = Saturday
# 3 = Sunday
# 4 = Monday
# 5 = Tuesday
# 6 = Wednesday

#create function what_day
what_day <- function(date_input){
  #Since R's first vector starts at 1, we create a vector which starts at 1 = Thursday
  day_of_week <- c("Thursday","Friday", "Saturday", "Sunday", "Monday", "Tuesday","Wednesday")
  unclass_date <- unclass(date_input)
  
  day <- (unclass_date %% 7) + 1 #plus 1 since if mod = 0 will return zero instead of Thursday [1]
  # if coerced to POSIXct use
  #(unclass(date_nw)/86400) %% 7) + 1
  #day <- ((unclass_date/86400) %% 7) + 1
  return(day_of_week[day])
}

#Accepts Date

date_input <- as.Date("2018-11-11")
date_input <- as.Date("2018-01-01")
date_input <- as.Date("2018-11-14")
# Test the Function Here:
what_day(date_input)

#----------------------------------------------------------------------------------------
#7. [added to Sorting] Create a function that accepts a vector and and integer n and 
# returns nth highest number

# Sorting function and return the nth value in the vector
# Return the nth highest number
# using selection sort function above
nth_largest <- function(vec,nth){
  return(selectionSort(vec)[nth])
}

nth_largest(A, 4)






