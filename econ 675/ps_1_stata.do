clear all
set more off, perm

* set working directory 
global dir "C:\Users\Nmath_000\Documents\MI_school\Second Year\675 Applied Econometrics\hw\hw1"

*import data 
import delimited using "$dir\LaLonde_1986.csv"
 

* question 2 * 

* create needed variables 
gen educ_sq = educ^2
gen black_earn74 = black*earn74
gen const = 1

* store needed variables in locals 
*local y earn76
*local x const treat black age educ educ_sq earn74 black_earn74 u74 u75 

* use mata
mata:


y = st_data(., "earn78")
x = st_data(., ("const", "treat", "black", "age", "educ", "educ_sq", "earn74","black_earn74", "u74","u75"))

b = invsym(cross(x,x))*cross(x,y)

bc = cholinv(cross(x,x))*cross(x,y)

diff = b-bc

diff

my_resid = y - x*b
d = diag(my_resid:*my_resid)

v = invsym(cross(x, x))*(x' * d * x) * invsym(cross(x, x)) 
      
se = sqrt(diagonal(v))	  
	  
tstat = b :/ se	  

 p_value = 2*(1- pt((abs(t_test)), n_row))

end



