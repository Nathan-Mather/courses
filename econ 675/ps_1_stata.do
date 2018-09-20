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

n_row = rows(x)
n_col = cols(x)

b = invsym(cross(x,x))*cross(x,y)

bc = cholinv(cross(x,x))*cross(x,y)

diff = b-bc

diff

my_resid = y - x*b
d = diag(my_resid:*my_resid:*(n_row/(n_row-n_col)))

v = invsym(cross(x, x))*(x' * d * x) * invsym(cross(x, x)) 
      
se = sqrt(diagonal(v))	  
	  
tstat = b :/ se	  

p_value = 2*ttail(n_row-n_col, abs(tstat))

CI_L = b - (se) * invt(n_row-n_col, .975 )
CI_U = b + (se) * invt(n_row-n_col, .975 )

all_data = b, se, tstat, p_value, CI_L, CI_U
all_data
end

// now run regression 
reg earn78 treat black age educ educ_sq earn74 black_earn74 u74 u75, robust

// nice, they match

