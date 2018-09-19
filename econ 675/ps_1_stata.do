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

	* get x and y matrices 
	y = st_data(., "earn78")
	x = st_data(., ("const", "treat", "black", "age", "educ", "educ_sq", "earn74","black_earn74", "u74","u75"))

	* calculate B using the symetric inverse 
	b = invsym(corss(x,x))*cross(x,y)



end



