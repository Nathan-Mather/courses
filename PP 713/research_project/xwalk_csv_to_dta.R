xwalk <- data.table(read.csv("C:/Users/Nmath_000/Documents/data/pp713/fips_to_census_new.csv", header = FALSE))


library(haven)
library(data.table)


write_dta(xwalk, "C:/Users/Nmath_000/Documents/data/pp713/fips_to_census.dta")


setnames(xwalk, c("V1", "V2"), c("c_region", "bpl"))

# load acs data 
acs <- data.table(read_dta("C:/Users/Nmath_000/Documents/data/pp713/acs_small.dta"))




# merge them 
merged <- merge(acs, xwalk, by = c("bpl"))
