###
### Claire Kelling
### ACS Variable Collection
###
### Created 10/30/17 for gathering of ACS variables for Detroit
### 


library(acs)
acs.tables.install()

#use my ACS API key 
ACS_key <- "8cd74c01f9459e6f923b7c8ff1979dfe35b47f9f"
api.key.install(key = ACS_key)

#make the region for which I want ACS data
acs.detroit <- geo.make(state = 'MI', county = 'Wayne County', tract = '*', block.group = '*')

#collecting ACS data for various categories:

#income
acs_income <- acs.fetch(geography = acs.detroit, endyear = '2015', table.number = 'B19001', col.names = 'pretty')
income <- data.frame(estimate(acs_income), 0)

acs_income2 <- acs.fetch(geography = acs.detroit, endyear = '2015', table.number = 'B19013', col.names = 'pretty')
med_income <- data.frame(estimate(acs_income2), 0)

#employment
acs_employment <- acs.fetch(geography = acs.detroit, endyear = '2015', table.number = 'B23025', col.names = 'pretty')
employment <- data.frame(estimate(acs_employment), 0)

#total population
acs_tot_pop <- acs.fetch(geography=acs.detroit, endyear=2015, table.number="B01003", col.name="pretty")
total_pop <- data.frame(estimate(acs_tot_pop), 0)

#gender and age (not separate)
acs_gen_age <- acs.fetch(geography=acs.detroit, endyear=2015, table.number="B01001", col.name="pretty")
gen_age <- data.frame(estimate(acs_gen_age), 0)



