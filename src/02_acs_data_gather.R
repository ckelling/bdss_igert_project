###
### Claire Kelling
### ACS Variable Collection
###
### Created 10/30/17 for gathering of ACS variables for Detroit
### 


library(acs)
#acs.tables.install()

#use my ACS API key 
ACS_key <- "8cd74c01f9459e6f923b7c8ff1979dfe35b47f9f"
api.key.install(key = ACS_key)

#make the region for which I want ACS data
acs.detroit <- geo.make(state = 'MI', county = 'Wayne County', tract = '*', block.group = '*')

#collecting ACS data for various categories:

#income
acs_income <- acs.fetch(geography = acs.detroit, endyear = '2015', table.number = 'B19001', col.names = 'pretty')
income <- data.frame(acs_income@geography$state,acs_income@geography$county,acs_income@geography$tract,
                     acs_income@geography$blockgroup, estimate(acs_income))

acs_income2 <- acs.fetch(geography = acs.detroit, endyear = '2015', table.number = 'B19013', col.names = 'pretty')
med_income <- data.frame(estimate(acs_income2))

#employment
acs_employment <- acs.fetch(geography = acs.detroit, endyear = '2015', table.number = 'B23025', col.names = 'pretty')
employment <- data.frame(estimate(acs_employment))

#total population
acs_tot_pop <- acs.fetch(geography=acs.detroit, endyear=2015, table.number="B01003", col.name="pretty")
total_pop <- data.frame(estimate(acs_tot_pop))

#poverty- all of the values are NA
#acs_poverty <- acs.fetch(geography=acs.detroit, endyear=2015, table.number="B17001", col.name="pretty")
#poverty <- data.frame(estimate(acs_poverty))

#gender and age (not separate)
acs_gen_age <- acs.fetch(geography=acs.detroit, endyear=2015, table.number="B01001", col.name="pretty")
gen_age <- data.frame(estimate(acs_gen_age))

#median age
acs_med_age <- acs.fetch(geography=acs.detroit, endyear=2015, table.number="B01002", col.name="pretty")
med_age <- data.frame(estimate(acs_med_age))

#measure of ethnic diversity
acs_race <- acs.fetch(geography=acs.detroit, endyear=2015, table.number="B02001", col.name="pretty")
race <- data.frame(estimate(acs_race))


#now, I will combine all of the relevant variables into a dataframe

#first, I must create the GEOID column, as this is not included in the acs geography
acs_dat <- data.frame(paste0(as.character(acs_income@geography$state), 
                             as.character(acs_income@geography$county), 
                             acs_income@geography$tract,
                             acs_income@geography$blockgroup))
colnames(acs_dat)[1] <- c("GEOID")

acs_dat$median_income <- med_income$B19013..Median.Household.Income.in.the.Past.12.Months..in.2015.Inflation.Adjusted.Dollars...Median.household.income.in.the.past.12.months..in.2015.Inflation.adjusted.dollars.
acs_dat$upemp_rate <- employment$Employment.Status.for.the.Population.16.Years.and.Over..In.labor.force./employment$Employment.Status.for.the.Population.16.Years.and.Over..Total.
acs_dat$total_pop <- total_pop$Total.Population..Total
acs_dat$perc_male <- gen_age$Sex.by.Age..Male./gen_age$Sex.by.Age..Total.
acs_dat$med_age <- med_age$Median.Age.by.Sex..Median.age....Total.
acs_dat$race_not_white <- 1- race$Race..White.alone/race$Race..Total.


# Creation of Herfendall Index: economic measure given by Dr. Corina Graif
acs_dat$herf_index <- rep(1,nrow(race))- ((race$Race..White.alone/race$Race..Total.)^2 +
  (race$Race..Black.or.African.American.alone/race$Race..Total.)^2 +
    (race$Race..American.Indian.and.Alaska.Native.alone/race$Race..Total.)^2+
    (race$Race..Asian.alone/race$Race..Total.)^2+
    (race$Race..Native.Hawaiian.and.Other.Pacific.Islander.alone/race$Race..Total.)^2+
    (race$Race..Some.other.race.alone/race$Race..Total.)^2+
    (race$Race..Two.or.more.races./race$Race..Total.)^2)


#how to find the GEOID for these blockgroups?
save(acs_dat, file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/acs_dat.Rdata")

