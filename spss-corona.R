library(haven)
library(dplyr)
library(tibble)
library(expss)
library(vcd)
#library(survey)

#read data
data <- read_spss('corona4.sav')

#simple recode
#1-same label:
data$q17recode<- recode(data$q17, 0:1~1, 2~2,3:4~3, with_labels = TRUE)

#2-different label:
data$tah2 <- recode(data$q23, 'diplom ya kamtar'=0:5~1,
                              'aali'=6:8~2)
#recode continous to categorical
data$ageGG <- recode(data$q21, 'zire30'=0:30~1,
                              '30ta50'=31:50~2,
                              '51~120'=51:120~3)
#function for age recode
agerecode <- function(a){
          recode(a, 'zire30'=0:30~1,
         '30ta50'=31:50~2,
         '51~120'=51:120~3)
}
#frequency table 
fre(data$tah2)
table(data$ageGG,useNA = 'ifany')

#crosstab
table(data$q15, data$edu)
ftable(data$q15~ data$edu)


#proportion on total
prop.table(table(data$q15, data$edu))
#proportion on row
prop.table(table(data$q15, data$edu),margin = 1)
#proportion on column
prop.table(table(data$q15, data$edu),margin = 2)

ftable(data$q15, data$edu)


#pivot, percentage
data %>%tab_cells(q15) %>%
  tab_cols(q20) %>%
  tab_stat_cpct() %>%
  tab_pivot()

#pivot, cases
data %>%tab_cells(q15) %>%
  tab_cols(q20) %>%
  tab_stat_cases() %>%
  tab_pivot()
calc_cro_cases(data2, q15, q20)
#percent on column  
calc_cro_cpct(data2, q15, q20)
#percent on total  
calc_cro_tpct(data2, q15, q20)
#percent on row  
calc_cro_rpct(data2, q15, q20)

#weight by frequency
data2 <- read_spss('corona6 final 1056 k weighted 99.3.13.sav')
fre(data2$q15, weight = data2$weight)
#crosstab with weight
data2 %>%
  tab_weight(weight) %>% 
  tab_cells(q15) %>%
  tab_cols(q20) %>%
  tab_stat_cases() %>%
  tab_pivot()

calc_cro_cases(data2, q15, q20, weight = weight)
#percent on column  
calc_cro_cpct(data2, q15, q20, weight = weight)
#percent on total  
calc_cro_tpct(data2, q15, q20,  weight = weight)
#percent on row  
calc_cro_rpct(data2, q15, q20,  weight = weight)

#significance test
a<- table(data$q15, data$edu)
summary(assocstats(a))
chisq.test(a)
cor(data$q27, data$q21, method = 'spearman')


