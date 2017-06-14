# Read in Medicaid data ------------------------------------------------------------
## read in libraries
libs = c('readxl','ggplot2','data.table','dplyr')
suppressWarnings(suppressPackageStartupMessages(lapply(libs, require, 
                                                       character.only = TRUE,
                                                       lib.loc = .libPaths()[1]
                                                       )))

# setwd('C:/Users/A770213/Desktop/pa_its') ## Set working directory


## read in PA medicaid file
# the original .xlsx file is here: http://listserv.dpw.state.pa.us/Scripts/wa.exe?A2=ind17&L=ma-food-stamps-and-cash-stats&F=&S=&P=2547
# the sheet with medicaid recipient totals ("MA Individuals-HistorybyCounty") has formatting that makes a direct import to R difficult. 
# I copy/pasted the needed sheet & saved as a .csv. I also transposed the data so that each county is a column & hard-coded the STATE TOTAL row before transposing.

pa_ma = setDT(read.csv('pa_medicaid_format.csv')) # read in file

cols = as.data.frame(apply(pa_ma[, 2:ncol(pa_ma), with=FALSE], 2, function(x) as.numeric(gsub(",", "", x)) )) # strip out commas

pa_ma = setDT(cbind(pa_ma[, 1, with=FALSE], cols)) # bind numeric & county columns

pa_ma[,month_yr := county] # rename county to date

pa_ma[,county := NULL] # delete county field

pa_ma[,':='(month = substr(month_yr, 1, 3)
            , month_n = ifelse(substr(month_yr, 1, 3) == 'Jan',sprintf("%02d",1),
                               ifelse(substr(month_yr, 1, 3) == 'Feb',sprintf("%02d",2),
                                      ifelse(substr(month_yr, 1, 3) == 'Mar',sprintf("%02d",3),
                                             ifelse(substr(month_yr, 1, 3) == 'Apr',sprintf("%02d",4),
                                                    ifelse(substr(month_yr, 1, 3) == 'May',sprintf("%02d",5),
                                                           ifelse(substr(month_yr, 1, 3) == 'Jun',sprintf("%02d",6),
                                                                  ifelse(substr(month_yr, 1, 3) == 'Jul',sprintf("%02d",7),
                                                                         ifelse(substr(month_yr, 1, 3) == 'Aug',sprintf("%02d",8),
                                                                                ifelse(substr(month_yr, 1, 3) == 'Sep',sprintf("%02d",9),
                                                                                       ifelse(substr(month_yr, 1, 3) == 'Oct',sprintf("%02d",10),
                                                                                              ifelse(substr(month_yr, 1, 3) == 'Nov',sprintf("%02d",11),
                                                                                                     ifelse(substr(month_yr, 1, 3) == 'Dec',sprintf("%02d",12),0 
                                                                                                            ))))))))))))
            , year = as.numeric(paste0("20",substr(month_yr, 5, 6) )) )] # create month & year variables

pa_ma[,quarter := ifelse(month %in% c('Jan','Feb','Mar'), sprintf("%02d",1),
                         ifelse(month %in% c('Apr','May','Jun'), sprintf("%02d",2),
                                ifelse(month %in% c('Jul','Aug','Sep'), sprintf("%02d",3),
                                       ifelse(month %in% c('Oct','Nov','Dec'), sprintf("%02d",4),0
                                         ))))] # create quarter variable

pa_ma[,':='(qtr_yr = paste0(quarter,"Q",substr(year,3,4))
            ,yr_qtr = as.numeric(paste0(year,quarter))
            ,yr_month = as.numeric(paste0(year,month_n)))] # concatenate year & quarter

## NOTE: Reporting methodology was changed in July 2014 to avoid duplicating individuals who moved to a new county in the same month.
# use only dates post-Jul 14

pa_ma13 = pa_ma[year >= 2013 & year <= 2016] # filter to 2014

## log curve in enrollees after 1/1/2015...
ggplot(data = pa_ma13, aes(x = as.character(yr_month), y = STATE.TOTAL, group=1)) + 
  geom_line() + 
  # scale_x_discrete(aes(breaks=as.character(yr_month)), labels = month_yr) +
  theme_bw() +
  ylim(0,3e6) +
  theme(axis.text.x = element_text(angle=90, vjust=.5))


# Read in county & state level population data ----------------------------
state_pop = setDT(read.csv('PEP_2016_state_pop.csv')) # all state populations
pa_pop = state_pop[GEO.display.label == 'Pennsylvania'] # limit to PA estimated population

pa_ma13[,ma_state_rate := ifelse(year == 2016, (STATE.TOTAL / pa_pop[,respop72016]) * 1000,
                                 ifelse(year == 2015, (STATE.TOTAL / pa_pop[,respop72015]) * 1000,
                                        ifelse(year == 2014, (STATE.TOTAL / pa_pop[,respop72014]) * 1000, 
                                               ifelse(year == 2013, (STATE.TOTAL / pa_pop[,respop72013]) * 1000, 0
                                               ))))]

pa_ma13[,':='(time = seq_along(yr_month) # set time variable 
             ,trend = ifelse(yr_month >= 201501, seq(1,24,1), 0) # set post-intervention trend variable
             ,level = ifelse(yr_month >= 201501, 1, 0) # set pre/post treatment 
              ) 
        ]

# plot PA's proportion on MA ----------------------------

ggplot(data = pa_ma13, aes(x = as.character(yr_month), y = ma_state_rate, group=1)) + 
  geom_line() + 
  geom_vline(aes(xintercept = which(levels(as.factor(yr_month)) %in% '201501')), linetype=2, col = 'red')+
  geom_vline(aes(xintercept = which(levels(as.factor(yr_month)) %in% '201407')), linetype=2, col = 'darkgrey')+
  scale_x_discrete(labels = pa_ma13$month_yr) +
  scale_y_continuous(labels = scales::comma, limits = c(100, 250)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust= .5, hjust = 1)) +
  labs(x = "Month & Year",
       y = "Population per 1,000 on medical assistance ",
       title = "Medicaid Enrollees in Pennsylvania:\nBefore & After Medicaid Expansion")



# preliminary model -------------------------------------------------------
library(nlme)
## linear fit
fit_lin = lm(ma_state_rate ~ level + time + trend, data = pa_ma13)
summary(fit_lin)
confint(fit_lin)
plot(resid(fit_lin))
plot(fit_lin) ## Observations 25, 26, 48

## non-linear fit
pa_ma13[,trend_sq := trend^2] # create squared trend term
fit_sq = lm(ma_state_rate ~ level + time + trend + trend_sq, data = pa_ma13)
summary(fit_sq)
plot(resid(fit_sq))
plot(fit_sq) # squared fit has much better residuals than linear fit

anova(fit_lin, fit_sq) # model fit is significantly different

acf(resid(fit_sq)) # ACF: exponential decay, lags at 1 & 7
acf(resid(fit_sq), type = 'partial') # PACF: no significant lag
## autoregressive structure: p = 1

library(car)
durbinWatsonTest(fit_sq, max.lag = 12, alternative = 'two.sided') # autocorrelation at lag 1 & 7

# no autocorrelation
fit_sq_auto = gls(ma_state_rate ~ level + time + trend + trend_sq,
                   data = pa_ma13,
                   # correlation=corARMA(p=1, form = ~time),
                   method = 'ML')
# autoregressive structure
fit_sq_auto1 = gls(ma_state_rate ~ level + time + trend + trend_sq,
                   data = pa_ma13,
                   correlation=corARMA(p=1, form = ~time),
                   method = 'ML')
# moving average
fit_sq_auto2 = gls(ma_state_rate ~ level + time + trend + trend_sq,
                   data = pa_ma13,
                   correlation=corARMA(q=7, form = ~time),
                   method = 'ML')

fit_sq_auto3 = gls(ma_state_rate ~ level + time + trend + trend_sq,
                   data = pa_ma13,
                   correlation=corARMA(q=1, form = ~time),
                   method = 'ML')


anova(fit_sq_auto, fit_sq_auto1, fit_sq_auto2, fit_sq_auto3) # significant differences b/n the 3 models

## save final model
# save(fit_sq_auto2, file='pa_medicaid_fit.RDA')

## Model using raw counts
fit_sq_auto_total = gls(STATE.TOTAL ~ level + time + trend + trend_sq,
                   data = pa_ma13,
                   correlation=corARMA(q=7, form = ~time),
                   method = 'ML')
# hard to interpret this...

# plot fit + counterfactual -------------------------------------------------------
# put fitted values into data.frames
plot_sq = data.frame(cbind(pa_ma13$yr_month,fitted(fit_sq_auto2)))

# Create counterfactual
cfac = summary(fit_sq_auto2)$coefficients['(Intercept)'] + 
  (summary(fit_sq_auto2)$coefficients['time'] * pa_ma13$time)
cfac = data.frame(cbind(
                  pa_ma13[,yr_month][24:length(pa_ma13[,yr_month])],
                  cfac[24:length(cfac)]))

## plot fitted against actuals + counterfactual
ggplot(data = pa_ma13, aes(x = as.character(yr_month), y = ma_state_rate, group=1)) + 
  geom_point() + 
  geom_line(data = plot_sq, aes(x=as.character(X1), y = X2, group = 1), col='forestgreen') +
  geom_line(data = cfac, aes(x=as.character(X1), y = X2, group = 1), col = 'forestgreen', linetype = 'dashed') +
  geom_vline(aes(xintercept = which(levels(as.factor(yr_month)) %in% '201501')), linetype=2, col = 'blue')+
  geom_vline(aes(xintercept = which(levels(as.factor(yr_month)) %in% '201407')), linetype=2, col = 'darkgrey')+
  scale_x_discrete(labels = pa_ma13$month_yr) +
  scale_y_continuous(labels = scales::comma, limits = c(100, 250)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust= .5, hjust = 1)) +
  labs(x = "Month & Year",
       y = "Population per 1,000 on medical assistance ",
       title = "Medicaid Enrollees in Pennsylvania:\nBefore & After Medicaid Expansion")

## difference in medicaid recipients per 
# absolute
fitted(fit_sq_auto2)[length(fitted(fit_sq_auto2))] - cfac$X2[length(cfac$X2)] # 35 / 1000 difference

# Relative
(fitted(fit_sq_auto2)[length(fitted(fit_sq_auto2))] - cfac$X2[length(cfac$X2)]) / 
  cfac$X2[length(cfac$X2)] # 18%


# difference between Trump / Not Trump ------------------------------------
votes = setDT(read.csv('pa_cty_election_res.csv')) # read in county votes data
trump = votes[TRUMP > CLINTON, COUNTY] # subset county votes to trump winners

## county population data 2010-2016
# https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?src=bkmk
county_pop = setDT(read.csv("PEP_2016_PEPANNRES.csv", stringsAsFactors = FALSE)) # bring in county population
county_pop[,county_name := sapply(strsplit(as.character(county_pop$GEO.display.label), split = " "), '[', 1)] # get county names
county_pop = county_pop[-1,]

# turn county population totals to numeric
nums = c('respop72010','respop72011','respop72012','respop72013','respop72014','respop72015','respop72016')
county_pop[,c(nums) := lapply(.SD, as.numeric), .SDcols = nums]
# county_pop = county_pop[county_name %in% trump]

# subset counties by candidate majorities
pop_trump = county_pop[county_name %in% trump] # trump counties
pop_clinton = county_pop[!county_name %in% trump] # clinton counties

# roll up county populations to total population by candidate & year
pop_clinton = data.frame(colSums(pop_clinton[,nums, with = FALSE])) # clinton
pop_clinton$year = c(2010,2011,2012,2013,2014,2015,2016) # create year for joining
rownames(pop_clinton) = NULL # remove rownames
names(pop_clinton)[1] = 'pop_clinton' # county total population for clinton
pop_trump = data.frame(colSums(pop_trump[,nums, with = FALSE])) # trump
pop_trump$year = c(2010,2011,2012,2013,2014,2015,2016) # create year for joining
rownames(pop_trump) = NULL # remove rownames
names(pop_trump)[1] = 'pop_trump' # county total population for trump

# total medicaid enrollees by month & county based on majority candidate
pa_ma13[,trump_medicaid := rowSums(pa_ma13[,which(names(pa_ma13) %in% trump), with=FALSE])] # Sum county medicaid popoulation where Trump won
pa_ma13[,clinton_medicaid := STATE.TOTAL - trump_medicaid] # Hillary wins  = state total - Trump medicaid

# join clinton/trump county populations by year
pa_ma13 = merge(pa_ma13, pop_trump, by = 'year', all.x = TRUE) # merge trump
pa_ma13 = merge(pa_ma13, pop_clinton, by = 'year', all.x = TRUE) # merge clinton
pa_ma13[,':='(clinton_ma_rate = clinton_medicaid / pop_clinton,
              trump_ma_rate = trump_medicaid / pop_trump)]

### Bind Clinton & Trump rates into long dataset
ma_clinton = pa_ma13[,list(yr_month, time, trend, level, trend_sq, trump = 0, ma_rate = clinton_ma_rate)] # clinton dataset
ma_trump = pa_ma13[,list(yr_month, time, trend, level, trend_sq, trump = 1, ma_rate = trump_ma_rate)] # trump dataset
ma_append = rbind(ma_clinton, ma_trump) # bind clinton & trump
ma_append[,':='(trump_time = trump * time,
                trump_level = trump * level,
                trump_trend = trump * trend,
                trump_trend_sq = trump * trend_sq,
                ma_rate = ma_rate * 1000)] # rate per 1,000

# save down both modeling files
# saveRDS(pa_ma13, file = 'pa_medicaid_fulldata')
# saveRDS(ma_append, file = 'medicaid_trump_clinton')


# plot trump vs hillary
ggplot(data = ma_append, aes(x=as.character(yr_month), y = ma_rate, group = as.character(trump), color = as.character(trump))) + 
  geom_line() +
  geom_vline(data = ma_append, aes(xintercept = which(levels(as.factor(yr_month)) %in% '201501')), linetype=2, col = 'blue')+
  theme_bw() +
  scale_x_discrete(labels = ma_append$month_yr) +
  scale_y_continuous(labels = scales::comma, limits = c(100, 300)) +
  theme(axis.text.x = element_text(angle=90, vjust= .5, hjust = 1)) +
  labs(x = "Month & Year",
       y = "Population per 1,000 on medical assistance",
       title = "Medicaid Enrollees in Pennsylvania:\nBefore & After Medicaid Expansion\nTrump vs Clinton")

## initial model
trump_fit_lin = lm(ma_rate ~ time + trump + trump_time + level + trend + trump_level + trump_trend,
                   data = ma_append)
summary(trump_fit_lin)
trump_fit_sq = lm(ma_rate ~ time + level + trend + trend_sq + trump + trump_time + trump_level + trump_trend + trump_trend_sq,
                  data = ma_append)
summary(trump_fit_sq)
anova(trump_fit_lin, trump_fit_sq) ## squared model provides a better fit + difference b/n linear & squared trend term

## test for autocorrelation
plot(trump_fit_sq) # error terms look good
acf(resid(trump_fit_sq)) # ACF: exponential decay, max 7
acf(resid(trump_fit_sq), type = 'partial') # PACF: no exponential decay
durbinWatsonTest(trump_fit_sq, alternative = 'two.sided', max.lag = 36)
library(nlme)

trump_sq_auto = gls(ma_rate ~ time + level + trend + trend_sq + trump + trump_time + trump_level + trump_trend + trump_trend_sq,
                  data = ma_append,
                  correlation=corARMA(q=7, form = ~time|trump),
                  method = 'ML')

trump_sq_auto1 = gls(ma_rate ~ time + level + trend + trend_sq + trump + trump_time + trump_level + trump_trend + trump_trend_sq,
                    data = ma_append,
                    correlation=corARMA(p=2, form = ~time|trump),
                    method = 'ML')

trump_sq_auto2 = gls(ma_rate ~ time + level + trend + trend_sq + trump + trump_time + trump_level + trump_trend + trump_trend_sq,
                     data = ma_append,
                     correlation=corARMA(p=1,q=1, form = ~time|trump),
                     method = 'ML')

summary(trump_sq_auto2)

anova(trump_sq_auto1, trump_sq_auto2)

## save final model
# save(trump_sq_auto2, file = 'trump_v_clinton_fit.RDA')

ma_fitted = cbind(ma_append,fit = fitted(trump_sq_auto2)) # join fitted values to dataset

# To do
# model the counterfactuals for trump vs control
# estimate the difference b/n counterfactual & trump

