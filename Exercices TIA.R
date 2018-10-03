############# EXAMEN 5 - TIA Quiz 

######### QUIZ 6 ---------------------------------------------------------------------------

##### Question 1 -----------------------------------
# we want to calculate the idicated rate, using two proection methods : Premium-based 
# and Exposure-based 

# we are given the indicated projected Loss Ratio, so we are going to use the 
# LossRatio method. We are missing the fixed exposure ratio and the variable 
# exposure ratio, but we have everything to calculate them 

# We start by calculating the % of fixed and variable premium 

general <- c(21600, 22000)
(fix_gen_p <- round(mean(0.75*general/c(480000, 502500)),3)) #earned premium
(var_gen_p <- round(mean(0.25*general/c(480000, 502500)),3)) #earned premium

license <- c(200, 195)
(fix_lic_p <- round(mean(license/c(40000, 39000)), 3)) # written premium

other <- c(45000, 48000)
(fix_other_p <- round(mean(0.75*other/c(500000, 525000)), 3)) #written premium 
(var_other_p <- round(mean(0.25*other/c(500000, 525000)), 3)) #written premium 

tax <- c(800, 780)
(var_tax_p <- round(mean(tax/c(40000, 39000)), 3))

commission <- c(4800, 4680)
(var_com_p <- round(mean(commission/c(40000, 39000)), 3))

#total fixed expense ratio 
(fix_ratio <- fix_gen_p + fix_lic_p + fix_other_p)

#total variable ratio
(var_ratio <- var_gen_p + var_tax_p + var_com_p + var_other_p)

# If we use the Premium-based method of projection, we can stop here and say that the 
# fixed expense will trend the same as the premium. 
# If we use the Exposure-based method of projection, we need to calculate the trended 
# fixed expense per exposure 

(fix_gen_e <- 0.75*general/c(580, 602))
(fix_lic_e <- license/c(50, 45))
(fix_other_e <- 0.75*other/c(600, 625))

(fix_e <- fix_gen_e + fix_lic_e + fix_other_e)

# the trended period is from the average written date of the historical period
# to the average written date of the rating period (07/01/2015)
# So we trend 3 years for 2012 and 2 years for 2013. 
# The trending factor is 2% (given in the question)

# we take the average of the trended fix expense per exposure ratio to determine 
# our final ratio per exposure.
(trend_fix_e <- round(mean(fix_e*c(1.02**3, 1.02**2)),3))

# Using the trended fix expense per exposure ratio, we calculate the fixed expense 
# ratio, dividing by the projected on-level AVERAGE premium, because :
# fixed expense ratio = fixed expense ($) / earned premium ($)

fix_ratio_e <- round((trend_fix_e/875), 3)

# We are now ready to calculate the indicated rate change 

(PLR <- 1 - 0.05 - var_ratio)

# Premium-based method :
(ind_change_pre <- (0.7+fix_ratio)/PLR - 1)

# Exposure-based method
(ind_change_exp <- (0.7+fix_ratio_e)/PLR - 1)







##### Question 2 -----------------------------------
# We are asked to calculate the expense fees
# EXPENSE FEE = (FIXED EXPENSES PER EXPOSURE)/PLR
# FIXED EXPENSE PER EXPOSURE = AVG PREMIUM * FIX_RATIO

# since we have the expenses in %, we need premiums to find ($)

exp <- c(500, 200, 400, 300)
rel <- c(1, 1.25, 0.80, 0.60)
# average rel of the other factors (except territory) = 1.40
(premium_terr <- sum(400*exp*rel*1.40))
(avg_prem <- premium_terr/sum(exp))

(fix_ratio <- sum(c(0.75*0.05, 0.75*0.1, 0.01)))
(fixed_expense_exp <- fix_ratio*avg_prem)

(PLR <- 1 - sum(c(15, 2, 0.25*5, 0.25*10 )/100) - 0.05)
(expense_fee <- fix_ratio*avg_prem/PLR)
# FINAL ANSWER : 82.49158 $ per exposure






##### Question 3 -----------------------------------
# we have a workers comp, with a discount percentage increasing by layer of premium

## a) calculate the total premium DISCOUNT PERCENTAGE 

(fix_ratio_range <- c(15+8, 12+6, 10+4, 9+3)/100)

(expense_reduction <- fix_ratio_range[1] - fix_ratio_range)

# the discount per range = expense reduction / PLR

(PLR <- 1-.04 - .01)

discount_range <- expense_reduction/PLR

# the standard premum is 675 000$, we have to split it by range
prem_range <- c(5000, 95000, 400000, 175000)

(prem_disc <- sum(discount_range*prem_range))

(tot_disc_rate <- round(prem_disc/675000,4))

## b) Calculate the rate per 1000$ of payroll for the policy

# payroll = 12 M$
# expense fee = 300$

(total_premium <- 675000 - prem_disc + 300)
# FINAL ANSWER 
(prem_1000 <- round(1000 * total_premium/12000000,3))














######### QUIZ 7 ---------------------------------------------------------------------------
##### Question 1 -----------------------------------

z <- C

































