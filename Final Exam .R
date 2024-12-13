####final exam

##1

public_dummy <- ifelse(schoolvax$pubpriv == "PUBLIC",1,0) 

relig_exempt <- ifelse (schoolvax$religious> 0,1,0)

newschoolvax <- data.frame(schoolvax,public_dummy,relig_exempt)

##2

mean_values <- newschoolvax[,c( 'enrollment', 'allvaccs','conditional','medical','religious',
                             'dptMiss','polMiss','mmrMiss','hepMiss','varMiss','public_dummy','relig_exempt')]

###because I joined the schoolvax and dummy variables into a dataframe i am using the colMeans function instead of the mean function

colMeans(mean_values)



###3

medicalexempt<- lm (medical~pubpriv + enrollment, data = newschoolvax)
summary(medicalexempt)


###question4

##Based on the result above we can spot that the distribution of residuals have some outlier with a min value 
##of -0.3657 and a maximum of 15.1251 .

####now based on this result of the coefficients of -0.184 and the t test pvalue of 0.0712 for pubprivPublic ,coefficeint of  0.0017019 and 
#test pvalue of 0.0607  for enrollment

###we fail to reject the null hypothesis for pubpriv and enrollment  because they are both greater than the alpha of 0.05
###knowing this information above we can state that pubpriv(both public/ private status) and enrollment have weak evidence and are not statistically significant
##to influence the medical exemption rates and no further analysis should be done.

###Looking at the Ftest of 2.258 with a pvalue of 0.1053, we can fail to reject the null hypothesis and state that the model is statistically in significant on an alpha of 0.05
#in summary, the combination of pubpriv and enrollment doesn't significantly predict medical exemptions


###question5
###logistic regression model 

religexemptpredict<- glm(relig_exempt~public_dummy+enrollment, data =newschoolvax,family =binomial)
summary(religexemptpredict)

##question 6

##based on the result above 

##we are giving the Null hypothesis : Public school status doesn't affect the odds of students claiiming religious exemptions
  ###The alternative hypothesis:Public school status affects the odds of students claimin religious exemptions
###no looking at the Pvalue for Public dummy which is 0.753 which is larger than the 0.05 aplha value. this result let us know that the difference in religious exemptions between
###public and private schools is not statistically significant, which leads to us failing to reject the null hypotesis. there is no strong evidence 

##now we can calculate the odd ratio by taking the exponentiate of the coefficient of public_dummy -0.755  = e^-0.0755 = 0.927. which tells us that the odd 
###of claiming a religious exemption in public schools are about 92.7% of those in private, which is a very small effect.

##in conclusion ,the odds ratio indicates a slight decrease in the odds of religious exemptions for public schools compared to private schools, the p-value shows that this difference is not statistically significant. Therefore, public school status does not appear to meaningfully influence the likelihood of students claiming a religious exemption based on the data.

