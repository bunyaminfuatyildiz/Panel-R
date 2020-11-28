#Panel STUDY IN R i used the data i have applied 
#BUNYAMIN PANEL  WORK we will see something publishable in some weeks.
setwd("C:/Users/bunya/rwork")
library(plm)
library(DataExplorer)
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(pastecs)
library(car)
panel <- read.csv("C:/Users/bunya/rwork/lovelycsv.csv") #sometimes csv import names awkward
lovelydata <- read_excel("lovelydata.xlsx")
#choose one way excel or csv
View(lovelydata)
DataExplorer::plot_missing(panel)
panel<- plm.data(lovelydata, index=c("id", "time"))
panel <- pdata.frame(lovelydata, index = c("id", "time"))


# Generate log 
panel$logurbanization=log(panel$urb)
panel$logunemployment=log(panel$unem)
panel$logprisonincarceration=log(panel$prisoner)

# dont use mutate 
panel %>% mutate(log_urb = log(urb))
panel %>% mutate(log_pri = log(prisoner))


#load the data and label the two datasets 
library(tidyverse)
library(caret)



panel = select(panel, -3, -4,-8,-9)
analyse = select(panel, -1, -2)
summary(analyse)
stat.desc(analyse)
hist(analyse$growth)
hist(analyse$logunemployment)

# Shapiro Normality test
shapiro.test(panel$growth)
 
# correlation 
cor(analyse)
# might be multicollinearity problem between govefficiency and ruleoflaw possibly we will drop one of them

# Lets  normalize the growth data  
# MIN-MAX NORMALIZATION
newmin = 0
newmax = 1
#find min and max values?
min = min(panel$growth)
max = max(panel$growth)
for (i in 1:length(panel$growth)){
  x<-panel$growth[i]
  nx = ((x - min)/(max - min))*(newmax - newmin)+newmin
  panel$growth[i]<-nx
}

# Same for prison
newmin = 0
newmax = 1
#find min and max values?
min = min(panel$logprisonincarceration)
max = max(panel$logprisonincarceration)
for (i in 1:length(panel$logprisonincarceration)){
  x<-panel$logprisonincarceration[i]
  nx = ((x - min)/(max - min))*(newmax - newmin)+newmin
  panel$logprisonincarceration[i]<-nx
}
# Same for Unemployment
newmin = 0
newmax = 1
#find min and max values?
min = min(panel$logunemployment)
max = max(panel$logunemployment)
for (i in 1:length(panel$logunemployment)){
  x<-panel$logunemployment[i]
  nx = ((x - min)/(max - min))*(newmax - newmin)+newmin
  panel$logunemployment[i]<-nx
}

#Same for Urbanization 
newmin = 0
newmax = 1
#find min and max values?
min = min(panel$logurbanization)
max = max(panel$logurbanization)
for (i in 1:length(panel$logurbanization)){
  x<-panel$logurbanization[i]
  nx = ((x - min)/(max - min))*(newmax - newmin)+newmin
  panel$logurbanization[i]<-nx
}


#Same for ruleoflaw for ruleoflaw
newmin = 0
newmax = 1
#find min and max values?
min = min(panel$ruleoflaw)
max = max(panel$ruleoflaw)
for (i in 1:length(panel$ruleoflaw)){
  x<-panel$ruleoflaw[i]
  nx = ((x - min)/(max - min))*(newmax - newmin)+newmin
  panel$ruleoflaw[i]<-nx
}


#Same for goveff for goveff
newmin = 0
newmax = 1
#find min and max values?
min = min(panel$goveff)
max = max(panel$goveff)
for (i in 1:length(panel$goveff)){
  x<-panel$goveff[i]
  nx = ((x - min)/(max - min))*(newmax - newmin)+newmin
  panel$goveff[i]<-nx
}



#AFTER NORMALIZATION Process 
colnames(panel) <- c("time", "id","growth","ruleofl","govff","urban","unemp","prison")


analyse = select(panel, -1, -2)
summary(analyse)
stat.desc(analyse)

hist(analyse$rlfl)
hist(analyse$gvff)
hist(analyse$prison)
hist(analyse$unem)
scatter3d(prison ~ rlfl + gvff, data=analyse, id=list(n=3))

cor(analyse,)



#High Correlation between goveff and ruleoflaw use pca create new variabl
scatter3d(unem ~ rlfl + gvff, data=analyse, id=list(n=3))

pca_group <-select(analyse,-1,-4,-5,-6)
library(factoextra)


pcacalc <- prcomp(pca_group,  center = TRUE,scale. = TRUE )
pcacalc
View(pcacalc)
pcacalc[["x"]]
library(ggbiplot)

ggbiplot(pcacalc)

###pass here ####
res.pca <- prcomp(pca_group, scale = TRUE)
ind<- get_pca_ind(res.pca)
print(ind)
var <- get_pca_var(res.pca)
print(var)
head(var$contrib) # contributions of variables
#### pass here###


## write it to excel beyvb
library(writexl)
write_xlsx(panel,"C:\\Users\\bunya\\Desktop\\people.xlsx")

library(plm)

pcdtest(prison~ growth +urban +ruleofl+unemp, data=panel)
pcdtest(prison~ growth +urban +govff+unemp, data=panel)





prisonr.fe <- plm(prison~ growth +urban +ruleofl+unemp,data=panel, model="within")
prisongv.fe <- plm(prison~ growth +urban +govff+unemp, data=panel, model="within")
summary(prisonr.fe)
summary(prisongv.fe)

fixef(prisonr.fe)
fixef(prisongv.fe)
summary(fixef(prisonr.fe))
summary(fixef(prisongv.fe))
        
        
        
        
        
prisonr.re <- plm(prison~ growth +urban +ruleofl+unemp,data=panel, model="random")
prisongv.re <- plm(prison~ growth +urban +govff+unemp, data=panel, model="random")
summary(prisonr.re)
summary(prisongv.re)




prisonr.twfe <- plm(prison~ growth +urban +ruleofl+unemp,data=panel, model="within", effect="twoways")
prisongv.twfe <- plm(prison~ growth +urban +govff+unemp,data=panel, model="within", effect="twoways")


fglsr <- pggls(prison~ growth +urban +ruleofl+unemp, data=panel, model="pooling")
summary(fglsr)
fglsgv <-pggls(prison~ growth +urban +govff+unemp,data=panel, model="pooling")
summary(flgsgv)


##test of poolability
#the hypothesis that the same coefficients apply to each individual


prisonr.fe <- plm(prison~ growth +urban +ruleofl+unemp,data=panel, model="within")
prisongv.fe <- plm(prison~ growth +urban +govff+unemp, data=panel, model="within")
prisonr.pool <- plm(prison~ growth +urban +ruleofl+unemp,data=panel)
prisongv.pool <- plm(prison~ growth +urban +govff+unemp, data=panel)
pooltest(prisonr.pool,prisonr.fe)
pooltest(prisongv.pool,prisongv.fe)


#Tests for individual and time effects
plmtest(prisonr.pool, effect = "twoways", type = "ghm")
plmtest(prisongv.pool, effect="twoways", type = "ghm")

pFtest(prisonr.fe,prisonr.pool)
pFtest(prisongv.pool,prisongv.fe)


## Hausman 
#The null hypothesis is that the preferred model is random effects; 
#The alternate hypothesis is that the model is fixed effects

phtest(prisonr.fe, prisonr.re)
phtest(prisongv.fe, prisongv.re)

'''Tests of serial correlation
model with individual effects has composite errors that are serially correlated by definition.
the presence of the time-invariant error component gives rise to serial correlation which does
not die out over time, thus standard tests applied on pooled data always end up rejecting
the null of spherical residuals'''

#Unobserved effects test
'''The unobserved effects test a la Wooldridge s a semiparametric
test for the null hypothesis that There are no unobserved effects in the
residuals.While not rejecting the null favours the use of pooled OLS'''

pwtest(prison~ growth +urban +ruleofl+unemp, data=panel)
pwtest(prison~ growth +urban +govff+unemp, data=panel)



#Locally robust tests for serial correlation or random effects
'''The presence of random effects may affect tests for residual serial correlation, and the opposite
A joint LM test for random effects and serial correlation under normality and homoskedasticity of the
idiosyncratic errors has been derived by Baltagi and Li (1991) and Baltagi and Li (1995) and
is implemented as an option in pbsytest'''
pbsytest(prison~ growth +urban +govff+unemp, data=panel, test = "j")
pbsytest(prison~ growth +urban +govff+unemp, data=panel, test = "ar")
pbsytest(prison~ growth +urban +govff+unemp, data=panel, test = "re")


pbsytest(prison~ growth +urban +ruleofl+unemp, data=panel, test = "j")
pbsytest(prison~ growth +urban +ruleofl+unemp, data=panel, test = "ar")
pbsytest(prison~ growth +urban +ruleofl+unemp, data=panel, test = "re")

#Conditional LM test for AR(1) or MA(1) errors under random effects
'''Baltagi and Li (1995) derive a Lagrange multiplier test for serial
correlation in the idiosyncratic component of the errors under (normal, heteroskedastic) ran-
dom effects. Under the null of serially uncorrelated errors, the test turns out to be identical
for both the alternative of AR(1) and MA(1) processes. One- and two-sided versions are
provided, the one-sided having power against positive serial correlation only. The two-sided
is the default, while for the other one must specify the alternative option to onesided:'''
pbltest(prison~ growth +urban +govff+unemp, data=panel, alternative ="onesided")

pbltest(prison~ growth +urban +ruleofl+unemp, data=panel, alternative ="onesided")










#General serial correlation tests
'''objects retain the \demeaned" data, so the procedure is straightforward for them. The
wrapper functions pbgtest and pdwtest re-estimate the relevant quasi-demeaned model by
OLS and apply, respectively, standard Breusch-Godfrey and Durbin-Watson tests from pack-
age lmtest'''

pbgtest(prisonr.fe, order = 2)
pbgtest(prisongv.fe, order = 2)




#Wooldridge's test for serial correlation in \short" FE panels
pwartest(prison~ growth +urban +govff+unemp, data=panel)
pwartest(prison~ growth +urban +ruleofl+unemp, data=panel)



#first-difference-based test
'''In the context of the first difference model, Wooldridge (2002, 10.6.3) proposes a serial corre-
  lation test that can also be seen as a specification test to choose the most effcient estimator
between fixed effects (within) and first difference (fd).'''
pwfdtest(prison~ growth +urban +govff+unemp, data=panel)
pwfdtest(prison~ growth +urban +ruleofl+unemp, data=panel)








#### test for cross sectional dependence 
'''If XSD is present, the consequence is, at a minimum, inefficiency of the
usual estimators and invalid inference when using the standard covariance matrix'''
pcdtest(prison~ growth +urban +govff+unemp, data=panel)
pcdtest(prison~ growth +urban +ruleofl+unemp, data=panel)




##AR(1) pooling or random eects panel
'''Linear models with groupwise structures of time-dependence24 may be tted by gls(), spec-
ifying the correlation structure in the correlation option it neeeds time to be numeric for this panel[,"time"] <- as.numeric(panel[,"time"]) - 1''' 
panel[,"time"] <- as.numeric(panel[,"time"]) - 1
lmAR1MLrl <- gls(prison~ growth +urban +ruleofl+unemp, data = panel, correlation = corAR1(0, form = ~ time | id))


lmAR1MLgv <- gls(prison~ growth +urban +govff+unemp, data = panel, correlation = corAR1(0, form = ~ time | id))


#An LR test for serial correlation and one for random effects
'''A likelihood ratio test for serial correlation in the idiosyncratic residuals can be done as a
nested models test by anova(), comparing the model with spherical idiosyncratic residuals
with the more general alternative featuring AR(1) residuals The test takes the form of a zero
restriction test on the autoregressive parameter'''




''' random effects panel with, e.g., AR(1) errors (see Baltagi 2001, Chap-
ter 5), which is a very common specification in econometrics, may be t by lme specifying an
additional random intercept:'''

reAR1MLgv<-gls(prison~ growth +urban +govff+unemp, data = panel, random = ~ 1 |id, correlation = corAR1(0, form = ~ time | id))
