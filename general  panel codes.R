###################################################
### chunk number 1: 
###################################################
options(prompt= "R> ")


###################################################
### chunk number 2: 
###################################################
library("plm")


###################################################
### chunk number 3: 
###################################################
data("EmplUK", package="plm")
data("Produc", package="Ecdat")
data("Grunfeld", package="Ecdat")
data("Wages",package="Ecdat")



###################################################
### chunk number 4: setdata1
###################################################
Wages <- plm.data(Wages, index = 595)



###################################################
### chunk number 5: setdata2
###################################################
EmplUK <- plm.data(EmplUK, index = c("firm", "year"))



###################################################
### chunk number 6: 
###################################################
log(emp)~lag(log(emp),1)+lag(log(emp),2)+lag(log(wage),2)+lag(log(wage),3)+diff(capital,2)+diff(capital,3)


###################################################
### chunk number 7: 
###################################################
dynformula(emp~wage+capital,log=list(capital=FALSE,TRUE),lag=list(emp=2,c(2,3)),diff=list(FALSE,capital=TRUE))


###################################################
### chunk number 8: 
###################################################
grun.fe <- plm(inv~value+capital,data=Grunfeld,model="within")
grun.re <- plm(inv~value+capital,data=Grunfeld,model="random")


###################################################
### chunk number 9: 
###################################################
summary(grun.re)


###################################################
### chunk number 10: 
###################################################
fixef(grun.fe)


###################################################
### chunk number 11: 
###################################################
summary(fixef(grun.fe))


###################################################
### chunk number 12: 
###################################################
grun.twfe <- plm(inv~value+capital,data=Grunfeld,model="within",effect="twoways")
fixef(grun.twfe,effect="time")


###################################################
### chunk number 13: 
###################################################
emp.amem <- plm(log(emp)~log(wage)+log(capital),data=EmplUK,model="random",random.method="amemiya")


###################################################
### chunk number 14: 
###################################################
grun.tways <- plm(inv~value+capital,data=Grunfeld,effect="twoways",model="random",random.method="amemiya")
summary(grun.tways)


###################################################
### chunk number 15: pbron
###################################################
emp.iv <- plm(emp~wage+capital|lag(wage,1)+capital,data=EmplUK,model="random")
emp.iv <- plm(emp~wage+capital|.-wage+lag(wage,1),data=EmplUK,model="random")
emp.iv <- plm(emp~wage+capital,instruments=~lag(wage,1)+capital,data=EmplUK,model="random")
emp.iv <- plm(emp~wage+capital,instruments=~.-wage+lag(wage,1),data=EmplUK,model="random")


###################################################
### chunk number 16: 
###################################################
form <- lwage~wks+south+smsa+married+exp+I(exp^2)+bluecol+ind+union+sex+black+ed|sex+black+bluecol+south+smsa+ind
ht <- plm(form,data=Wages,model="ht")
summary(ht)


###################################################
### chunk number 17: grunfeld.within
###################################################
grun.varw <- pvcm(inv~value+capital,data=Grunfeld,model="within")
grun.varr <- pvcm(inv~value+capital,data=Grunfeld,model="random")
summary(grun.varr)


###################################################
### chunk number 18: gmm
###################################################
emp.gmm <- pgmm(dynformula(emp~wage+capital+output,lag=list(2,1,0,1),log=TRUE),EmplUK,effect="twoways",model="twosteps",gmm.inst=~log(emp),lag.gmm=list(c(2,99)))
summary(emp.gmm)


###################################################
### chunk number 19: pggls
###################################################
zz <- pggls(log(emp)~log(wage)+log(capital),data=EmplUK,model="random")
summary(zz)


###################################################
### chunk number 20: 
###################################################
zz <- pggls(log(emp)~log(wage)+log(capital),data=EmplUK,model="within")


###################################################
### chunk number 21: 
###################################################
znp=pvcm(inv~value+capital,data=Grunfeld,model="within")
zplm=plm(inv~value+capital,data=Grunfeld)
pooltest(zplm,znp)


###################################################
### chunk number 22: 
###################################################
pooltest(inv~value+capital,data=Grunfeld,model="within")


###################################################
### chunk number 23: 
###################################################
g <- plm(inv ~ value + capital,data=Grunfeld,model="pooling")
plmtest(g,effect="twoways",type="ghm")


###################################################
### chunk number 24: 
###################################################
plmtest(inv~value+capital,data=Grunfeld,effect="twoways",type="ghm")


###################################################
### chunk number 25: 
###################################################
gw <- plm(inv ~ value + capital,data=Grunfeld,effect="twoways",model="within")
gp <- plm(inv ~ value + capital,data=Grunfeld,model="pooling")
pFtest(gw,gp)


###################################################
### chunk number 26: 
###################################################
pFtest(inv~value+capital,data=Grunfeld,effect="twoways")


###################################################
### chunk number 27: 
###################################################
gw <- plm(inv~value+capital,data=Grunfeld,model="within")
gr <- plm(inv~value+capital,data=Grunfeld,model="random")
phtest(gw, gr)


###################################################
### chunk number 28: wtest
###################################################
pwtest(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp, data=Produc)


###################################################
### chunk number 29: pbsytestJoint
###################################################
pbsytest(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp,data=Produc,test="J")


###################################################
### chunk number 30: pbsytestAR
###################################################
pbsytest(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp,data=Produc)


###################################################
### chunk number 31: pbsytestRE
###################################################
pbsytest(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp,data=Produc,test="RE")


###################################################
### chunk number 32: pbltest
###################################################
pbltest(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp,data=Produc,alternative="onesided")


###################################################
### chunk number 33: generalAR
###################################################
## this can be taken away as soon as attached to plm.rnw
grun.fe <- plm(inv ~ value + capital, data = Grunfeld, model = "within")
pbgtest(grun.fe, order=2)


###################################################
### chunk number 34: pwartest
###################################################
pwartest(log(emp) ~ log(wage) + log(capital), data=EmplUK)


###################################################
### chunk number 35: pwfdtest1
###################################################
pwfdtest(log(emp) ~ log(wage) + log(capital), data=EmplUK)


###################################################
### chunk number 36: pwfdtest2
###################################################
pwfdtest(log(emp) ~ log(wage) + log(capital), data=EmplUK, h0="fe")


###################################################
### chunk number 37: pcdtest1
###################################################
pcdtest(inv~value+capital, data=Grunfeld)


###################################################
### chunk number 38: pcdtest2
###################################################
pcdtest(inv~value+capital, data=Grunfeld, model="within")


###################################################
### chunk number 39: 
###################################################
library("lmtest")
re <- plm(inv~value+capital,data=Grunfeld,model="random")
coeftest(re, vcov = pvcovHC)


###################################################
### chunk number 40: 
###################################################
coeftest(re, vcov = pvcovHC(re,method="white2",type="HC3"))


###################################################
### chunk number 41: 
###################################################
waldtest(re,update(re,.~.-capital),vcov=function(x) pvcovHC(x,method="white2",type="HC3"))


###################################################
### chunk number 42: 
###################################################
library("car")
linear.hypothesis(re, "2*value=capital", vcov.=pvcovHC)


###################################################
### chunk number 43: re
###################################################

data(Grunfeld, package="Ecdat")

reGLS<-plm(inv~value+capital,data=Grunfeld,model="random")

reML<-lme(inv~value+capital,data=Grunfeld,random=~1|firm)

coef(reGLS)

summary(reML)$coef$fixed



###################################################
### chunk number 44: vcmrand
###################################################
vcm<-pvcm(inv~value+capital,data=Grunfeld,model="random",effect="time")

vcmML<-lme(inv~value+capital,data=Grunfeld,random=~value+capital|year)

coef(vcm)

summary(vcmML)$coef$fixed



###################################################
### chunk number 45: vcmfixed
###################################################
vcmf<-pvcm(inv~value+capital,data=Grunfeld,model="within",effect="time")

vcmfML<-lmList(inv~value+capital|year,data=Grunfeld)



###################################################
### chunk number 46: gglsre
###################################################
sGrunfeld <- Grunfeld[Grunfeld$firm%in%4:6,]

ggls<-pggls(inv~value+capital,data=sGrunfeld,model="random")

gglsML<-gls(inv~value+capital,data=sGrunfeld,
            correlation=corSymm(form=~1|year))

coef(ggls)

summary(gglsML)$coef


###################################################
### chunk number 47: lmAR1
###################################################
lmAR1ML<-gls(inv~value+capital,data=Grunfeld,
             correlation=corAR1(0,form=~year|firm))


###################################################
### chunk number 48: reAR1
###################################################
reAR1ML<-lme(inv~value+capital,data=Grunfeld,random=~1|firm,
             correlation=corAR1(0,form=~year|firm))


###################################################
### chunk number 49: fetchcoefs
###################################################
summary(reAR1ML)$coef$fixed
coef(reAR1ML$modelStruct$corStruct,unconstrained=FALSE)


###################################################
### chunk number 50: LRar
###################################################
lmML<-gls(inv~value+capital,data=Grunfeld)
anova(lmML,lmAR1ML)


###################################################
### chunk number 51: LRarsubRE
###################################################
anova(reML,reAR1ML)


###################################################
### chunk number 52: LRre
###################################################
anova(lmML,reML)


###################################################
### chunk number 53: LRresubAR
###################################################
anova(lmAR1ML,reAR1ML)


