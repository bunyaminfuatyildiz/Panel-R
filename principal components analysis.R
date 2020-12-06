library("FactoMineR")
library("factoextra")
"Taken	together,	the	main	purpose	of	principal	component	analysis	is	to:
identify	hidden	pattern	in	a	data	set,
reduce	the	dimensionnality	of	the	data	by	removing	the	noise	and	redundancy	in	the	data,
identify	correlated	variables"

#We	start	by	subsetting	active	individuals	and	active	variables	for	the	principal	component	analysis:
pca_group <-select(panel,-1,-2,-3,-4,-5,-7)
res.pca<-PCA(pca_group,	scale.unit	=	TRUE,	ncp	=	4,	graph	=	TRUE)
print(res.pca)
get_eigenvalue(res.pca)
fviz_eig(res.pca)
get_pca_ind(res.pca)
get_pca_var(res.pca)
fviz_pca_ind(res.pca)
fviz_pca_var(res.pca)
fviz_pca_biplot(res.pca)

library("factoextra")
eig.val	<-	get_eigenvalue(res.pca)
eig.val
fviz_eig(res.pca,	addlabels	=	TRUE,	ylim	=	c(0,	100))
var	<-	get_pca_var(res.pca)
var
fviz_pca_var(res.pca,	col.var	=	"black")
head(var$cos2) #quality	of	representation	of	the	variables	on	factor	map	is	called	cos2

library("corrplot")
corrplot(var$cos2,	is.corr=FALSE)
fviz_cos2(res.pca,	choice	=	"var",	axes	=	1:2)
#A	high	cos2	indicates	a	good	representation	of	the	variable	on	the	principal	component.	In	this
#case	the	variable	is	positioned	close	to	the	circumference	of	the	correlation	circle.
fviz_pca_var(res.pca,	col.var	=	"cos2",
             gradient.cols	=	c("#00AFBB",	"#E7B800",	"#FC4E07"),	
             repel	=	TRUE	#	Avoid	text	overlapping
)
fviz_pca_var(res.pca,	alpha.var	=	"cos2")
corrplot(var$contrib,	is.corr=FALSE)	
#	Contributions	of	variables	to	PC1
fviz_contrib(res.pca,	choice	=	"var",	axes	=	1,	top	=	10)
#	Contributions	of	variables	to	PC2
fviz_contrib(res.pca,	choice	=	"var",	axes	=	2,	top	=	10)

fviz_pca_var(res.pca,	col.var	=	"contrib",
             gradient.cols	=	c("#00AFBB",	"#E7B800",	"#FC4E07")
)
fviz_pca_var(res.pca,	alpha.var	=	"contrib")

ind	<-	get_pca_ind(res.pca)
ind
#	Coordinates	of	individuals
head(ind$coord)
#	Quality	of	individuals
head(ind$cos2)
#	Contributions	of	individuals
head(ind$contrib)
res.pca$quanti.sup
write.infile(res.pca,	"pca.csv",	sep	=	";")
write.infile(res.pca,	"pca.txt",	sep	=	"\t")


library(prcomp)
res.pcaprcomp	<-	prcomp(pca_group,	scale.	=	TRUE)
p<-res.pcaprcomp
p$x[,1] 
s <- summary(res.pcaprcomp)
s
View(p)
 dat <- cbind(pca_group,p[["x"]])
 View(dat)

 panel<-cbind(panel,p$x[,1]) # we extract pca1 to the our panel dataset
 


library(princomp)
res.pcaprincomp	<-	princomp(pca_group,	cor	=	TRUE)

