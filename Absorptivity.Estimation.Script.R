#This script uses the output from a CID Bio-Science Miniature Leaf Spectrometer CI-710
#Due to the CID software, output file have to be named a certain way.
#The naming structure is such that a species' unique ID is followed by a period (.)
#which is followed by the number of the leaf replicate, and then another period (.)
#The unique ID's are codes that correspond to certain species (names not included here).

#Store all of your spectrometer data in one file
ufn	=	list.files("you path to sample/Absorptivity.data", pattern="*.csv", full.names=T)

rfl=trl=list()
for(i	in	1:length(ufn)){
   		df=read.csv(paste(ufn[i]),	fill=T,	header=F,	sep=",",	row.names	=	NULL)
   		#Extract	name	of	measurement,	split	name	into	parts,	then	recombine	name	so	that	each	name	has	4	parts.
   		split.names=strsplit(as.character(df[2,3]),".",	fixed=T)
   		name.df=data.frame(name=apply(data.frame(matrix(unlist(split.names),	ncol=4,	nrow=1,	byrow=T),	stringsAsFactors	=	F),2,function(x)gsub('\\s+',	'',x)))
   		
   		
   		#255:3383	are	the	rows of data	from	400nm	to	1000nm
   		waves=as.numeric(as.character(df[255:3383,1]))
   		
   		#extract	data,	remove	"%"	sign	and	divide	by	100	to	calculate	appropriate	spec.	data.	
      par.spec.data=as.numeric(gsub("%",	"",	df[255:3383,3]))/100
   		
      #create a data frame and rename the columns 
      d=data.frame( rep(name.df[1,1],	length(par.spec.data)), leaf.number=rep(name.df[2,1],	length(par.spec.data)),	waves, par.spec.data)
   		names(d)[c(1:4)]=	c("ID","leaf",	paste(df[5,1]),	paste(df[4,1]))
  
   		#sort	"d"	data.frames	into	lists	if	they	are	tranmittance	or	reflectance	data
   		if(as.character(colnames(d)[4])=="Transmittance"){trl[[i]]=d}
     	if(as.character(colnames(d)[4])=="Reflectance"){rfl[[i]]=d}
}
#Remove empty lists
trldf=do.call("rbind", trl[lengths(trl)!= 0])
rfdfl=do.call("rbind", rfl[lengths(rfl)!= 0])

#Merge the transmittance and reflectance datasets by the ID's the leaf number, and each wavelength
abs=merge(trldf,	rfdfl,	by=c("ID", "leaf",	"Wavelength"),	all=T)
str(abs)
 
#Calculate absorptivity for each waverlength
abs$Absorptivity=1-abs$Reflectance-abs$Transmittance

#Calculate the mean absorptivity, transmittance, and reflectance across all wavelenths
specdata=aggregate(abs[,4:6],	list("ID"=abs$ID, "leaf"=abs$leaf),	mean)

