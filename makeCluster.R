require(mclust)
require(ggplot2)
# Read original dataframe
dataF = read.table("data_francois_raw.csv",header = F,sep=",")
dataL = read.table("pos_no_inter.csv",header=F,sep=",")
datapos = read.table("length_CDS.csv",sep=",",header = T)
# Transpose data from francois to get one gene per lines (instead of columns)
tdata= t(dataF)

# get colnames from first line, then remove first line
colnames(tdata) = tdata[1,]
tdata = tdata[-1,]

# remove useless columns
stdata = (tdata[,-c(10,11,12,13,14,15)])


# join the two dataframe (info on blast / gene status  + info on length for plotting with scale)
all_d = data.frame(stdata,dataL)

# Rename columns
colnames(all_d) = c("pseudo","#unique peptides","M1601","F38","ILRI181","Ckid","PG50","s_990145","s_95010","id","AAlength","start","stop")

# Reformat columns which were interpreted as factor:
all_d$pseudo  = as.numeric(levels(all_d$pseudo))[all_d$pseudo]
all_d$pseudo[all_d$pseudo==0] <- "no"
all_d$pseudo[all_d$pseudo==1] <- "yes"

all_d$"#unique peptides"  = as.numeric(levels(all_d$"#unique peptides"))[all_d$"#unique peptides"]
all_d$M1601  = as.numeric(levels(all_d$M1601))[all_d$M1601]
all_d$F38  = as.numeric(levels(all_d$F38))[all_d$F38]
all_d$ILRI181  = as.numeric(levels(all_d$ILRI181))[all_d$ILRI181]
all_d$Ckid  = as.numeric(levels(all_d$Ckid))[all_d$Ckid]
all_d$PG50  = as.numeric(levels(all_d$PG50))[all_d$PG50]
all_d$s_990145  = as.numeric(levels(all_d$s_990145))[all_d$s_990145]
all_d$s_95010  = as.numeric(levels(all_d$s_95010))[all_d$s_95010]


# Remove lines with NA for mixture model analysis

tmp = all_d[which(!is.na(all_d$Ckid)),]
clust = Mclust(tmp$Ckid)
clust$classification
tmp2 = data.frame(id = tmp$id,classification = clust$classification)
head(tmp2)
merged_dat = merge(tmp2,all_d,by.x = "id", by.y = "id",all.x=TRUE)
head(merged_dat)
dim(merged_dat)
#ggplot(merged_dat) + geom_segment(aes(x = start,xend=stop,y=Ckid,yend=Ckid,color=as.factor(classification)),size= 2) 

merged_dat = merge(merged_dat,datapos,by.x = "id", by.y = "Label")

# Write resulting dataframe for clustering analysis: 
write.table(file = "Merged_data.csv",merged_dat,sep=",")

# Merged_data is used in a pyton program that output intervals of significant cluster in cluster_abs_pos.txt.
# We know use those interval to plot significant cluster on the figure

clust_pos = read.table("cluster_abs_pos.txt",header=F)

# visual check of concerned lines: 
# Interval are designed with +1 for begin due to counting convention difference between R and Python (pffff)
merged_dat[which(rownames(merged_dat) %in% c(clust_pos$V1+1)),]
merged_dat[which(rownames(merged_dat) %in% clust_pos$V2)),]

# here we get the actual genomic coordinates of clusters:

start_clust = merged_dat[which(rownames(merged_dat) %in% c(clust_pos$V1+1)),]$Begin
end_clust = merged_dat[which(rownames(merged_dat) %in% clust_pos$V2),]$End
clust_coord = data.frame(begin = start_clust,end = end_clust)

# Now we're good to make a plot
pdf(height = 10, width = 35)

# First an histogram of the mixture model:

ggplot(merged_dat) + geom_histogram(aes(x =Ckid,fill=as.factor(classification)),position = "dodge",binwidth = 1) + theme_minimal() + ylab("Number") + xlab("identity percentage with Ckid homolog") + scale_fill_brewer(name="Classification",type ="Seq",palette ="Set1")
# Then a plot along genomic coordinates:

ggplot(merged_dat) + geom_line(aes(x = Begin,y=Ckid),color="black",size=0.5) + geom_point(aes(x=Begin,y=Ckid,color=as.factor(classification)))  + theme_minimal() + xlab("Genomic position") + ylab("identity percentage with Ckid homolog") + scale_color_brewer(name="Classification",type ="Seq",palette ="Set1") + geom_rect(data = clust_coord, aes(xmin = begin, xmax = end, ymin = 20,ymax = 100), color = "black", alpha = 0.1)

dev.off()

# Now some work to extract all genes belonging to the differents clusters from the dataframe
data_clust = data.frame()
for(tt in 1:nrow(clust_pos)){
	print(tt)
	a = clust_pos[tt,1]+1
	b = clust_pos[tt,2]
	print(a:b)	
	print(merged_dat[which(rownames(merged_dat) %in% a:b),])
	data_clust <- rbind(data_clust,merged_dat[which(rownames(merged_dat) %in% a:b),])	
	data_clust$
}

dim(data_clust)

