# Data from
# http://static.geneatlas.roslin.ed.ac.uk/gwas/allWhites/imputed/data.copy/imputed.allWhites.selfReported_n_1245.chr6.csv.gz
# http://static.geneatlas.roslin.ed.ac.uk/gwas/allWhites/snps/extended/snps.imputed.chr6.csv.gz
library(data.table)
x<-fread("~/Downloads/imputed.allWhites.selfReported_n_1245.chr6.csv",sep=" ")
p <- -log10(x$`PV-selfReported_n_1245`)
snp<-fread("~/Downloads/snps.imputed.chr6.csv",sep=" ")
snp<-snp[match(x$SNP,snp$SNP),]
# Chromosome
plot(snp$Position[p>1],p[p>1],pch=20,cex=.2)
rug(snp$Position[p>1],side=3,ticksize = -0.03)
abline(v=c(30e6,34e6))
# Zoomed
library(rtomahawk)
data(gmap)
plot(gmap$chr6[gmap$chr6$position>30e6&gmap$chr6$position<34e6,c(1,2)],type="l",col="blue",ylim=c(0,100),xaxs="i",yaxs="i",las=2)
points(snp$Position[snp$Position>30e6&snp$Position<34e6],p[snp$Position>30e6&snp$Position<34e6]/max(p[snp$Position>30e6&snp$Position<34e6])*100,pch=20,cex=.5)
rug(snp$Position[snp$Position>30e6&snp$Position<34e6],side=3,ticksize = -0.03)

# LD was calculated with Tomahawk for chr6:30e6-34e6
twk<-rtomahawk::LoadHeader("/media/mdrk/NVMe/1kgp3/1kgp3_chr6_30_34_p32626301.two")
ld<-head(twk,1000000)
ld$posA<-ld$posA+1
ld$posB<-ld$posB+1
pos<-snp$Position[snp$Position>32e6&snp$Position<33e6]
posLD<-pos[pos%in%ld$posB]
pvals<-p[snp$Position>32e6&snp$Position<33e6]
rownames(ld)<-ld$posB
ld<-ld[as.character(posLD),]
lzcolors<-c("darkblue","blue","green","orange","red")

plot(gmap$chr6[gmap$chr6$position>32e6&gmap$chr6$position<33e6,c(1,2)],type="l",col="blue",ylim=c(0,100),xaxs="i",yaxs="i",las=2)
points(pos[which(!pos%in%posLD)],pvals[which(!pos%in%posLD)]/round(max(pvals)+5,-1)*100,pch=20,cex=.66,col="black")
points(pos[which(pos%in%posLD)],pvals[which(pos%in%posLD)]/round(max(pvals)+5,-1)*100,pch=21,cex=1,bg=lzcolors[as.numeric(cut(ld$R2,breaks = seq(0,1,length.out = 6),right = T))])
rug(pos,side=3,ticksize = -0.03)
