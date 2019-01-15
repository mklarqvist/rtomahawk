plotLDRegionTriangular<-function(dataSource, from, to, ...){
    colors<-paste0(colorRampPalette(c("blue","red"))(10),seq(0,100,length.out = 11))
    colors[1]<-paste0(colors[1],"0")
    colors[length(colors)]<- substr(colors[length(colors)],1,7)
    
    # Assumes all the data is from the same chromosome
    b<-dataSource[dataSource$posA>=from & dataSource$posA<=to & dataSource$posB>=from & dataSource$posB<=to,]
    b<-b[b$posA<b$posB,] # upper triangular only
    b<-b[order(b$R2,decreasing = F),] # sort for Z-stack
    plot(b$posA + ((b$posB-b$posA)/2), b$posB-b$posA,pch=20,cex=.2,col=colors[cut(b$R2,breaks=seq(0,1,length.out = 11),include.lowest = T)],xaxs="i",yaxs="i", ...)
}

# Data from
# http://static.geneatlas.roslin.ed.ac.uk/gwas/allWhites/imputed/data.copy/imputed.allWhites.selfReported_n_1245.chr6.csv.gz
# http://static.geneatlas.roslin.ed.ac.uk/gwas/allWhites/snps/extended/snps.imputed.chr6.csv.gz
library(data.table)
x<-fread("zcat ~/Downloads/imputed.allWhites.selfReported_n_1245.chr6.csv.gz",sep=" ")
snp<-fread("zcat ~/Downloads/snps.imputed.chr6.csv.gz",sep=" ")
snp<-snp[match(x$SNP,snp$SNP),]
snp$p <- -log10(x$`PV-selfReported_n_1245`)

# Chromosome
plot(snp$Position[snp$p>1],snp$p[snp$p>1],pch=20,cex=.2)
rug(snp$Position[snp$p>1],side=3,ticksize = -0.03)
abline(v=c(18e6,22e6))
# Zoomed
library(rtomahawk)
data(gmap,package = "rtomahawk")
par(mar=c(5,5,2,5))
plot(gmap$chr6[gmap$chr6$position>30e6&gmap$chr6$position<34e6,c(1,2)],type="l",col="blue",ylim=c(0,100),xaxs="i",yaxs="i",axes=F, xlab=NA, ylab=NA)
axis(side=4,las=2)
mtext(side = 4, line = 3, "Recombinatin rate (cm/Mb)")
par(new = T)
plot(snp$Position[snp$Position>18e6&snp$Position<22e6],snp$p[snp$Position>18e6&snp$Position<22e6],pch=20,cex=.5,xaxs="i",yaxs="i",las=2,ylab="-log10(P)",xlab="Position")
rug(snp$Position[snp$Position>18e6&snp$Position<22e6],side=3,ticksize = -0.03)

#"6:20682622"
plotLZ<-function(twk, tgt_snp, window, snp, gmap, threads=4){
    ld<-rtomahawk::twk_scalc(twk,tgt_snp,window, threads=threads,window=window)
    ld$posA<-ld$posA+1
    ld$posB<-ld$posB+1
    
    if(!tgt_snp%in%snp$Position){
        stop("cannot find target snp in set")
    }
    
    #tgt_snp<-20686878
    from<-tgt_snp - window
    to<-tgt_snp + window
    pos<-snp$Position[snp$Position>from&snp$Position<to]
    posLD<-pos[pos%in%union(ld$posA,ld$posB)]
    pvals<-snp$p[snp$Position>from&snp$Position<to]
    pvalsLD<-pvals[which(pos%in%posLD)]
    ld<-ld[ld$posB==tgt_snp,]
    rownames(ld)<-ld$posA
    ld<-ld[as.character(posLD),]
    posLD<-posLD[order(ld$R2,decreasing = F)]
    pvalsLD<-pvalsLD[order(ld$R2,decreasing = F)]
    ld<-ld[order(ld$R2,decreasing = F),]
    
    lzcolors<-rev(c("#D43F3AFF", "#EEA236FF", "#5CB85CFF", "#46B8DAFF", "#357EBDFF"))
    
    par(mar=c(5,5,2,5))
    plot(gmap$chr6[gmap$chr6$position>from&gmap$chr6$position<to,c(1,2)],type="l",col="blue",ylim=c(0,100),xaxs="i",yaxs="i",axes=F, xlab=NA, ylab=NA)
    axis(side=4,las=2,col="blue",col.axis="blue")
    mtext(side = 4, line = 3, "Recombination rate (cm/Mb)",col = "blue")
    par(new = T)
    plot(pos[which(!pos%in%posLD)],pvals[which(!pos%in%posLD)],pch=20,cex=.9,col="#B8B8B8FF",xaxs="i",yaxs="i",las=2,ylab="-log10(P)",xlab="Position",ylim=c(0, round(max(pvals)+5,-1)))
    points(posLD,pvalsLD,pch=21,cex=1,bg=lzcolors[as.numeric(cut(ld$R2,breaks = seq(0,1,length.out = 6),right = T))])
    points(tgt_snp, snp[snp$Position==tgt_snp,p], pch=24, bg="#9632B8FF",cex=1.2)
    suppressWarnings(rug(pos,side=3,ticksize = -0.03))
    legend("topright",fill = c(rev(lzcolors),"#B8B8B8FF"), legend = c("0.8-1.0","0.6-0.8","0.4-0.6","0.2-0.4","0.0-0.2","NA"),y.intersp = 0.5,cex = 1.3, title=expression("LD "  ~ R^2))
    text(x=from+((to-from)/2),y=round(max(pvals)+5,-1)-3,labels = tgt_snp)
}

# LD was calculated with Tomahawk for chr6:30e6-34e6
# Slice out region with twk view -i <file.two> -I 6,6:32626301
twk<-rtomahawk::LoadHeader("~/Downloads/tests.two")
ld<-head(twk,1000000)
#ld$posA<-ld$posA+1
#ld$posB<-ld$posB+1
# temp
#ld<-ld[!duplicated(paste0(ld$posA,ld$posB)),]

tgt_snp<-20682622
#tgt_snp<-20686878
from<-tgt_snp-1000e3
to<-tgt_snp+1000e3
pos<-snp$Position[snp$Position>from&snp$Position<to]
posLD<-pos[pos%in%union(ld$posA,ld$posB)]
pvals<-snp$p[snp$Position>from&snp$Position<to]
pvalsLD<-pvals[which(pos%in%posLD)]
ld<-ld[ld$posB==tgt_snp,]
rownames(ld)<-ld$posA
ld<-ld[as.character(posLD),]
posLD<-posLD[order(ld$R2,decreasing = F)]
pvalsLD<-pvalsLD[order(ld$R2,decreasing = F)]
ld<-ld[order(ld$R2,decreasing = F),]

lzcolors<-rev(c("#D43F3AFF", "#EEA236FF", "#5CB85CFF", "#46B8DAFF", "#357EBDFF"))

par(mar=c(5,5,2,5))
plot(gmap$chr6[gmap$chr6$position>from&gmap$chr6$position<to,c(1,2)],type="l",col="blue",ylim=c(0,100),xaxs="i",yaxs="i",axes=F, xlab=NA, ylab=NA)
axis(side=4,las=2,col="blue",col.axis="blue")
mtext(side = 4, line = 3, "Recombination rate (cm/Mb)",col = "blue")
par(new = T)
plot(pos[which(!pos%in%posLD)],pvals[which(!pos%in%posLD)],pch=20,cex=.9,col="#B8B8B8FF",xaxs="i",yaxs="i",las=2,ylab="-log10(P)",xlab="Position",ylim=c(0, round(max(pvals)+5,-1)))
points(posLD,pvalsLD,pch=21,cex=1,bg=lzcolors[as.numeric(cut(ld$R2,breaks = seq(0,1,length.out = 6),right = T))])
points(tgt_snp, snp[snp$Position==tgt_snp,p], pch=24, bg="#9632B8FF",cex=1.2)
rug(pos,side=3,ticksize = -0.03)
legend("topright",fill = c(rev(lzcolors),"#B8B8B8FF"), legend = c("0.8-1.0","0.6-0.8","0.4-0.6","0.2-0.4","0.0-0.2","NA"),y.intersp = 0.5,cex = 1.3, title=expression("LD "  ~ R^2))
text(x=from+((to-from)/2),y=round(max(pvals)+5,-1)-3,labels = tgt_snp)

#
plotLDRegionTriangular(ld2[ld2$R2>0.2,],min(ld2$posA),max(ld2$posB),ylim=c(500e3,0),xlim=c(from,to),las=2)
##

library(rehh)
p<-fread("~/Downloads/test_hap.txt",h=T)
res <- new("haplohh")
res@haplo<-as.matrix(p[,-1])+1
res@position<-as.numeric(colnames(p)[-1])
res@chr.name<-"6"
res@nhap<-nrow(res@haplo)
res@snp.name<-paste0("snp",1:ncol(p))
res@nsnp<-ncol(p)
#bifurcation.diagram(res,mrk_foc=which(res@position==20682622),all_foc=1,nmrk_l=9,nmrk_r=9,refsize = 0.1)
bifurcation.diagram(res,mrk_foc=which(res@position==32626302),all_foc=2,nmrk_l=15,nmrk_r=15,refsize = 0.1)
