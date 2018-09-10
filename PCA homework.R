load('LeukError.RData')

summary(leuk$V5001)

pca=prcomp(leuk[1:5000])

idx=which(apply(leuk[1:5000],2,max)==20)

leuk1=subset(leuk,,-idx)

pca1=prcomp(leuk[1:ncol(leuk1)])

plot(pca$x[,1],pca$x[,2])

plot(pca1$x[,1],pca1$x[,2])

library(RColorBrewer)
display.brewer.all()
palette(brewer.pal(n = 8, name = "Dark2"))

plot(leuk1[,sample(1:ncol(leuk1)-1,2)],col = leuk1$V5001)
plot(pca1$x[,1],pca1$x[,2],col = leuk1$V5001, x.lab = "Principal Component 1", y.lab = "Principal Component 2")

colors = factor(palette())
colors = colors[leuk1$V5001]

library(rgl)
plot3d(x = pca1$x[,1], y = pca1$x[,2],z= pca1$x[,3],col = colors, xlab = "Principal Component 1", ylab = "Principal Component 2", zlab = "Principal Component 3", type="s",alpha=0.3)
texts3d(x = pca1$x[,1], y = pca1$x[,2],z= pca1$x[,3], text = 1:38)