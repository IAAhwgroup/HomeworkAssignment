load('LeukError.RData')

# there are no duplicated nor NA variables and no na in data
# sum(duplicated(leuk))
# any(is.na(leuk))
# sum(is.na(leuk))

summary(leuk$V5001)

idx=which(apply(leuk[1:5000],2,function(x) all(x==20)))

leuk=leuk[-idx]

pca=prcomp(subset(leuk, select=-c(V5001)))

# first 3 PC explains 47% of the variance
sum(pca$sdev[1:3]^2)/sum(pca$sdev^2)

library(RColorBrewer)
# display.brewer.all()
palette(brewer.pal(n = 8, name = "Dark2"))

# plot(leuk[,sample(1:ncol(leuk)-1,2)],col = leuk$V5001)
plot(pca$x[,1],pca$x[,2],col = leuk$V5001, x.lab = "Principal Component 1", y.lab = "Principal Component 2")
# text(x=pca$x[,1], y=pca$x[,2], labels = 1:38)

colors = factor(palette())
colors = colors[leuk$V5001]

summary(colors)

library(rgl)
plot3d(x = pca$x[,1], y = pca$x[,2],z= pca$x[,3],col = colors, xlab = "Principal Component 1", ylab = "Principal Component 2", zlab = "Principal Component 3", type="s",alpha=0.3)
texts3d(x = pca$x[,1], y = pca$x[,2],z= pca$x[,3], text = 1:38)

# try scale but not a good idea


# Groups
groups <- iris$Species
levs <- levels(groups)
group.col <- c("red", "green", "blue")
# Plot observations
rgl_init()
rgl.spheres(x, y, z, r = 0.2,
            color = group.col[as.numeric(groups)]) 
rgl_add_axes(x, y, z, show.bbox = FALSE)
# Compute ellipse for each group
for (i in 1:length(levs)) {
  group <- levs[i]
  selected <- groups == group
  xx <- x[selected]; yy <- y[selected]; zz <- z[selected]
  ellips <- ellipse3d(cov(cbind(xx,yy,zz)), 
                      centre=c(mean(xx), mean(yy), mean(zz)), level = 0.95) 
  shade3d(ellips, col = group.col[i], alpha = 0.1, lit = FALSE) 
  # show group labels
  texts3d(mean(xx),mean(yy), mean(zz), text = group,
          col= group.col[i], cex = 2)
}


groups <- iris$Species
levs <- levels(groups)
group.col <- c("red", "green", "blue")
# Plot observations
rgl_init()
rgl.spheres(x, y, z, r = 0.2,
            color = group.col[as.numeric(groups)]) 
rgl_add_axes(x, y, z, show.bbox = FALSE)
# Compute ellipse for each group
for (i in 1:length(levs)) {
  group <- levs[i]
  selected <- groups == group
  xx <- x[selected]; yy <- y[selected]; zz <- z[selected]
  ellips <- ellipse3d(cov(cbind(xx,yy,zz)), 
                      centre=c(mean(xx), mean(yy), mean(zz)), level = 0.95) 
  shade3d(ellips, col = group.col[i], alpha = 0.1, lit = FALSE) 
  # show group labels
  texts3d(mean(xx),mean(yy), mean(zz), text = group,
          col= group.col[i], cex = 2)
}