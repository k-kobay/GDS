# import packages
library(rgl)

# set dataset directory
data_path <- "your pass here"

# import proccessed data `rain_uk.csv`
ouk <- read.csv(file=file.path(data_path, "rain_uk.csv"))

# import functions
## func_path <- "please set path of function folder path here"
func_path <- "~/Dropbox/03_さきがけ（酒井）/UK Precipitation/Sakai_R_programs/02_Functions"
source(file.path(func_path, "0_1_create.adj.matrix.R"))
source(file.path(func_path, "1_1_smoothing_calc.R"))
source(file.path(func_path, "1_2_PCA_each_day.R"))
source(file.path(func_path, "1_32_plot.smooth_rev2.R"))
source(file.path(func_path, "2_1_floyd.R"))
source(file.path(func_path, "2_2_dPathLengthAb.R"))
source(file.path(func_path, "2_3_path.calc.R"))
source(file.path(func_path, "2_4_UK.plot3d.R"))

# import sample data 
load('~/01_Dataset/sample_alpha_beta_length.Rdata')

mod.uk <- c()
# delete a day no exist
for(i in 1931:2015){
  X <- create.adj.matrix(year=i, distance=FALSE)[[1]]
  mod.uk <- rbind(mod.uk, X)
}

x  <- smoothing_calc(Data=mod.uk, colnum=4:6, type="spline")
x1 <- PCA_each_day(Data=mod.uk, colnum=4:6)

pca.uk <- data.frame(mod.uk[,1:3],prcomp(mod.uk[,-(1:3)])$x)
pca.x  <- smoothing_calc(Data=pca.uk, colnum=4:6, type="spline")
pca.x1 <- PCA_each_day(Data=mod.uk, colnum=4:6)


# calculation
X <- prcomp(mod.uk[,-c(1:3)])$x

# 年月日の列を追加
X <- cbind(X,mod.uk[,1:3])

# 例として1986年の部分行列を取り出す
tXp <- subset(X, X$Year==1986)[,1:3]

k <- 20
num <- seq(1,366,k)

aa <- plot.smooth(smoothing_calc=pca.x, PCA_each_day=pca.x1, tms.prm=0.1, filter.num=1,
                  num=num, pcol="day", leap=FALSE, plotting=FALSE)

mXp <- tXp
a <- aa[[1]]

for(i in 1:3){
  tmp <- mXp[,i] - min(mXp[,i])
  tmp <- (tmp/max(tmp))
  inter <- a[2,i] - a[1,i]
  tmp <- tmp*inter
  tmp <- tmp + a[1,i]
  mXp[,i] <- tmp
}


clear3d()
UK.plot3d(Data=year.1986, num=1, pcol="day", size=8, lcol="black", theta=50, phi=15, fov=60, Xp=mXp)

  # 複数を描画する

clear3d()

mat <- matrix(1:4, 2, 2)
layout3d(mat, height = c(3, 1), sharedMouse = TRUE)

aa <- plot.smooth(smoothing_calc=pca.x, PCA_each_day=pca.x1, tms.prm=0.1,
                  filter.num=1, num=num, pcol="day", leap=FALSE, plotting = FALSE)

PCA.filtered <- aa[[2]]

theta <- seq(-pi, pi, 0.02)

for(i in num){
  oo   <- PCA.filtered[[i]][,1]
  PC1  <- t(PCA.filtered[[i]][,c(2,3)])
  PC2  <- t(PCA.filtered[[i]][,c(4,5)])
  a    <- PC1[1,] - oo
  b    <- PC2[1,] - oo
  
  XYZ <- t(a%*%t(cos(theta)) + b%*%t(sin(theta)) + oo) 
  
  lines3d(XYZ, col=rainbow(366)[i], lwd=2)
}


next3d()
text3d(0,0,0, paste("smoothing roop"))

UK.plot3d(Data=year.1986, num=1, pcol="day", size=8, lcol="black", theta=50, phi=15, fov=60, Xp=mXp)

next3d()
text3d(0, 0, 0, paste("alpha beta distance"))