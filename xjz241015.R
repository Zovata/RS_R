
# 配置 ----------------------------------------------------------------------
# 分节符快捷键 Ctrl+Shift+R
# 注释快捷键 Ctrl + Shift + C
# install.packages("lidR")
install.packages("ggplot2")
install.packages("sf")
install.packages("gstat")

library(lidR)
library(ggplot2)
library(gstat)
library(sf)
library(rgl)  # 用于3D图形
getwd()



# 读入数据 --------------------------------------------------------------------


las1 <- readLAS("C:/Users/15850/GS/xjz/Z123las/20241011新济州北岛区域1.las")
las1 <- readLAS("/Users/a86188/20241011新济州北岛区域1.las")
print(las1)


las_check(las1)

# 查看点云数据中的分类信息
classification_counts <- table(las1@data$Classification)
print(classification_counts)
## 初步绘图 --------------------------------------------------------------------

plot(las1)

plot(las1, color = "ScanAngleRank", bg = "white", axis = TRUE, legend = TRUE)


# 去噪 ----------------------------------------------------------------------


# 使用 Statistical Outlier Removal (SOR) 去除噪声
# 该算法基于统计学方法去除那些与周围点分布异常的点
las1 <- lasfilterdecimate(las1, s = 1)  # 删除低密度点
las_denoised <- lasnoise(las_denoised, k = 10, res = 0.5)


# Ground Classification ---------------------------------------------------

# 检查可用的核心数量
available_cores <- parallel::detectCores()
cat("可用核心数：", available_cores, "\n")

# 设置多线程并行化的核心数，假设使用所有可用核心
set_lidr_threads(available_cores)

# 检查当前设置的线程数
current_threads <- get_lidr_threads()
cat("当前设置的线程数：", current_threads, "\n")

las1 <- classify_ground(las1, algorithm = pmf(ws = 5, th = 2))

# las1_classified <- classify_ground(las1, algorithm = pmf(ws = 5, th = 2))  #las1已经分类过了

# 检查分类结果
summary(las1_classified)

# 保存分类后的 LAS 文件
output_file <- "C:/Users/15850/GS/xjz/Z123las/las1.las"
writeLAS(las1, output_file)

# 可视化点云，地面点分类为2
plot(las1_classified, color = "Classification")


## 剖面图 ---------------------------------------------------------------------

p1 <- c(40361540, 40361551)
p2 <- c(40361540, 40361551)

p1 <- st_point(c(40361540, 40361551))  # 替换为你的实际坐标
p2 <- st_point(c(40361540, 40361551))  # 替换为你的实际坐标
las1_tr <- clip_transect(las1, p1, p2, width = 5, xz = TRUE)


ggplot(payload(las1_tr), aes(X,Z, color = Z)) + 
  geom_point(size = 0.5) + 
  coord_equal() + 
  theme_minimal() +
  scale_color_gradientn(colours = height.colors(50))


# Digital terrain model ---------------------------------------------------


## Triangular irregular network -------------------------------------------
# 这个算法会报错

# --- 生成 DTM ---
# 使用 Tin-based interpolation (TIN) 创建 DTM
dtm_tin <- grid_terrain(las1, res = 1, algorithm = tin())

# 检查缩放因子
header <- las1_classified@header
print(header@VLR[[1]])  # 打印出缩放因子信息

# 假设缩放因子不正确，修正缩放因子
header@VLR[[1]]$X_scale_factor <- 0.01  # 根据数据实际情况调整
header@VLR[[1]]$Y_scale_factor <- 0.01
header@VLR[[1]]$Z_scale_factor <- 0.01

# 应用修正后的缩放因子
las@header <- header



# --- 提取点云数据为数据框 ---
# 将 LAS 对象转换为数据框
las1_points <- as.data.frame(las1_classified@data)

# --- 移除重复的 X, Y 坐标点 ---
# 通过检查 duplicated(cbind(X, Y)) 过滤重复点
las1_points_filtered <- las1_points[!duplicated(cbind(las1_points$X, las1_points$Y)), ]

# --- 重新构建 LAS 对象 ---
# 使用过滤后的点重建 LAS 对象
las1_classified_filtered <- LAS(las1_points_filtered, header = las1_classified@header)

# --- 绘制 DTM ---
# 将生成的 DTM 转换为栅格图层并绘制
plot(dtm, main = "Digital Terrain Model (DTM)", col = terrain.colors(50))


# --- 修正 LAS 文件的缩放因子 (scale factors) ---
# 确保缩放因子 (scale factors) 正确
header <- las@header
header@VLR[[1]]$X_scale_factor <- 0.01  # 这个值可能需要根据数据调整
header@VLR[[1]]$Y_scale_factor <- 0.01  # 这个值可能需要根据数据调整
header@VLR[[1]]$Z_scale_factor <- 0.01  # 这个值可能需要根据数据调整

# 重新应用修正的缩放因子到点云数据
las@header <- header

# --- 移除退化点 ---
# 使用 lasfilter_duplicates 函数去除具有相同 X, Y 坐标的点
las1_classified <- lasfilter_duplicates(las1_classified)

# --- 生成 DTM ---
# 使用 TIN 插值算法生成数字地形模型
dtm <- grid_terrain(las_filtered, res = 1, algorithm = tin())

# --- 可视化 DTM ---
plot(dtm, main = "Digital Terrain Model (DTM)", col = terrain.colors(50))


## Invert distance weighting -----------------------------------------------
# 这个算法勉强可以
dtm_idw <- rasterize_terrain(las1, algorithm = knnidw(k = 10L, p = 2))
plot_dtm3d(dtm_idw, bg = "black") 


## Kriging -----------------------------------------------------------------
# 太慢了跑不出来
dtm_kriging <- rasterize_terrain(las1, algorithm = kriging(k = 40))
plot_dtm3d(dtm_kriging, bg = "white") 



# Height normalization ----------------------------------------------------

# 移除非地面点
gnd1 <- filter_ground(las1)
plot(gnd1, size = 3, bg = "white", color = "Classification")


plot(dtm_idw, col = gray(1:50/50))


## 计算标准化点云 -----------------------------------------------------------------


nlas1 <- las1 - dtm_idw
plot(nlas1, size = 4, bg = "white")

output_file <- "C:/Users/15850/GS/xjz/Z123las/nlas1.las"
writeLAS(nlas1, output_file)

# 检查地面点的 Z 值范围
range(filter_ground(nlas1)$Z)

# 获取 Z 值的范围
z_range <- range(filter_ground(nlas1)$Z)

# 使用 Z 值的范围自动调整 breaks
hist(filter_ground(nlas1)$Z, 
     breaks = seq(floor(z_range[1]), ceiling(z_range[2]), 0.01), 
     main = "", 
     xlab = "Elevation")

# hist(filter_ground(nlas1)$Z, breaks = seq(-0.6, 0.6, 0.01), main = "", xlab = "Elevation")


# Digital Surface Model and Canopy Height model ---------------------------

## Point-to-raster ---------------------------------------------------------

chm <- rasterize_canopy(nlas1, res = 1, algorithm = p2r())
col <- height.colors(25)
plot(chm, col = col)

# 设置黑色背景，白色字体
par(bg = "black", fg = "white", col.axis = "white", col.lab = "white", col.main = "white")

# 绘制冠层高度模型 (CHM) 并将背景设置为黑色
plot(chm, main = "Canopy Height Model", col = terrain.colors(50), axes = TRUE)


# 设置输出文件的路径和名称
output_file <- "C:/Users/15850/GS/xjz/Z123las/1_chm.png"

# 使用 png() 函数，指定 300 DPI 的分辨率
png(filename = output_file, width = 8, height = 6, units = "in", res = 300)

# 保存原来的图形参数
op <- par(no.readonly = TRUE)

# 设置黑色背景，白色字体
par(bg = "black", fg = "white", col.axis = "white", col.lab = "white", col.main = "white")

# 绘制冠层高度模型 (CHM)
plot(chm, main = "Canopy Height Model", col = terrain.colors(50), axes = TRUE)

# 关闭图形设备，保存 PNG 文件
dev.off()


## 多层图像 --------------------------------------------------------------------

# The first layer is a regular triangulation
layer0 <- rasterize_canopy(nlas1, res = 0.5, algorithm = p2r())

# Triangulation of first return above 10 m
above10 <- filter_poi(nlas1, Z >= 10)
layer10 <- rasterize_canopy(above10, res = 0.5, algorithm = p2r())

# Triangulation of first return above 20 m
above20 <- filter_poi(above10, Z >= 20)
layer20 <- rasterize_canopy(above20, res = 0.5, algorithm = p2r())

# The final surface is a stack of the partial rasters
dsm <- layer0
dsm[] <- pmax(as.numeric(layer0[]), as.numeric(layer10[]), as.numeric(layer20[]), na.rm = T)

layers <- c(layer0, layer10, layer20, chm)
names(layers) <- c("Base", "Layer10m", "Layer20m", "pitfree")
plot(layers, col = col)

plot(layer10, col = col)
## Triangulation -----------------------------------------------------------

chm_tin <- rasterize_canopy(nlas1, res = 0.5, algorithm = dsmtin())
plot(chm, col = col)



# Indivitual tree dectection and segmentation -----------------------------


plot(nlas1, bg = "black", size = 4)

# 手动设置窗口尺寸 (宽度和高度)
rgl.set(viewport = c(0, 0, 800, 800))  # 设置宽度和高度为 800x800 像素


# 保存当前 3D 图像为 PNG 文件
rgl.snapshot("nlas1.png", fmt = "png")



# 保存当前 3D 图像为 PNG 文件
rgl.snapshot("nlas1_high_res.png", fmt = "png")


nlas1_sg <- segment_trees(nlas1, li2012(dt1 = 1.4))
nlas1_sg <- readLAS("C:/Users/15850/GS/xjz/Z123las/nlas1_sg.las")

col <- pastel.colors(200)
plot(nlas1_sg, color = "treeID", colorPalette = col)
# 保存当前3D图像为PNG文件
rgl.snapshot("3d_tree_segmentation.png", fmt = "png")


output_file <- "C:/Users/15850/GS/xjz/Z123las/nlas1_sg.las"
writeLAS(nlas1_sg, output_file)
