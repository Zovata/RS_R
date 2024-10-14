
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
getwd()



# 读入数据 --------------------------------------------------------------------


las1 <- readLAS("C:/Users/15850/GS/xjz/Z123las/20241011新济州北岛区域1.las")
las1 <- readLAS("/Users/a86188/20241011新济州北岛区域1.las")
print(las1)


las_check(las1)


## 初步绘图 --------------------------------------------------------------------

plot(las1)

plot(las1, color = "ScanAngleRank", bg = "white", axis = TRUE, legend = TRUE)



# Ground Classification ---------------------------------------------------


las1_classified <- classify_ground(las1, algorithm = pmf(ws = 5, th = 2))

# 检查分类结果
summary(las1_classified)

# 保存分类后的 LAS 文件
output_file <- "C:/Users/15850/GS/xjz/Z123las/las1_classified.las"
writeLAS(las1_classified, output_file)

# 可视化点云，地面点分类为2
plot(las1_classified, color = "Classification")


## 剖面图 ---------------------------------------------------------------------

p1 <- c(273457, 5274357)
p2 <- c(273542, 5274542)

p1 <- st_point(c(40361540, 40361551))  # 替换为你的实际坐标
p2 <- st_point(c(40361540, 40361551))  # 替换为你的实际坐标
las1_tr <- clip_transect(las1_classified, p1, p2, width = 5, xz = TRUE)


ggplot(payload(las_tr), aes(X,Z, color = Z)) + 
  geom_point(size = 0.5) + 
  coord_equal() + 
  theme_minimal() +
  scale_color_gradientn(colours = height.colors(50))


# Digital terrain model ---------------------------------------------------


## Triangular irregular network -------------------------------------------
# 这个算法会报错

# --- 生成 DTM ---
# 使用 Tin-based interpolation (TIN) 创建 DTM
dtm_tin <- grid_terrain(las1_classified_filtered, res = 1, algorithm = tin())

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
dtm_idw <- rasterize_terrain(las1_classified, algorithm = knnidw(k = 10L, p = 2))
plot_dtm3d(dtm_idw, bg = "white") 


## Kriging -----------------------------------------------------------------
# 太慢了跑不出来
dtm_kriging <- rasterize_terrain(las1_classified, algorithm = kriging(k = 40))
plot_dtm3d(dtm_kriging, bg = "white") 



# Height normalization ----------------------------------------------------

# 移除非地面点
gnd1 <- filter_ground(las1_classified)
plot(gnd1, size = 3, bg = "white", color = "Classification")


plot(dtm_idw, col = gray(1:50/50))


## 计算标准化点云 -----------------------------------------------------------------


nlas1 <- las1_classified - dtm_idw
plot(nlas1, size = 4, bg = "white")


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


## Triangulation -----------------------------------------------------------

chm_tin <- rasterize_canopy(nlas1, res = 0.5, algorithm = dsmtin())
plot(chm, col = col)



# Indivitual tree dectection and segmentation -----------------------------


plot(nlas1, bg = "white", size = 4)
