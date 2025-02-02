
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
library(terra)
getwd()



# 读入数据 --------------------------------------------------------------------


las123 <- readLAS("C:/Users/15850/GS/xjz/Z123las/xjz1011_merged.las")
print(las123)

las1 <- readLAS("/Users/a86188/20241011新济州北岛区域1.las")
print(las1)


las_check(las1)

# 查看点云数据中的分类信息
classification_counts <- table(las123@data$Classification)
print(classification_counts)
## 初步绘图 --------------------------------------------------------------------

plot(las1)

plot(las1, color = "ScanAngleRank", bg = "white", axis = TRUE, legend = TRUE)


# 去噪 ----------------------------------------------------------------------


# 例如，去除 Z 值异常的点（假设 Z 在 0 到 50 之间是合理的范围）
las_denoised <- filter_poi(las1, Z > 0 & Z < 50)

# 可视化去噪前后的对比
plot(las, color = "Classification", main = "原始点云数据")
plot(las_denoised, color = "Classification", main = "去噪后的点云数据")


# Ground Classification ---------------------------------------------------

# 检查可用的核心数量
available_cores <- parallel::detectCores()
cat("可用核心数：", available_cores, "\n")

# 设置多线程并行化的核心数，假设使用所有可用核心
set_lidr_threads(available_cores)

# 检查当前设置的线程数
current_threads <- get_lidr_threads()
cat("当前设置的线程数：", current_threads, "\n")

las123 <- classify_ground(las123, algorithm = pmf(ws = 5, th = 2))

las1 <- readLAS("/Users/a86188/las1_classified.las")

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

las123 <- readLAS("C:/Users/15850/GS/xjz/Z123las/xjz1011_merged_重采样_去噪.las")

## Triangular irregular network -------------------------------------------
# 这个算法会报错

# --- 生成 DTM ---
# 使用 Tin-based interpolation (TIN) 创建 DTM
dtm_tin <- grid_terrain(las123, res = 1, algorithm = tin())

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
dtm_idw <- rasterize_terrain(las123, algorithm = knnidw(k = 10L, p = 2))
plot_dtm3d(dtm_idw, bg = "black") 



## Kriging -----------------------------------------------------------------
# 太慢了跑不出来
dtm_kriging <- rasterize_terrain(las1, algorithm = kriging(k = 40))
plot_dtm3d(dtm_kriging, bg = "white") 



# Height normalization ----------------------------------------------------

# 移除非地面点
gnd1 <- filter_ground(las1)
plot(gnd1, size = 3, bg = "white", color = "Classification")




## 绘制DTM -------------------------------------------------------------------

library(terra)
dtm <- rasterize_terrain(las, algorithm = tin())
dtm_prod <- terrain(dtm_idw, v = c("slope", "aspect"), unit = "radians")
dtm_hillshade <- shade(slope = dtm_prod$slope, aspect = dtm_prod$aspect)
plot(dtm_hillshade, col =gray(0:30/30), legend = FALSE)


output_file <- "C:/Users/15850/GS/xjz/Z123las/pic/123_DTM_shade.png"
# 使用 png() 函数，指定 300 DPI 的分辨率
png(filename = output_file, width = 8, height = 6, units = "in", res = 300)

# 设置黑色背景，白色字体
par(bg = "black", fg = "white", col.axis = "white", col.lab = "white", col.main = "white")

plot(dtm_idw, col = gray(1:50/50))
# 关闭图形设备，保存 PNG 文件
dev.off()
## 计算归一化点云 -----------------------------------------------------------------


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


##Point cloud normalization -----------------------------------------------

nlas <- normalize_height(las1, knnidw())

nlas123 <- readLAS("C:/Users/15850/GS/xjz/Z123las/xjz1011_merged_重采样_去噪_根据地面点归一化.las")

# Digital Surface Model and Canopy Height model ---------------------------

## Point-to-raster ---------------------------------------------------------

chm <- rasterize_canopy(nlas123, res = 1, algorithm = p2r())
col <- height.colors(25)
plot(chm, col = col)
class(chm)
# 导出为 .tif 文件
writeRaster(chm, "chm.tif", filetype="GTiff", overwrite=TRUE)

# 设置黑色背景，白色字体
par(bg = "black", fg = "white", col.axis = "white", col.lab = "white", col.main = "white")

# 绘制冠层高度模型 (CHM) 并将背景设置为黑色
plot(chm, main = "Canopy Height Model", col = terrain.colors(50), axes = TRUE)


# 设置输出文件的路径和名称
output_file <- "C:/Users/15850/GS/xjz/Z123las/pic/123_chm.png"

# 使用 png() 函数，指定 300 DPI 的分辨率
png(filename = output_file, width = 8, height = 6, units = "in", res = 300)

# 保存原来的图形参数
op <- par(no.readonly = TRUE)

# 设置黑色背景，白色字体
par(bg = "black", fg = "white", col.axis = "white", col.lab = "white", col.main = "white")

# 绘制冠层高度模型 (CHM)
plot(chm,  col = terrain.colors(50), axes = TRUE)

# 关闭图形设备，保存 PNG 文件
dev.off()


## 多层图像 --------------------------------------------------------------------

# The first layer is a regular triangulation
layer0 <- rasterize_canopy(nlas123, res = 0.5, algorithm = p2r())


# Triangulation of first return above 10 m
above5 <- filter_poi(nlas123, Z >= 5)
layer5 <- rasterize_canopy(above5, res = 0.5, algorithm = p2r())

# Triangulation of first return above 10 m
above10 <- filter_poi(nlas123, Z >= 10)
layer10 <- rasterize_canopy(above10, res = 0.5, algorithm = p2r())

# Triangulation of first return above 20 m
above20 <- filter_poi(nlas123, Z >= 20)
layer20 <- rasterize_canopy(above20, res = 0.5, algorithm = p2r())

# The final surface is a stack of the partial rasters
dsm <- layer0
dsm[] <- pmax(as.numeric(layer0[]), as.numeric(layer10[]), as.numeric(layer20[]), na.rm = T)

layers <- c(layer0, layer10, layer20, chm)
names(layers) <- c("Base", "Layer10m", "Layer20m", "pitfree")
plot(layers, col = col)

# 假设你已经定义了一个调色板
colors_palette <- terrain.colors(50)  # 使用 terrain.colors() 创建颜色调色板

# 绘制图层
plot(layer0, col = colors_palette)

plot(layer5, col = colors_palette)
plot(layer10, col = colors_palette)
plot(layer20, col = colors_palette)

output_file <- "C:/Users/15850/GS/xjz/Z123las/pic/123_CHM5.png"
# 使用 png() 函数，指定 300 DPI 的分辨率
png(filename = output_file, width = 8, height = 6, units = "in", res = 300)
# 设置黑色背景，白色字体
par(bg = "black", fg = "white", col.axis = "white", col.lab = "white", col.main = "white")
plot(layer5, col = colors_palette)
# 关闭图形设备，保存 PNG 文件
dev.off()




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
nlas1 <- readLAS("/Users/a86188/nlas1.las")

nlas1_sg <- segment_trees(nlas1, li2012(dt1 = 1.4))
nlas1_sg <- readLAS("C:/Users/15850/GS/xjz/Z123las/nlas1_sg.las")

col <- pastel.colors(200)
plot(nlas1_sg, color = "treeID", colorPalette = col)
# 保存当前3D图像为PNG文件
rgl.snapshot("3d_tree_segmentation.png", fmt = "png")


output_file <- "/Users/a86188/nlas1_sg.las"
output_file <- "C:/Users/15850/GS/xjz/Z123las/nlas1_sg.las"
writeLAS(nlas1_sg, output_file)


## 绘制单株树 -------------------------------------------------------------------

tree110 <- filter_poi(nlas123, treeID == 110)
plot(tree110, size = 8, bg = "black")

tree70 <- filter_poi(nlas1_sg, treeID == 70)
plot(tree70, size = 8, bg = "black")


crowns <- crown_metrics(nlas1_sg, func = .stdtreemetrics, geom = "convex")
plot(crowns["convhull_area"], main = "Crown area (convex hull)")



# 栅格计算 --------------------------------------------------------------------

# 计算10*10的平均高度
hmean <- pixel_metrics(nlas123, ~mean(Z), 10) # calculate mean at 10 m
p1 <- plot(hmean, col = height.colors(50))

metrics <- pixel_metrics(nlas1, .stdmetrics, 10) # calculate standard metrics
plot(metrics, col = height.colors(50))

# Density
density <- pixel_metrics(nlas1, ~length(Z)/16, 4) # calculate density
plot(density, col = gray.colors(50,0,1)) # some plotting

# Intensity
imap <- pixel_metrics(nlas1, ~mean(Intensity), 4, filter = ~ReturnNumber == 1L) # mapping average intensity
plot(imap, col = heat.colors(25))

install.packages("raster")
library(raster)



## 冠层覆盖度 -------------------------------------------------------------------


# 设置栅格的分辨率（单位是米）
resolution <- 10

# 使用 grid_canopy 生成冠层高度模型 (CHM)
# p2r() 是点到栅格的算法，将最高点赋予栅格
chm <- grid_canopy(nlas123, res = resolution, algorithm = p2r())

# 冠层覆盖度的阈值，假设高度大于 2 米的点代表冠层
canopy_threshold <- 2

# 计算冠层覆盖度
canopy_cover <- chm > canopy_threshold

# 将 TRUE/FALSE 转换为 0 和 1（1 代表有冠层覆盖，0 代表没有）
canopy_cover_raster <- canopy_cover * 1

# 计算冠层覆盖度比例
cover_percentage <- cellStats(canopy_cover_raster, stat = 'mean') * 100
cat("冠层覆盖度：", cover_percentage, "%\n")

# 可视化冠层覆盖度
plot(canopy_cover_raster, col = c("white", "green"), main = "Canopy Cover")




## AGB ---------------------------------------------------------------------


# 设置栅格分辨率
resolution <- 1

# 生成冠层高度模型 (CHM)
chm <- grid_canopy(las, res = resolution, algorithm = p2r())

# 检查生成的冠层高度模型
plot(chm, main = "Canopy Height Model", col = terrain.colors(50))

# 冠层高度模型的基本统计信息
summary(chm)

# 假设生物量模型 AGB = a * (Height)^b
# 其中 a 和 b 是根据研究或实验确定的常数
# 下面的 a 和 b 值是示例值，请根据实际情况调整
a <- 0.05  # 常数，代表木材密度或其他与森林类型相关的参数
b <- 2.5   # 幂指数，代表高度与生物量的关系

# 计算 AGB (单位为每个栅格的生物量, 例如吨/公顷)
agb <- a * (chm[]^b)

# 将 AGB 转换为栅格对象
agb_raster <- chm
values(agb_raster) <- agb

# 绘制 AGB 栅格
plot(agb_raster, main = "Above-Ground Biomass (AGB)", col = terrain.colors(50))

# 保存 AGB 栅格为文件
writeRaster(agb_raster, "agb_raster.tif", format = "GTiff")




# 植被格局分析 ------------------------------------------------------------------


## 冠层斑块的幂律分布 ---------------------------------------------------------------
install.packages("poweRlaw")   # 用于幂律分布检验
library(poweRlaw)

# 创建一个新的栅格，值大于5米的设为1，不大于5米的设为0
reclass_matrix <- matrix(c(-Inf, 6, 0,   # 高度小于等于5的部分设为0
                           6, Inf, 1),  # 高度大于5的部分设为1
                         ncol=3, byrow=TRUE)

chm_binary <- classify(chm, reclass_matrix)

# 查看分类后的结果
plot(chm_binary)


# 第4步：使用 poweRlaw 包进行幂律分布拟合和检验
# 创建幂律对象，plfit是用于幂律分布拟合的函数
pl_model <- conpl$new(chm_above_5m)


# 估计参数 xmin 和 alpha（幂律指数）
est <- estimate_xmin(pl_model)
pl_model$setXmin(est)

# 查看估计的最小值和alpha值
cat("最小值 xmin: ", est$xmin, "\n")
cat("幂律指数 alpha: ", est$pars, "\n")

# 第5步：进行拟合检验
# 通过Kolmogorov-Smirnov检验来评估模型的拟合优度
goftest <- bootstrap_p(pl_model, no_of_sims=1000)

# 输出结果
cat("p-value: ", goftest$p, "\n")

# 结果解释：
# 如果p-value较大（通常 > 0.1），表明数据符合幂律分布；
# 如果p-value较小，表明数据不符合幂律分布。