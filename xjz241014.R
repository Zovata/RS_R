
# 配置 ----------------------------------------------------------------------
# 分节符快捷键 Ctrl+Shift+R
# install.packages("lidR")
library(lidR)

getwd()



# 读入数据 --------------------------------------------------------------------


las1 <- readLAS("C:/Users/15850/GS/xjz/Z123las/20241011新济州北岛区域1.las")

print(las1)


las_check(las1)


## 初步绘图 --------------------------------------------------------------------

plot(las1)

plot(las1, color = "ScanAngleRank", bg = "white", axis = TRUE, legend = TRUE)



# Ground Classification ---------------------------------------------------

# 安装 lidR 包（如果未安装）
# install.packages("lidR")

# 加载 lidR 包
library(lidR)

# 读取 LAS 文件
las_file <- "path_to_your_las_file.las"  # 替换为你的 las 文件路径
las <- readLAS(las_file)

# 检查是否成功加载数据
if (is.empty(las)) {
  stop("LAS 文件加载失败。请检查文件路径和文件内容。")
}

# 使用 PMF (Progressive Morphological Filter) 进行地面分类
# 可调节的参数有 csf、slope、threshold 等
las1 <- classify_ground(las1, algorithm = pmf(ws = 5, th = 2))

# 检查分类结果
summary(las1)

# 保存分类后的 LAS 文件
output_file <- "path_to_save_classified_las_file.las"
writeLAS(las, output_file)

# 可视化点云，地面点分类为2
plot(las, color = "Classification")

# 可视化
library(ggplot2)
