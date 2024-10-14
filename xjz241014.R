
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
