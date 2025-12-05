#读取OTU/ASV丰度表
ASV <- read.csv('Rare asv.csv', row.names = 1)
#行数
row = as.numeric(length(row.names(ASV)))
#列数
col = as.numeric(length(colnames(ASV)))
#求和
col_sum = rep(colSums(ASV), row)
#转化矩阵
col_sum = matrix(col_sum, nrow = col, ncol = row)
#OTU/ASV相对丰度表
ASV_relative=ASV/t(col_sum)
#检查每列结果是否为1
colSums(ASV_relative)
# CK1 CK2 CK3 LT1 LT2 LT3 MT1 MT2 MT3 HT1 HT2 HT3 
# 1   1   1   1   1   1   1   1   1   1   1   1 
#保存文件
write.csv(ASV_relative,"Rare relative.csv")
#加载R包
library(picante)
library(vegan)
#加载数据
otu<-read.csv("Rare relative.CSV",row.names=1)
#OTU丰富表
tax <- read.csv("Rare tax.csv",row.names=1)
#otu注释表
design <- read.csv("Group.csv",row.names=1)
#分组文件#统计样品数量
n<-length(colnames(otu))
n
##### 画图颜色设置,可根据RGB16进制码颜色自行修改
col_figure<-c("#FDDC7B","#4169B2","#B1A4C0","#479E9B","#ACCDDC","#DD5F60"              
              ,"#F2B379","#7CB6DB","#EE4A25","#BCEE68","#479E9B","#B1A4C0"              
              ,"#FDDC7B","#4169B2","#ACCDDC","#DD5F60","#F2B379","#7CB6DB"              
              ,"#FA3105","#CD3700","#BC8F8F","#1C86EE","#A4D3EE","blue")
##Phylum物种组成分析
aa<-cbind(otu,tax)
aa
library(car)
se_phy<-aggregate(aa[,1:n],by=list(aa$Phylum),FUN=sum)
rownames(se_phy)<-se_phy[,1]
mean<-se_phy[,-1]
#统计样品数量
a<-length(rownames(mean))
aa<-as.numeric(a)
a
mean<-mean[order(rowSums(mean),decreasing=T),]
mean#############此时物种已经按照门分类水平分类，可以保存此表用于其他分析
write.csv(mean,"稀有Phylum.csv")
rm(list = ls()) # 清除环境变量
library(circlize) # 引用包

# 读取数据
genus <- read.csv("phylum.csv", row.names = 1, header = T)

# 计算整理前十菌门相对丰度表
genus$rowsum <- apply(genus, 1, sum)
genus <- genus[order(genus$rowsum, decreasing = TRUE), ] # 对菌门进行降序排序
genus <- genus[, -ncol(genus)] # 删除求和列（最后一列）

# 计算物种相对丰度
df <- data.frame(apply(genus, 2, function(x) x / sum(x)), stringsAsFactors = FALSE)

# 按行求指定列平均值
df$Abundant <- apply(df[, 1:278], 1, mean) 
df$Aare <- apply(df[, 279:556], 1, mean)

# 只保留前 9 个菌门（去掉 Others）
df <- df[1:10, ] # 直接取前 9 行
df <- df[, c("Abundant", "Rare")] # 只保留两组的平均值

# 转换为矩阵
data1 <- as.matrix(df)

# 预设 links 配色
col <- c("#8DD3C7", "#BEBADA", '#009933', "#FF7F00", "#7ED1E4", '#CC3366',
         "#FB8072", "#F781BF", "#80B1D3")

# 预设 grid.col 颜色（去掉 Others）
grid.col <- c(
  Abundant = '#EF9A9A', 
  Aare = '#90CAF9',
  Proteobacteria = "#BEBADA", 
  Actinobacteria = "#8DD3C7", 
  Acidobacteria = '#009933', 
  Firmicutes = "#FF7F00",
  Verrucomicrobia = '#c5c5f2', 
  Gemmatimonadetes = '#CC3366',
  Chloroflexi = "#FB8072", 
  Bacteroidetes = "#F781BF", 
  Armatimonadetes = "#80B1D3",
  Unassigned = "#A65628"
)

# 设置 PDF 输出
pdf("ChordDiagram.pdf", width = 10, height = 8) # 设置 PDF 文件名和尺寸

# 绘制和弦图
chordDiagram(data1, 
             grid.col = grid.col, 
             column.col = col, 
             directional = -1)

# 添加图例
legend.col <- c(
  Proteobacteria = "#BEBADA", 
  Actinobacteria = "#8DD3C7", 
  Acidobacteria = '#009933', 
  Firmicutes = "#FF7F00",
  Verrucomicrobia = '#c5c5f2', 
  Gemmatimonadetes = '#CC3366',
  Chloroflexi = "#FB8072", 
  Bacteroidetes = "#F781BF", 
  Armatimonadetes = "#80B1D3",
  Unassigned = "#A65628"
)

legend(
  x = 1, y = 0.4,
  title = "Phylum", title.adj = 0,
  bty = 'n',
  legend = c(
    "Proteobacteria", "Actinobacteria", "Acidobacteria", "Firmicutes", 
    "Verrucomicrobia", "Gemmatimonadetes", "Chloroflexi", "Bacteroidetes", 
    "Armatimonadetes", "Unassigned"
  ),
  pch = 16,
  col = legend.col,
  cex = 1, pt.cex = 1,
  ncol = 1, xpd = TRUE
)

# 关闭 PDF 设备
dev.off()

circos.clear() # 清理当前对和弦图的参数设置
