setwd("Users/mojave/Desktop/Projects/Segmentation Analysis for Home Device")
DT = read.csv("./Survey Data.csv")
DT = DT[1:250,]
View(DT)
summary(DT)
dim(DT)
SurveyVar <- c(30:41, 52:56 )

ScaDT= apply(DT[, SurveyVar], 2, function(x) scale(x))
View(ScaDT)
EuclideanD <- dist(ScaDT[1:250,])
EuclideanD = as.matrix(EuclideanD)
View(EuclideanD)
View(round(EuclideanD, 1))

#CLustering Methods
EuclideanD = dist(ScaDT[1:250, ])
Hierarchichal_Cluster  = hclust(EuclideanD)
plot(Hierarchichal_Cluster)
heatmap(as.matrix(ScaDT[1:250,]))

HC_membership = as.vector(cutree(Hierarchichal_Cluster, k=4))
DT_HC = cbind(DT, HC_membership)
View(DT_HC)
prop.table(table(DT_HC$HC_membership))
Mean_by_Group <- function(data, groups) {
  aggregate(data, list(groups),
            function(x) mean(as.numeric(x)))
}
Group_Character_HC = Mean_by_Group(DT_HC, DT_HC$HC_membership)

Group_Character_HC
View(Group_Character_HC)

save(ScaDT, file = "./ScaDT.RData")
save(ScaDT, file = "./ScaDT.csv")
save(DT_HC, file = "./DT_HC.xlsx")
install.packages("writexl")
library("writexl")
write_xlsx(DT_HC, "./DT_HC.xlsx")
