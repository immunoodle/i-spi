library(RColorBrewer)

colors=c("#E04C5C", "#FB894B", "#E7DA36", "#7DAF4C", "#187A51",
         "#5EA4A2", "#23AECE", "#3D3C4E", "#4D1836", "#C51B7D",
         "#E9A3C9",  "#B35806", "#F1A340", "#FEE08B", "#D9EF8B",
         "#91CF60", "#C7EAE5", "#5AB4AC", "#01665E", "#E7D4E8",
         "#AF8DC3", "#762A83")

arm.color <- c('infant' = "#23AECE",'child' = "#E04C5C", 'elderly' = "#7DAF4C")
arm.s.color <- c("#23AECE","#E04C5C", "#7DAF4C")

arm.infantchild.color <- c('infant' = "#23AECE",'child' = "#E04C5C")
arm.infantchilds.color <- c("#23AECE","#E04C5C")

arm.childelder.color <- c('child' = "#E04C5C", 'elderly' = "#7DAF4C")
arm.childelders.color <- c("#E04C5C", "#7DAF4C")

arm.short.color <- c('matplus_iap' = "#E04C5C", 'matplus_iwp' = "#7DAF4C", 'matminus_iwp' = "#23AECE")
arm.reserve.color <- c("#E04C5C", "#7DAF4C", "#23AECE", "#FB894B", "#E7DA36", "#187A51",
                       "#5EA4A2", "#3D3C4E", "#4D1836", "#C51B7D", "#E9A3C9", "#B35806", "#F1A340", "#FEE08B", "#D9EF8B",
                       "#91CF60", "#C7EAE5", "#5AB4AC", "#01665E", "#E7D4E8", "#AF8DC3", "#762A83")

arm.boxplot.color <- c("#E04C5C", "#86B3B6", "#23AECE", "#E9A3C9")
arm.crossbar <- c("#E04C5C","#E04C5C","#86B3B6","#86B3B6", "#23AECE", "#23AECE")


sex.color <- c('Female' = "#F1A340", 'Male' = "#91CF60")

visit.color <- c('vaccinated' = "white", 'boosted' = "lightgrey")
visit.shape <- c('vaccinated' = 21, 'boosted' = 16)
visit.boxplot.color <- c("#4D1836", "#91CF60")
visit.crossbar <- c("#4D1836", "#4D1836", "#91CF60", "#91CF60")

clust.colors <- c("#E69F00", "#56B4E9", "#009E73", "#D31E00", "#0072B2", "#D55E00", "#CC79A7")
tangle.clust.colors <- c('1'="#E69F00", '2'="#56B4E9", '3'="#009E73", '4'="#D31E00", '5'="#0072B2", '6'="#D55E00", '7'="#CC79A7")
clust.color.umap <- c('1' = "#E69F00", '2' = "#56B4E9", '3' = "#009E73", '4' = "#D31E00", '5' = "#0072B2", '6' = "#D55E00",'7' = "#CC79A7")

clust.colors.8 <- c("#E69F00", "#56B4E9", "#009E73", "#D31E00", "#762A83", "#0072B2", "#D55E00", "#CC79A7")
tangle.clust.colors.8 <- c('1'="#E69F00", '2'="#56B4E9", '3'="#009E73", '4'="#D31E00", '5'="#762A83", '6'="#0072B2", '7'="#D55E00", '8'="#CC79A7")
clust.color.umap.8 <- c('1' = "#E69F00", '2' = "#56B4E9", '3' = "#009E73", '4' = "#D31E00", '5'="#762A83", '6' = "#0072B2", '7' = "#D55E00",'8' = "#CC79A7")


# tangle.clust.colors <- c('1'="red",'2'="darkolivegreen",'3'="deepskyblue",'4'="deeppink2",'5'="darkorchid3",'6'="darkorange2",'7'="cyan4")

# anno_colors <- list(Arm = arm.color)

##arm.color <- c('Non-Pregnant 15 mcg' = "#E9A3C9", 'Pregnant 15 mg' = "#7DAF4C", 'Pregnant 30 mcg' = "#187A51")

##arm.color <- c('Non-Pregnant 15 mcg' = "#D37295", 'Pregnant 15 mg' = "#86B3B6", 'Pregnant 30 mcg' = "#499894")

