library(pacman)
library(tidyverse)
library(funModeling)
library(Hmisc)
library(DataExplorer)
library(dplyr)
library(sqldf)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(ggplot2)
library(partykit)
library(smbinning)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
p_load(scorecard)

old_customers_1 <- read.delim("/Users/mazhecheng/Desktop/数亿惠/RdataAnylst/20210622/new_customers.txt")
old_customers_2 <- read.delim("/Users/mazhecheng/Desktop/数亿惠/RdataAnylst/20210622/new_customers_2.txt")
old_customers_3 <- read.delim("/Users/mazhecheng/Desktop/数亿惠/RdataAnylst/20210622/new_customers_3.txt")


old_customers <- sqldf("select distinct *
                        from old_customers_1 a 
                       join old_customers_2 b on(a.tq_user_id = b.tq_user_id and a.tq_order_no = b.tq_order_no)
                       join old_customers_3 c on(a.tq_user_id = c.tq_user_id and a.tq_order_no = c.tq_order_no)")

write.csv(old_customers,file = "/Users/mazhecheng/Desktop/数亿惠/RdataAnylst/20210622/new_customers_all.csv")

new_customers_all <- read.delim("~/Desktop/数亿惠/RdataAnylst/20210622/new_customers_all.csv")

old_customers <- new_customers_all[, -1]

old_customers[is.na(old_customers)] <- -999

old_customers <- sqldf("select * from old_customers where target <> 2")

table(old_customers$target)

desc_df <- profiling_num(old_customers)

desc_df2 <- df_status(old_customers)

df_result <- sqldf("select * from desc_df a join desc_df2 b on(a.variable = b.variable)")


old_customers$target <- as.factor(old_customers$target)


library(DataExplorer)

# 数据区分探索
plot_density(old_customer)
plot_bar(old_customer)
plot_intro(old_customer)
plot_missing(old_customer)
plot_boxplot(old_customer, by = "target")


bins_tree <- woebin(old_customers, y = "target", method = "tree")

tree_woe<-data.table::rbindlist(bins_tree)
write.csv(tree_woe,file = "/Users/mazhecheng/Desktop/数亿惠/RdataAnylst/20210622/tree_woe.csv")


plotlist <- woebin_plot(bins_tree)
for (i in 1:length(plotlist)) {
  ggplot2::ggsave(
    paste0("~/Desktop/数亿惠/RdataAnylst/20210622/tree-", names(plotlist[i]), ".png"), plotlist[[i]],
    width = 15, height = 9, units = "cm"
  )
}




bins_chi <- woebin(old_customers, y = "target", method = "chimerge", stop_limit = 0.5)

plotlist_chi <- woebin_plot(bins_chi)
for (i in 1:length(plotlist_chi)) {
  ggplot2::ggsave(
    paste0("~/Desktop/数亿惠/RdataAnylst/20210622/chimerge-", names(plotlist_chi[i]), ".png"), plotlist_chi[[i]],
    width = 15, height = 9, units = "cm"
  )
}

bins_germ_df = data.table::rbindlist(bins_chi)


control <- rpart.control(xval = 10, minsplit = 2, minbucket = 6, maxdepth = 8,surrogatestyle=1)

dtree <- rpart(target ~ ., data = old_customers, method = "class", parms = list(split = "information"), control = control)


summary(dtree)
dtree$variable.importance
printcp(dtree)
plotcp(dtree, lwd = 2)

dtree$cptable[which.min(dtree$cptable[, "xerror"]), "CP"]

library(rpart.plot)
rpart.plot(dtree,
           branch = 1, shadow.col = "gray",
           box.col = "green", border.col = "blue",
           split.col = "red", split.cex = 1.2, main = "决策树"
)

asRules(dtree)



info_value = iv(old_customers,y = "target")

dt_sel = var_filter(old_customers,y = 'target',iv_limit = 0.1,missing_limit = 0.75)

names(dt_sel)

bins2_tree = woebin(old_customers,y = "target",method = "tree")

dt_woe = woebin_ply(old_customers,bins = bins_chi)

bins_df = data.table::rbindlist(dt_woe)


head(dt_woe)


model<-glm(target~.,data=old_customers,family = "binomial")

score <- scorecard(bins2_tree,model =dtree ,points0 = 600,odds0 = 1/19)

bins_germ_woe = data.table::rbindlist(score)

scorecard_ply(dt = old_customers,card = score)


score$app_system_count
