
library(ggplot2)
library(dplyr)
library(tidyr)


##### Target dataset #################

junction_table=read.table("~/tsclient/torresdizm/Box/ATT LAB/Katharina/Junction_Plots/2021_11_08_with_SEMA6A_SRSF10/target_all.csv",sep = ",",header = T)
NT5C2_data=junction_table[,2:5]
colnames(NT5C2_data)=c("names","exon4a_exon5","exon4_exon5","exon4_exon4a")
NT5C2_data_long=NT5C2_data %>% gather(junction,s_norm,2:4)


sorted_names = NT5C2_data_long %>% group_by(names) %>% summarise(total = sum(log2(1+s_norm))) %>% arrange(total)
sorted_names = sorted_names$names
#print(sorted_names )
NT5C2_data_long$names = factor(NT5C2_data_long$names, levels = sorted_names)

lsv_source=c("exon4_exon4a","exon4_exon5")
lsv_target=c("exon4a_exon5","exon4_exon5")
for (g in c("lsv_source","lsv_target")) {
      p = ggplot(NT5C2_data_long[NT5C2_data_long$junction %in% get(g),], aes(names,s_norm,fill = junction)) +
        geom_bar(stat="identity", size = .8) + ggtitle(paste("TARGET" ,"NT5C2", g) ) +
        theme_bw(base_size = 10)  + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        theme(legend.position="bottom")
      filename <- paste("TARGET" , "NT5C2", g , "barplot.png", sep = "_")
      ggsave(filename, width = 18 , height = 5)
      
      p = ggplot(NT5C2_data_long[NT5C2_data_long$junction %in% get(g),], aes(names,s_norm,fill = junction)) +
        geom_bar(stat="identity", size = .8, position = "fill") + ggtitle(paste("TARGET" ,"NT5C2", g) ) +
        theme_bw(base_size = 10)  + 
        ylab("Percentage") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        theme(legend.position="bottom")
      filename <- paste("TARGET" , "NT5C2", g , "frequency.png", sep = "_")
      ggsave(filename, width = 18 , height = 5)
}      
      

###### SJ dataset ########


junction_table=read.table("~/tsclient/torresdizm/Box/ATT LAB/Katharina/Junction_Plots/2021_11_08_with_SEMA6A_SRSF10/SJ_all.csv",sep = ",",header = T)

NT5C2_data=junction_table[,2:5]
colnames(NT5C2_data)=c("names","exon4a_exon5","exon4_exon5","exon4_exon4a")
NT5C2_data_long=NT5C2_data %>% gather(junction,s_norm,2:4)


sorted_names = NT5C2_data_long %>% group_by(names) %>% summarise(total = sum(log2(1+s_norm))) %>% arrange(total)
sorted_names = sorted_names$names
print(sorted_names )
NT5C2_data_long$names = factor(NT5C2_data_long$names, levels = sorted_names)

lsv_source=c("exon4_exon4a","exon4_exon5")
lsv_target=c("exon4a_exon5","exon4_exon5")
for (g in c("lsv_source","lsv_target")) {
  p = ggplot(NT5C2_data_long[NT5C2_data_long$junction %in% get(g),], aes(names,s_norm,fill = junction)) +
    geom_bar(stat="identity", size = .8) + ggtitle(paste("SJ" ,"NT5C2", g) ) +
    theme_bw(base_size = 10)  + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    theme(legend.position="bottom")
  filename <- paste("SJ" , "NT5C2", g , "barplot.png", sep = "_")
  ggsave(filename, width = 18 , height = 5)
  
  p = ggplot(NT5C2_data_long[NT5C2_data_long$junction %in% get(g),], aes(names,s_norm,fill = junction)) +
    geom_bar(stat="identity", size = .8, position = "fill") + ggtitle(paste("SJ" ,"NT5C2", g) ) +
    theme_bw(base_size = 10)  + 
    ylab("Percentage") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    theme(legend.position="bottom")
  filename <- paste("SJ" , "NT5C2", g , "frequency.png", sep = "_")
  ggsave(filename, width = 18 , height = 5)
}      

