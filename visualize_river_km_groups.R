#import data from Dan Isaak and USFS collaborators to visualize patterns across individual species and native vs. invasive status

#one thing to be very careful of: even opening the csv will convert the stream order range to a date

library(tidyverse)
library(cowplot)

#read in data
df <- read.csv("data_reformat.csv", na.strings = " . ")

#change NA to 0
df[is.na(df)] <- 0

#set columns from character to factor
df$Species <- as.factor(df$Species)
df$Taxonomy <- as.factor(df$Taxonomy)
df$Native_Invasive <- as.factor(df$Native_Invasive)
df$Climate <- as.factor(df$Climate)
df$Temperature.class <- as.factor(df$Temperature.class)
df$OrderClass <- as.factor(df$OrderClass)
  #1-2 = headwaters, 3-4 = small rivers, 5-6 = large rivers, 7-8 = lower mainstem*
df$Clearwater <- as.numeric(df$Clearwater)
df$MidSnake <- as.numeric(df$MidSnake)
df$Panhandle <- as.numeric(df$Panhandle)
df$Salmon <- as.numeric(df$Salmon)
df$SnakeBear <- as.numeric(df$SnakeBear)
df$Total <- as.numeric(df$Total)

#for now, we will focus in on optimal

df_optimal <- df %>%
  filter(Temperature.class == "Optimal") %>%
  select(Species, Taxonomy, Native_Invasive, Climate, OrderClass, Total)

#change factor order of climate

levels(df_optimal$Species)

df_optimal$Climate_Scenario <- relevel(df_optimal$Climate, "Current climate")

#make species figure, so listing all of them

#color coding by native/invasive

#reorder: anadromous native, resident native, invasive resident trout, then small mouth

df_optimal$Species <- factor(df_optimal$Species, levels=c('Spring/Summer Chinook', 'Steelhead', 'Rainbow Trout', 'Bull Trout', 'Cutthroat Trout', 'Brook Trout', 'Brown Trout', 'Smallmouth Bass'))

#reorder the stream orders
df_optimal$OrderClass <- factor(df_optimal$OrderClass, levels=c('Headwaters', 'Small Rivers', 'Large Rivers', 'Lower Mainstem'))

#going to subset the data and make 4 plots. doing this because Chris is thinking having each axis almost match (e.g. 1-2 and 3-4 stream orders will have same axis, 5-6 and 7-8 would have their own).

#make plots

df_optimal_small <- df_optimal %>%
  filter(OrderClass == 'Headwaters' | OrderClass == 'Small Rivers')

df_optimal_small_sp <-  ggplot(data = df_optimal_small, aes(x=Species, y = Total, fill=Climate_Scenario)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  facet_wrap(~OrderClass, scales='free') +
  scale_fill_manual('Climate Scenario', values=c("dodgerblue2","gold","firebrick1")) +
  ylab("River Kilometers") +
  scale_y_continuous(expand = c(0,0), limits = c(0, max(df_optimal$Total+500))) +
  theme_bw(base_size=14) +
  guides(size = FALSE) +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x = element_text(angle = 90, hjust=1, colour = c("black", "black", "black", "black", "black", "red", "red", "red")))


df_optimal_large <- df_optimal %>%
  filter(OrderClass == 'Large Rivers' | OrderClass == 'Lower Mainstem')

df_optimal_large_sp <-  ggplot(data = df_optimal_large, aes(x=Species, y = Total, fill=Climate_Scenario)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  facet_wrap(~OrderClass, scales='free') +
  scale_fill_manual('Climate Scenario', values=c("dodgerblue2","gold","firebrick1")) +
  ylab("River Kilometers") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 5000)) +
  theme_bw(base_size=14) +
  guides(size = FALSE) +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x = element_text(angle = 90, hjust=1, colour = c("black", "black", "black", "black", "black", "red", "red", "red")))


#we talked about only looking at 3-4 and 5-6 stream order
# 
# df_optimal_order <- df_optimal %>%
#   filter(OrderClass == "3-4" | OrderClass == "5-6")
# 
# df_optimal_sp_26 <- ggplot(data = df_optimal_order, aes(x=Species, y = Total, fill=Climate_Scenario)) +
#   geom_bar(stat="identity", position = "dodge", color = "black") +
#   facet_wrap(~OrderClass, scales='free') +
#   scale_fill_manual('Climate Scenario', values=c("dodgerblue2","gold","firebrick1")) +
#   xlab("Species") +
#   ylab("River Kilometers") +
#   scale_y_continuous(expand = c(0,0), limits = c(0, max(df_optimal_order$Total + 500))) +
#   theme_classic(base_size=14) +
#   theme(axis.text.x = element_text(angle = 90, hjust=1, colour = c("black", "black", "black", "black", "black", "red", "red", "red")))
# 
# ggsave("df_optimal_sp_26.png", plot = df_optimal_sp_26, 
#        width = 40, height = 20, units = "cm", dpi = 360)

#other grouping is native/invasive and taxonomic groups
#will need to create a fresh column that combines taxonomic group and native/invasive status. starting with only optimal from above

df_optimal$Status <- paste0(df_optimal$Native_Invasive," ",df_optimal$Taxonomy)
df_optimal$Status <- as.factor(df_optimal$Status)

#will start with the calculating the average total
df_summary <- df_optimal %>%
  group_by(Status, OrderClass, Climate_Scenario) %>%
  summarize(Meankm = mean(Total), Sdkm=sd(Total))

#sd is NA for smallmouth, so going to turn that to 0
#change NA to 0
df_summary[is.na(df_summary)] <- 0

#change levels/order
df_summary$Status <- fct_relevel(df_summary$Status, c('Native Salmonidae','Invasive Salmonidae','Invasive Centrarchidae'))

#going to put here for ease of changing the status labels
levels(df_summary$Status) <- c("Native Salmonidae", "Invasive Salmonidae", "Invasive Smallmouth Bass")

df_summary_small <- df_summary %>%
  filter(OrderClass == 'Headwaters' | OrderClass == 'Small Rivers')

df_sum_order_small <- ggplot(data = df_summary_small, aes(x=Status, y = Meankm, fill=Climate_Scenario)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin=pmax(Meankm-Sdkm, 0), ymax=Meankm+Sdkm),
                width=.3,
                position=position_dodge(.9)) +
  facet_wrap(~OrderClass, scales='free') +
  scale_fill_manual('Climate Scenario', values=c("dodgerblue2","gold","firebrick1")) +
  ylab("Mean River Kilometers") + 
  scale_y_continuous(expand = c(0,0), limits = c(0, (max(df$Total)+500))) +
  theme_bw(base_size=14) +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x = element_text(angle = 90, hjust=1, color = c("black", "red", "red")))

df_summary_large <- df_summary %>%
  filter(OrderClass == 'Large Rivers' | OrderClass == 'Lower Mainstem')

df_sum_order_large <- ggplot(data = df_summary_large, aes(x=Status, y = Meankm, fill=Climate_Scenario)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin=pmax(Meankm-Sdkm, 0), ymax=Meankm+Sdkm),
                width=.3,
                position=position_dodge(.9)) +
  facet_wrap(~OrderClass, scales='free') +
  scale_fill_manual('Climate Scenario', values=c("dodgerblue2","gold","firebrick1")) +
  ylab("Mean River Kilometers") + 
  scale_y_continuous(expand = c(0,0), limits = c(0, 5000)) +
  theme_bw(base_size=14) +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x = element_text(angle = 90, hjust=1, color = c("black", "red", "red")))

#again, if only focusing on the 3-4 and 5-6
# df_summary_order <- df_summary %>%
#   filter(OrderClass == "3-4" | OrderClass == "5-6")
# 
# df_sum_order_26 <- ggplot(data = df_summary_order, aes(x=Status, y = Meankm, fill=Climate_Scenario)) +
#   geom_bar(stat="identity", position = "dodge", color = "black") +
#   geom_errorbar(aes(ymin=pmax(Meankm-Sdkm, 0), ymax=Meankm+Sdkm),
#                 width=.3,
#                 position=position_dodge(.9)) +
#   facet_wrap(~OrderClass, scales='free') +
#   scale_fill_manual('Climate Scenario', values=c("dodgerblue2","gold","firebrick1")) +
#   xlab("Status") +
#   ylab("Mean River Kilometers") +
#   scale_y_continuous(expand = c(0,0), limits = c(0, (max(df_summary$Meankm)+1000))) +
#   theme_classic(base_size=14) +
#   theme(axis.text.x = element_text(angle = 25, hjust=1, color = c("black", "red", "red")))
# 
# ggsave("df_sum_order_26.png", plot = df_sum_order_26, 
#        width = 40, height = 20, units = "cm", dpi = 360)


#combine the 8 panels of interest. top is by group, bottom by species.

#shared legend
legend_b <- get_legend(
  df_sum_order_small + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

plot_final <- plot_grid(legend_b, df_sum_order_small, df_sum_order_large, df_optimal_small_sp, df_optimal_large_sp, labels = c('', 'A)', '', 'B)', ''), label_size = 16, ncol = 1, rel_heights = c(.1, 1,1,1,1))

ggsave("plot_final.png", plot = plot_final, 
       width = 30, height = 50, units = "cm", dpi = 600)
