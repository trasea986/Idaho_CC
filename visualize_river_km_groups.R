#import data from Dan Isaak and USFS collaborators to visualize patterns across individual species and native vs. invasive status

#one thing to be very careful of: even opening the csv will convert the stream order range to a date

library(tidyverse)

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

df_optimal$Climate_Scenario <- relevel(df_optimal$Climate, "Current climate")

#make species figure, so listing all of them

#color coding by native/invasive

df_optimal_sp <- ggplot(data = df_optimal, aes(x=Species, y = Total, fill=Climate_Scenario)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  facet_wrap(~OrderClass, scales='free') +
  scale_fill_manual('Climate Scenario', values=c("dodgerblue2","gold","firebrick1")) +
  xlab("Species") +
  ylab("River Kilometers") +
  scale_y_continuous(expand = c(0,0), limits = c(0, max(df_optimal$Total+500))) +
  theme_classic(base_size=14) +
  guides(size = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust=1, colour = c("red", "red", "black", "black", "black", "red", "black", "black")))

ggsave("df_optimal_sp.png", plot = df_optimal_sp, 
       width = 40, height = 20, units = "cm", dpi = 360)

#we talked about only looking at 3-4 and 5-6 stream order

df_optimal_order <- df_optimal %>%
  filter(OrderClass == "3-4" | OrderClass == "5-6")

df_optimal_sp_26 <- ggplot(data = df_optimal_order, aes(x=Species, y = Total, fill=Climate_Scenario)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  facet_wrap(~OrderClass, scales='free') +
  scale_fill_manual('Climate Scenario', values=c("dodgerblue2","gold","firebrick1")) +
  xlab("Species") +
  ylab("River Kilometers") +
  scale_y_continuous(expand = c(0,0), limits = c(0, max(df_optimal_order$Total + 500))) +
  theme_classic(base_size=14) +
  theme(axis.text.x = element_text(angle = 45, hjust=1, colour = c("red", "red", "black", "black", "black", "red", "black", "black")))

ggsave("df_optimal_sp_26.png", plot = df_optimal_sp_26, 
       width = 40, height = 20, units = "cm", dpi = 360)

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

df_sum_order_all <- ggplot(data = df_summary, aes(x=Status, y = Meankm, fill=Climate_Scenario)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin=pmax(Meankm-Sdkm, 0), ymax=Meankm+Sdkm),
                width=.3,
                position=position_dodge(.9)) +
  facet_wrap(~OrderClass, scales='free') +
  scale_fill_manual('Climate Scenario', values=c("dodgerblue2","gold","firebrick1")) +
  xlab("Status") +
  ylab("Mean River Kilometers") + 
  scale_y_continuous(expand = c(0,0), limits = c(0, (max(df$Total)+500))) +
  theme_classic(base_size=14) +
  theme(axis.text.x = element_text(angle = 25, hjust=1, color = c("black", "red", "red")))

ggsave("df_sum_order_all.png", plot = df_sum_order_all, 
       width = 40, height = 20, units = "cm", dpi = 360)

#again, if only focusing on the 3-4 and 5-6
df_summary_order <- df_summary %>%
  filter(OrderClass == "3-4" | OrderClass == "5-6")

df_sum_order_26 <- ggplot(data = df_summary_order, aes(x=Status, y = Meankm, fill=Climate_Scenario)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin=pmax(Meankm-Sdkm, 0), ymax=Meankm+Sdkm),
                width=.3,
                position=position_dodge(.9)) +
  facet_wrap(~OrderClass, scales='free') +
  scale_fill_manual('Climate Scenario', values=c("dodgerblue2","gold","firebrick1")) +
  xlab("Status") +
  ylab("Mean River Kilometers") +
  scale_y_continuous(expand = c(0,0), limits = c(0, (max(df_summary$Meankm)+1000))) +
  theme_classic(base_size=14) +
  theme(axis.text.x = element_text(angle = 25, hjust=1, color = c("black", "red", "red")))

ggsave("df_sum_order_26.png", plot = df_sum_order_plot, 
       width = 40, height = 20, units = "cm", dpi = 360)
