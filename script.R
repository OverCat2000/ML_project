library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(GGally)
library(readr)
library(ggjoy)
library(FactoMineR)
library(factoextra)
library(ggpubr)
library(corrplot)
library(cowplot)
library(glue)
library(car)
library(tidyverse)

Medicalpremium <- read_csv("C:/Users/malis/3rdYear/New folder (3)/New folder/Medicalpremium.csv")
df = Medicalpremium
df

cat.cols = colnames(df[, -c(which(colnames(df) %in% c("Age", "Weight", "Height", "PremiumPrice")))])
num.cols = colnames(df[, c(which(colnames(df) %in% c("Age", "Weight", "Height", "PremiumPrice")))])
cat.cols
num.cols

df[, cat.cols] = lapply(df[, cat.cols], factor)
lapply(df, class)

dt = df

dt = dt %>%
  mutate(BMI = df$Weight/((df$Height/100)^2)) %>%
  mutate(BMI_cat = cut(BMI, breaks=c(-Inf, 18.5, 24.5, 29.9, 34.9, Inf), 
                       labels=c("underweight", "normal", "overweight", "obese", "Extreme")))
dt

dq = dt %>%
  mutate(Age_cat = cut(Age, breaks=c(17, 30, 59, Inf), labels=c("Young Adult", "Adult", "Old")))
dq

cat.cols = colnames(dt)[lapply(dt, class) == "factor"]
num.cols = colnames(dt)[lapply(dt, class) == "numeric"]

attach(dt)


##################################################EDA################################################## 
cols = c("darkred", "firebrick", "indianred2", "salmon", "lightsalmon")

ggplot(dt, mapping=aes(x=Age,
                       y=NumberOfMajorSurgeries, 
                       fill=NumberOfMajorSurgeries)) +
  geom_boxplot() +
  scale_fill_manual(values=c("darkred", "firebrick", "indianred2", "lightsalmon")) +
  theme(legend.position = "bottom")

dt %>%
  group_by(NumberOfMajorSurgeries) %>%
  count()

ggplot(dq, aes(y=AnyTransplants, x=PremiumPrice, fill=AnyTransplants)) +
    geom_boxplot() +
    scale_fill_manual(values=c("darkred", "indianred2")) +
  theme(legend.position = "bottom")

ggplot(dq, aes(y=AnyChronicDiseases, x=PremiumPrice, fill=AnyChronicDiseases)) +
    geom_boxplot() +
    scale_fill_manual(values=c("darkred", "indianred2")) +
  theme(legend.position = "bottom")

ggplot(dq, aes(y=HistoryOfCancerInFamily, x=PremiumPrice, fill=HistoryOfCancerInFamily)) +
    geom_boxplot() +
    scale_fill_manual(values=c("darkred", "indianred2")) +
  theme(legend.position = "bottom")

ggplot(dq, aes(y=BloodPressureProblems, x=PremiumPrice, fill=BloodPressureProblems)) +
    geom_boxplot() +
    scale_fill_manual(values=c("darkred", "indianred2"))

ggplot(dq, aes(y=Diabetes, x=PremiumPrice, fill=Diabetes)) +
    geom_boxplot() +
    scale_fill_manual(values=c("darkred", "indianred2")) +
  theme(legend.position = "bottom")

ggplot(dq, aes(y=NumberOfMajorSurgeries, x=PremiumPrice, fill=NumberOfMajorSurgeries)) +
    geom_boxplot() +
    scale_fill_manual(values=cols) +
  theme(legend.position = "bottom")

plt1 = ggplot(dq, aes(y=NumberOfMajorSurgeries, x=PremiumPrice, fill=NumberOfMajorSurgeries)) +
    geom_boxplot() +
    scale_fill_manual(values=cols) +
  labs(y="") +
  theme(legend.position = "none")

temp = dq %>% 
  group_by(NumberOfMajorSurgeries) %>%
  summarise(mean_price=mean(PremiumPrice)) %>%
  arrange(desc(mean_price))
  
plt2 = ggplot(temp, aes(y=NumberOfMajorSurgeries, x=mean_price, fill=NumberOfMajorSurgeries)) +
    geom_bar(stat="identity") +
    scale_fill_manual(values = cols) +
    labs(x="mean Premium Price") +
  theme(legend.position = "none")

leg = get_legend(plt2 +
                   theme(legend.position = "bottom"))

cow1 = plot_grid(plt2, plt1, ncol=2, rel_heights=c(2, 1))

plot_grid(cow1 , leg, ncol=1, rel_heights = c(1, 0.1))

temp = dq %>% 
  group_by(Age_cat) %>%
  summarise(mean_price=mean(PremiumPrice)) %>%
  arrange(desc(mean_price))
  
  plt = ggplot(temp, aes(x=Age_cat, y=mean_price, fill=Age_cat)) +
    geom_bar(stat="identity") +
    scale_fill_manual(values = c("darkred", "indianred2", "lightsalmon")) +
    labs(y="mean Premium Price") +
    theme(legend.position = "bottom")
  print(plt)

temp = dq %>% 
  group_by(Age) %>%
  summarise(mean_price=mean(PremiumPrice)) %>%
  arrange(desc(Age))
temp

ggplot(temp, mapping=aes(x=Age, y=mean_price)) +
  geom_point(color="indianred2") +
  geom_smooth(method = "lm", se=F, color="darkred") +
  labs(y="mean Premium Price")

plt1 = ggplot(dt, mapping=aes(x=PremiumPrice, fill=KnownAllergies)) +
  geom_histogram(binwidth=2000, color="black") +
  scale_fill_manual(values=c("indianred2", "darkred")) +
  theme(legend.position = "none") +
  facet_wrap(~factor(KnownAllergies, levels=c(1, 0)), nrow=2) +
  labs(y="")
plt2 = ggplot(dt, mapping=aes(x=PremiumPrice, y=KnownAllergies, fill=KnownAllergies)) +
  geom_joy2(scale=0.8) +
  scale_fill_manual(values = c("indianred2", "darkred")) +
  theme(legend.position="none")
leg = get_legend(plt2 +
                   theme(legend.position = "bottom"))

cow1 = plot_grid(plt2, plt1, ncol=2, rel_heights=c(2, 1))

plot_grid(cow1 , leg, ncol=1, rel_heights = c(1, 0.1))

histBox = function(var, ...) {
  plt1 = ggplot(dt, aes(x=!!sym(var))) +
    geom_histogram(..., color="black", fill="darkred") +
    xlab("")
  
  plt2 = ggplot(dt, aes(x=!!sym(var))) +
    geom_boxplot(fill="indianred2", color="black")
  
  plt3 = title = ggdraw()+
    draw_label(glue("Histogram and Boxplot of {var}"))
  
  cow1 = plot_grid(plt1, plt2, ncol=1, rel_heights=c(2, 1), align='v', axis='lr')
  plot_grid(plt3, cow1, ncol=1, rel_heights = c(0.1, 1))
  
  }

histBox("PremiumPrice", binwidth=2500)

temp = num.cols[4]
num.cols[4] = num.cols[5]
num.cols[5] = temp
ggcorr(dt[, num.cols], label=T, label_round=3, low="white", mid="lightsalmon", high="darkred") +
  labs(title="Correlation Heatmap")

model = lm(PremiumPrice ~ ., data=df)         

as.data.frame(vif(model))


##################################################FAMD################################################## 


X = df[, -dim(df)[2]]
y = df[, dim(df)[2]]

X.copy = X
cat.cols = colnames(X)[lapply(X, class) == "factor"]
cat.cols = cat.cols[1:(length(cat.cols) -1)]
for (col in cat.cols) {
  X[[col]] = recode_factor(X[[col]], "0" = paste0(col, "0"),
                           "1" = paste0(col, "1"))
}

X
res.famd = FAMD(X)

res.famd$var$contrib

fviz_famd_var(res.famd, "quanti.var", repel=T, col.var="contrib", gradient.cols=rev(cols))

fviz_famd_var(res.famd, "quali.var", repel=T, col.var="contrib", gradient.cols=rev(cols))

fviz_famd_var(res.famd, "var", repel=T, col.var="coord", gradient.cols=rev(cols))

fviz_screeplot(res.famd)

as.data.frame(res.famd$eig)

res.famd$var$contrib[, 1:2]

res.km = kmeans(res.famd$ind$coord, centers=3, nstart=25, iter.max=50)
fviz_mfa_ind(res.famd, habillage=as.factor(res.km$cluster), palette=c("darkred", "indianred2", "salmon"), addEllipses=T, repel=T, geom="point")

dt["cluster"] = as.factor(res.km$cluster)
dq["cluster"] = as.factor(res.km$cluster)

fviz_nbclust(X.copy, kmeans, method = "silhouette")

ggplot(dt, aes(x=cluster, y=PremiumPrice, color=cluster)) +
  geom_jitter() +
  labs(title="Visualizing clusters against Premium price")

ggplot(dt, aes(x=cluster, y=Age, color=cluster)) +
  geom_jitter() +
  labs(title="Visualizing clusters against Premium price")

ggplot() +
  geom_bar(dq, mapping=aes(x=cluster, fill=Age_cat), position="fill")

ggplot() +
  geom_bar(dt, mapping=aes(x=cluster, fill=Diabetes), position="fill")

cat.cols = colnames(dt)[lapply(dt, class) == "factor"]
cat.cols = cat.cols[1:(length(cat.cols) - 1)]

for (i in cat.cols) {
  plt = ggplot(dt, aes(x=cluster, fill=!!sym(i))) +
    geom_bar(position="fill") +
    scale_fill_brewer(palette="Set2")
  print(plt)
}

temp = dt[, c(cat.cols, "cluster")]
summary(temp[temp$cluster == 1, ])
summary(temp[temp$cluster == 2, ])
summary(temp[temp$cluster == 3, ])
