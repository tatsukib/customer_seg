# Carregando as bibliotecas -----------------------------------------------


# Libraries
library(tidyverse)
library(patchwork)
library(plyr)
library(plotly)
library(GGally)
library(dendextend)
library(factoextra)

# Explorando os dados -----------------------------------------------------

# Load Data
data <- read.csv('Mall_Customers.csv', check.names = F)
head(data)
 
glimpse(data)

# Verificando se há algum Na no dataset
anyNA(data)

# Renomeando as colunas
newColnames <- c('Id','Gender','Age','Income','Score')

names(data) <- newColnames

glimpse(data)

# Visualizando os dados ---------------------------------------------------


# Distribuição entre homens e mulheres

data %>% 
  group_by(Gender) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count)*100) %>% 
  ggplot(aes(x = Gender,
             y= perc,
             fill = Gender)) +
  geom_bar(stat = 'identity',
           alpha = .5,
           show.legend = F)+
  geom_text(aes(label = paste0(perc, '%')),
            size = 4,
            vjust = -.5) +
  labs(title = "Gender Distribution",
       y = 'Percent')


#Calculando a média de idade de cada grupo
genderGroup <- ddply(data, 'Gender', summarise, grp.mean = mean(Age))

# Plots Age x Gender
p11 <- ggplot(data = data,
              aes(x = Age,
                  color = Gender,
                  fill = Gender)) +
  geom_histogram(alpha = .4,
                 position = 'identity',
                 show.legend = F,
                 binwidth = 1) + 
  geom_vline(data = genderGroup,
             aes(xintercept = grp.mean,
                 color = Gender),
             linetype = 'dashed',
             show.legend = F) + 
  labs(title = "Histogram")


p12 <-  ggplot(data = data,
               aes(x = Age,
                   color = Gender,
                   fill = Gender)) +
  geom_density(alpha = .5,
               show.legend = F) +
  geom_vline(data = genderGroup,
             aes(xintercept = grp.mean,
                 color = Gender),
             linetype = 'dashed',
             show.legend = F) +
  labs(title = "Density")

p13 <-  ggplot(data = data,
               aes(x = Age,
                   y = Gender,
                   color = Gender,
                   fill = Gender)) +
  geom_boxplot(alpha = .5) + 
  labs(title = "Box-Plot",
       y = NULL) +
  coord_flip()


p11+p13+p12 + plot_layout(widths = c(2,1)) + plot_annotation(title = "Age - Gender")


# Plots Income x Gender

#Calculando a média de idade de cada grupo
genderGroup <- ddply(data, 'Gender', summarise, grp.mean = mean(Income))
genderGroup

p21 <- ggplot(data = data,
              aes(x = Income,
                  color = Gender,
                  fill = Gender)) +
  geom_histogram(alpha = .4,
                 position = 'identity',
                 show.legend = F,
                 binwidth = 3) + 
  geom_vline(data = genderGroup,
             aes(xintercept = grp.mean,
                 color = Gender),
             linetype = 'dashed',
             show.legend = F) + 
  labs(title = "Histogram")


p22 <-  ggplot(data = data,
               aes(x = Income,
                   color = Gender,
                   fill = Gender)) +
  geom_density(alpha = .5,
               show.legend = F) +
  geom_vline(data = genderGroup,
             aes(xintercept = grp.mean,
                 color = Gender),
             linetype = 'dashed',
             show.legend = F) +
  labs(title = "Density") 

p23 <- ggplot(data = data,
               aes(x = Income,
                   y = Gender,
                   color = Gender,
                   fill = Gender)) +
  geom_boxplot(alpha = .5) + 
  labs(title = "Box-Plot",
       y = NULL) +
  coord_flip()

p21+p23+p22 + plot_layout(widths = c(2,1)) + plot_annotation(title = "Income - Gender")

# Plots Score x Gender

#Calculando a média de idade de cada grupo
genderGroup <- ddply(data, 'Gender', summarise, grp.mean = mean(Score))
genderGroup

p31 <- ggplot(data = data,
              aes(x = Score,
                  color = Gender,
                  fill = Gender)) +
  geom_histogram(alpha = .4,
                 position = 'identity',
                 show.legend = F,
                 binwidth = 3) + 
  geom_vline(data = genderGroup,
             aes(xintercept = grp.mean,
                 color = Gender),
             linetype = 'dashed',
             show.legend = F) + 
  labs(title = "Histogram") 

p32 <-  ggplot(data = data,
               aes(x = Score,
                   color = Gender,
                   fill = Gender)) +
  geom_density(alpha = .5,
               show.legend = F) +
  geom_vline(data = genderGroup,
             aes(xintercept = grp.mean,
                 color = Gender),
             linetype = 'dashed',
             show.legend = F) +
  labs(title = "Density") 

p33 <- ggplot(data = data,
              aes(x = Score,
                  y = Gender,
                  color = Gender,
                  fill = Gender)) +
  geom_boxplot(alpha = .5) + 
  labs(title = "Box-Plot",
       y = NULL) +
  coord_flip() 


p31+p33+p32 + plot_layout(widths = c(2,1)) + plot_annotation(title = "Score - Gender")


#correlação

ggpairs(data, columns = 3:5, mapping = ggplot2::aes(color = Gender))


#distancias
dist <- data %>% 
  select(Income, Score) %>% 
  scale() %>% 
  dist(method = 'euclidean')

#verificando elbow

data %>% 
  select(Income, Score) %>% 
  scale() %>% 
  fviz_nbclust(FUN = hcut, method = 'wss')

# verificando silhueta
data %>% 
  select(Income, Score) %>% 
  scale() %>% 
  fviz_nbclust(FUN = hcut, method = 'silhouette')

# dendogramas

hc1 <- hclust(dist, method = 'single') %>% 
  as.dendrogram() %>% 
  set('labels', '') %>% 
  set('branches_k_color', k = 5)
plot(hc1, type = 'triangle', main = "Hierarchical Clustering: Single Linkage") 


hc2 <- hclust(dist, method = 'complete') %>% 
  as.dendrogram() %>% 
  set('labels', '') %>% 
  set('branches_k_color', k = 5) 
plot(hc2, type = 'triangle', main = "Hierarchical Clustering: Complete Linkage")



hc3 <- hclust(dist, method = 'average') %>% 
  as.dendrogram() %>% 
  set('labels', '') %>% 
  set('branches_k_color', k = 5) 
plot(hc3,  type = 'triangle', main = "Hierarchical Clustering: Average Linkage")


hc4 <- hclust(dist, method = 'ward.D') %>% 
  as.dendrogram() %>% 
  set('labels', '') %>% 
  set('branches_k_color', k = 5) 
plot(hc4,  type = 'triangle', main = "Hierarchical Clustering: Ward's Method")


# clusters

clust1 <- cutree(hc1, k = 5) %>% 
  data.frame() %>% 
  cbind(data) %>% 
  ggplot() + 
  geom_point(aes(x = Income,
                 y = Score,
                 color = as.factor(.)),
             show.legend = F) +
  labs(title = "Single Linkage") 

clust2 <- cutree(hc2, k = 5) %>% 
  data.frame() %>% 
  cbind(data) %>% 
  ggplot() + 
  geom_point(aes(x = Income,
                 y = Score,
                 color = as.factor(.)),
             show.legend = F) +
  labs(title = "Complete Linkage") 

clust3 <- cutree(hc3, k = 5) %>% 
  data.frame() %>% 
  cbind(data) %>% 
  ggplot() + 
  geom_point(aes(x = Income,
                 y = Score,
                 color = as.factor(.)),
             show.legend = F) +
  labs(title = "Average Linkage") 

clust4 <- cutree(hc4, k = 5) %>% 
  data.frame() %>% 
  cbind(data) %>% 
  ggplot() + 
  geom_point(aes(x = Income,
                 y = Score,
                 color = as.factor(.)),
             show.legend = F) +
  labs(title = "Ward's Method") 


clust1 + clust2 + clust3 + clust4

# dados padronizados
data_std <- data %>%
  select(Income, Score) %>% 
  scale()

# k-means
set.seed(100)

# data %>% 
#   select(Income, Score) %>% 
#   scale() %>% 
#   fviz_nbclust(FUN = kmeans, method = 'silhouette')


clusterK5 <- kmeans(data_std, centers = 5) %>%
  fviz_cluster(geom = 'point', data = data_std) +
  ggtitle("k-means clustering: k = 5") +
  theme(legend.position = 'bottom', legend.title = element_blank())

clusterK5


#dbscan

dbscan <- fpc::dbscan(data_std, eps = .4, MinPts = 3) 

data %>% 
  cbind(dbscan$cluster) %>% 
  ggplot() +
  geom_point(aes(x = Income,
                 y = Score,
                 color = as.factor(dbscan$cluster))) +
  theme(legend.position = 'bottom',
        legend.title = element_blank())

             