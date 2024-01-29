library(tidyverse)

df <- read_csv("C://Users//rafae//Downloads//shopping_trends_updated.csv")

head(df)

dim(df)
ncol(df)
nrow(df)

colnames(df)

str(df)

summary(df)

sum(is.na(df))

sum(duplicated(df))

levels(df$Gender) <- c("Mulheres", "Homens")

ggplot(df, aes(x = factor(df$Gender), fill = Gender)) +
  geom_bar(stat = "count") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.3, hjust = 0.5) +
  geom_label(stat = "count", aes(label = c("Mulheres", "Homens")[..x..]), vjust = -0.9, hjust = 0.5) +
  theme(legend.position = "none", axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), plot.title = element_text(hjust = 0.5)) +
  labs(title = "Vendas por gênero", x = "Gênero", y = "Quantidade de compras")

  df %>% 
  group_by(Gender) %>% 
  summarise(count = n()) %>%  
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(desc(percentage)) %>% 
  ggplot(df, mapping = aes(x = "", y = percentage, fill = Gender)) +
  geom_col() +
  coord_polar(theta = "y") +
  geom_text(aes(label = Gender), position = position_stack(vjust = 0.5)) +  
  geom_text(aes(label = paste0(format(percentage / 100 * 100, nsmall = 2, big.mark = ","), "%")),
            position = position_stack(vjust = 0.30)) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5)
    ) +
  labs(title = "Porcentagem de compras entre os gêneros")

ggplot(df, aes(x = Age)) +
  geom_histogram(bins = 25, color = "skyblue", fill = "skyblue", alpha = 0.7) +
  geom_density(color = "red") +
  labs(x = "Age", y = "Count / Density", title = "Age Distribution Histogram with Density Curve") +
  theme_bw()
  
df %>% 
  count(df$Category)

ggplot(df, aes(x = factor(df$Category), fill = df$Category)) +
  geom_bar(stat = "count") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.3, hjust = 0.5) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    #axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),  
    text = element_text(family = "Open Sans"),  
    panel.background = element_blank(),  
    panel.grid.major.y = element_line(color = "gray90", size = 0.2),  
    plot.background = element_rect(fill = "white")  
  ) +
  labs(
    title = "Vendas por Categoria",  
    subtitle = "Total de compras por categoria em 2024",
    y = "Quantidade de Compras",
    caption = "Fonte: Dados da empresa XYZ"  
  ) +
  scale_fill_brewer(palette = "Set2")  


df %>% 
  count(`Item Purchased`) %>% 
  arrange(desc(n))

df %>% 
  group_by(`Item Purchased`) %>% 
  summarise(count_item = n()) %>% 
  ggplot(mapping = aes(x = count_item, y = `Item Purchased`, fill = `Item Purchased`)) +
  geom_col()

 
