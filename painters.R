library(dplyr)
library(ggplot2)
library(FactoMineR)
library(MASS)
library(tidyr)

theme_set(theme_bw())

res.pca <- prcomp(painters[, -5])

dat.pca <- tibble(
  res.pca$x %>% as_tibble(),
  school = painters$School)

taus <- 100 * res.pca$sdev**2 / sum(res.pca$sdev**2)
labz <- sprintf("Dim. %i (%0.1f%%)", seq_along(taus), taus)
dat.pca %>%
  ggplot(aes(PC1, PC2, color = school)) + 
  geom_point() + 
  stat_ellipse() +
  labs(x = labz[1], y = labz[2])

cor(painters[, -5])


res.mca <- MCA(data.frame(lapply(painters, as.factor)), quali.sup = 5)

painters %>%
  as_tibble(rownames = "painter") %>%
  pivot_longer(2:4)
