## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")

## ---- lib, eval=TRUE-----------------------------------------------------
library('pollen')

## ---- dat, eval=TRUE-----------------------------------------------------
data('pollen_count')
head(pollen_count)

## ---- df, eval=TRUE------------------------------------------------------
df <- subset(pollen_count, site=='Oz')
pollen_season(df, value="birch", date="date", method="95")

## ---- df2, eval=TRUE-----------------------------------------------------
df2 <- subset(pollen_count, site=='Atlantis')
pollen_season(df2, value="alder", date="date", method="95")

## ---- purrr, eval=TRUE---------------------------------------------------
library('purrr')
pollen_count %>% split(., .$site) %>% 
                map_df(~pollen_season(., value="hazel", date="date", method="95"), .id="site")

## ------------------------------------------------------------------------
df <- subset(pollen_count, site=='Oz')

ps_methods <- c("90", "95", "98", "Mesa", "Jager", "Lejoly")
names(ps_methods) <- c("90", "95", "98", "Mesa", "Jager", "Lejoly")
df_seasons <- ps_methods %>% map_df(~pollen_season(method=., x=df, value="birch", date="date"), .id="method") 
head(df_seasons)

## ------------------------------------------------------------------------
df <- subset(pollen_count, site=='Shire')
new_df <- outliers_replacer(df, value="alder", date="date")
identical(df, new_df)

## ------------------------------------------------------------------------
library('purrr')
new_pollen_count <- pollen_count %>% split(., .$site) %>% 
                  map_df(~outliers_replacer(., value="hazel", date="date", threshold=4))

