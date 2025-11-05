## load libraries ##
library("tidytuesdayR")
library(tidyverse)
`%notin%` <- Negate(`%in%`)
## load libraries ##

## data engineering ##
df_obj <- tidytuesdayR::tt_load('2025-03-11')
films <- df_obj$pixar_films
ratings <- df_obj$public_response
df_main <- left_join(films, ratings, by = "film")
df_main <- df_main %>% filter(is.na(film) == FALSE)
df_main <- as.data.frame(df_main)
df_plot <- df_main %>% filter(is.na(rotten_tomatoes) == FALSE) # no rating for Lightyear, Luca, Turning Red
seqs <- c('Toy Story 2', 'Toy Story 3', 'Toy Story 4', 'Incredibles 2', 'Cars 2', 'Cars 3', 'Finding Dory', 'Monsters University')
df_plot$film_type <- ifelse(df_plot$film %notin% seqs, 'Original', 'Sequel')
df_plot$franchise <- ifelse(df_plot$film %in% c('Toy Story', 'Toy Story 2', 'Toy Story 3', 'Toy Story 4'), 'Toy Story', 
                      ifelse(df_plot$film %in% c('Finding Nemo', 'Finding Dory'), 'Nemo',
                      ifelse(df_plot$film %in% c('The Incredibles', 'Incredibles 2'), 'Incredibles',   
                      ifelse(df_plot$film %in% c('Monsters, Inc.', 'Monsters University'), 'Monsters',       
                      ifelse(df_plot$film %in% c('Cars', 'Cars 2', 'Cars 3'), 'Cars', 'Original Only')))))

# how correlated are the ratings metrics?
ggplot(df_plot) + 
  geom_point(aes(x=metacritic, y=rotten_tomatoes)) +
  scale_x_continuous(limits=c(40, 100)) + scale_y_continuous(limits=c(40, 100))
# there is a bit more of a spread for metacritic vs critics choice ratings and we won't lose any films

# create a composite index using metacritic and rotten tomatoes
df_plot$composite_rating <- (df_plot$rotten_tomatoes + df_plot$metacritic) / 2
df_plot$film <- factor(df_plot$film, levels = df_plot$film[order(df_plot$composite_rating)])
## data engineering ##

## variable exploration ##
# is run time important?
ggplot(df_plot) + geom_point(aes(x=run_time, y=composite_rating)) # nothing obvious
cor(df_plot$composite_rating, df_plot$run_time) # -0.18

# is film rating important?
df_plot %>% group_by(film_rating) %>% summarize(avg_rt = mean(composite_rating)) # nothing obvious
## variable exploration ##

## exploratory visualizations ##
# cars franchise scatter !!!
df_plot2 <- df_plot %>% rename(`Film Type` = film_type)
df_plot2$cars_franchise <- ifelse(df_plot2$franchise == 'Cars', 'Cars', 'Non-Cars')

ggplot(df_plot2, aes(x=release_date, y = composite_rating, color=cars_franchise)) + 
  geom_point(size=2) + geom_smooth(method=loess, se=FALSE, color = "darkgrey", linetype='dashed') + 
  theme_classic() +
  labs(x='Release Date', y = 'Composite Rating',
       title='Composite Rating by Release Date for all 23 Pixar Films',
       caption = '* The dashed line signifies a local polynomial regression fit to the data') +
  theme(
    axis.title.x=element_text(size=16), 
    axis.title.y=element_text(size=16), 
    axis.text.x=element_text(size=12), 
    axis.text.y=element_text(size=12), 
    legend.position = 'top', 
    legend.title = element_blank(), 
    title = element_text(size=16)
    ) + ylim(0, 100) +
  scale_color_manual(values=c('Cars'="red", 'Non-Cars'= "black"))

# toy story franchise scatter !!!
df_plot3 <- df_plot2 %>% rename(`Franchise` = franchise)
ggplot(df_plot3, aes(x=release_date, y = composite_rating, color=`Franchise`)) + 
  geom_point(size = 2) + 
  theme_classic() +
  labs(x='Release Date', y = 'Composite Rating', 
       title='Composite Rating by Release Date for all 23 Pixar Films',) +
  theme(legend.position = 'top',
    axis.title.x=element_text(size=16), 
    axis.title.y=element_text(size=16), 
    axis.text.x=element_text(size=12), 
    axis.text.y=element_text(size=12), 
    title = element_text(size=16)
  ) + ylim(0, 100) +
  scale_color_manual(values=c('Toy Story'='#05A0D1', 'Nemo'='orange','Incredibles'='black', 
                              'Monsters'='#00E63A', 'Cars'="red", 'Original Only'= "grey"))

# franchise vs original bar !!!
ggplot(df_plot3, aes(x=film, y = composite_rating, color=`Franchise`)) + 
  geom_segment(aes(x = film, xend = film, y = 0, yend = composite_rating)) + 
  geom_point(size = 4, alpha = 1) +
  coord_flip() + theme_classic() +
  labs(y = 'Composite Rating', x= '', 
       title='Ranked Composite Rating for all 23 Pixar Films') +
  theme(legend.position = 'top',
        axis.title.x=element_text(size=16), 
        axis.title.y=element_text(size=16), 
        axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12), 
        title = element_text(size=16)
  ) +
  scale_color_manual(values=c('Toy Story'='#05A0D1', 'Nemo'='orange','Incredibles'='black', 
                              'Monsters'='#00E63A', 'Cars'="red", 'Original Only'= "grey"))

## deep dive on originals vs sequels performance ##
# how have originals performed against sequels on avg? 
# originals vs sequels pdf facet!!!
ggplot(df_plot2, aes(x = composite_rating, fill = `Film Type`, color = `Film Type`)) +
  geom_density(alpha = 0.6) + facet_grid(cols = vars(`Film Type`)) +
  theme_classic() + 
  labs(y = 'Probability Density Function', x = 'Composite Rating', 
       title='Composite Rating Probability Density Function for Originals vs Sequels') + 
  theme(legend.position = "none", 
        axis.title.y=element_text(size=16), 
        axis.title.x=element_text(size=16),
        axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12), 
        title = element_text(size=15)) +
  xlim(0, 100) +
  scale_color_manual(values=c("lightblue2", "orange2")) + 
  scale_fill_manual(values=c("lightskyblue", "orangered"))

df_plot %>% 
  group_by(film_type) %>% 
  summarize(median_cr = median(composite_rating), cnt = n())
# even when you gamble on original content it still typically achieves a higher rating and has less variability !!!

wilcox.test(composite_rating ~ film_type, data = df_plot, exact = FALSE) # p-val of 0.22

# which franchise sequels fell off the most from the original?
franchises <- df_plot %>% filter(franchise != 'Original Only')
f_max_rt_score <- franchises %>% group_by(franchise) %>% summarize(max_rt_score = max(composite_rating)) 
franchises <- franchises %>% left_join(f_max_rt_score, by='franchise')
franchises$franchise <- factor(franchises$franchise, levels = c('Toy Story', 'Nemo', 'Incredibles', 'Monsters', 'Cars'))

# franchise decay facet !!!
ggplot(franchises, aes(x=release_date, y = composite_rating, color=franchise)) + 
  geom_point() + geom_line() +
  theme_classic() +
  labs(x='Release Date', y = 'Composite Rating', 
       title='Composite Rating by Release Date broken out by Franchise') +
  facet_grid(cols = vars(franchise)) + ylim(0, 100) +
  theme(legend.position = 'none',
        axis.title.x=element_text(size=16), 
        axis.title.y=element_text(size=16), 
        axis.text.x=element_text(size=7), 
        axis.text.y=element_text(size=12), 
        title= element_text(size=16)
  ) + scale_color_manual(values=c('Toy Story'='#05A0D1', 'Nemo'='orange','Incredibles'='black', 
                              'Monsters'='#00E63A', 'Cars'="red", 'Original Only'= "grey"))
# interesting how the two sequels to drop the most had the lowest original ratings, Cars 3 was a unique save

ts <- franchises %>% filter(franchise == 'Toy Story') %>% arrange(number)
ts$ct_lag <- lag(ts$composite_rating)
ts$ry_lag <-lag(year(ts$release_date))

nemo <- franchises %>% filter(franchise == 'Nemo') %>% arrange(number)
nemo$ct_lag <- lag(nemo$composite_rating)
nemo$ry_lag <-lag(year(nemo$release_date))

inc <- franchises %>% filter(franchise == 'Incredibles') %>% arrange(number)
inc$ct_lag <- lag(inc$composite_rating)
inc$ry_lag <-lag(year(inc$release_date))

mons <- franchises %>% filter(franchise == 'Monsters') %>% arrange(number)
mons$ct_lag <- lag(mons$composite_rating)
mons$ry_lag <-lag(year(mons$release_date))

cars <- franchises %>% filter(franchise == 'Cars') %>% arrange(number)
cars$ct_lag <- lag(cars$composite_rating)
cars$ry_lag <-lag(year(cars$release_date))
# ^ wish I had a window function here haha

franchises2 <- rbind(ts, nemo, inc, mons, cars)
franchises2 <- franchises2 %>% filter(is.na(ct_lag) == FALSE)
franchises2$seq_dif <- franchises2$composite_rating - franchises2$ct_lag
franchises2$yr_dif <- year(franchises2$release_date) - franchises2$ry_lag

franchises2 %>% select(film, seq_dif) %>% arrange(seq_dif) 
mean(franchises2$seq_dif) # 6 point drop on average
franchises2 %>% group_by(franchise) %>% summarize(avg_seq_dif = mean(seq_dif), avg_yr_dif = mean(yr_dif)) %>% arrange(avg_seq_dif)

ggplot(franchises2) + geom_point(aes(x=yr_dif, seq_dif)) 
cor(franchises2$yr_dif, franchises2$seq_dif) # 0.09
# doesn't seem to be much of a difference from year between release interval and performance decay
## deep dive on originals vs sequels performance ##

# additional data points
pre_2010 <- df_plot %>% filter(release_date < as.Date('2011-01-01'))
mean(pre_2010$rotten_tomatoes) # 95

# latest film rotten tomatoes scores
# Luca - Original, 91
# Turning Red - Original, 95
# Lightyear - Sequel, 74
# Elemental - Original, 73
# Inside Out 2 - Sequel, 91
# Elio - Original, 83

latest_originals_avg_rt <- (91 + 95 + 73 + 83) / 4
latest_sequels_avg_rt <- (74 + 91) / 2


# archive
ggplot(df_plot2, aes(x=`Film Type`, y=composite_rating)) + 
  geom_violin() + geom_jitter(height = 0, width = 0.08) +
  theme_classic() +
  labs(y = 'Composite Rating', x= '') + ylim(0, 100) +
  theme(axis.title.y=element_text(size=16), 
        axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12)
  ) 

ggplot(df_plot2, aes(x = composite_rating, y = `Film Type`, fill = `Film Type`, color = `Film Type`)) +
  geom_density_ridges(alpha = 0.6) +
  theme_ridges() + 
  theme(legend.position = "none") +
  labs(y = element_blank(), x = 'Composite Rating') +
  scale_color_manual(values=c("lightblue2", "orange2")) + 
  scale_fill_manual(values=c("lightskyblue", "orangered")) + xlim(0, 100)


