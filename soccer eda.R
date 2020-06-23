# starting with some EDA to decide on what variable we want to predict and what covariates might be 

library(tidyverse)

soccer_qunant <- womens_ncaa_soccer_20182019 %>% 
  select(team,
         team_games,
         assists_gp,
         fouls_gp,
         points_gp,
         save_pct,
         saves_gp,
         gpg,
         sog_pct,
         win_pct,
         sog_gp,
         season)

soccer_qunant %>% 
  select(assists_gp,
         fouls_gp,
         pk_pct,
         points_gp,
         save_pct,
         saves_gp,
         gpg,
         sog_pct,
         win_pct,
         sog_gp) %>% 
  cor()

womens_ncaa_soccer_20182019 %>% 
  select(win_pct, 
         points_gp,
         shots_gp,
         saves_gp) %>% 
  pairs()

womens_ncaa_soccer_20182019 %>% 
  ggplot(aes(x  = points_gp,
             y = win_pct)) +
  geom_point()

womens_ncaa_soccer_20182019 %>% 
  ggplot(aes(x  = shots_gp,
             y = win_pct)) +
  geom_point()

womens_ncaa_soccer_20182019 %>% 
  ggplot(aes(x  = saves_gp,
             y = win_pct)) +
  geom_point()

womens_ncaa_soccer_20182019 %>% 
  ggplot(aes(x  = assists_gp,
             y = win_pct)) +
  geom_point()

womens_ncaa_soccer_20182019 %>% 
  ggplot(aes(x  = fouls_gp,
             y = win_pct)) +
  geom_point()

womens_ncaa_soccer_20182019 %>% 
  select(win_pct,
         points_gp,
         shots_gp,
         saves_gp,
         assists_gp,
         fouls_gp,
         sog_gp,
         ga) %>% 
  cor()

womens_ncaa_soccer_20182019 %>% 
  ggplot(aes(x = pk_pct,
             y = win_pct,
             color = as.factor(season))) +
  geom_point()

hist(womens_ncaa_soccer_20182019$fouls)

hist(womens_ncaa_soccer_20182019$psatt)


womens_ncaa_soccer_20182019 %>% select_if(is.numeric) %>% cor() %>% View()


#starting to fit regression

#starting by including all predictors that have a correlation of more than 0.7

init_fit <- lm(win_pct ~ assists_gp + gaa + points_gp + 
                 gpg + shots_gp + sog_gp,
               data = womens_ncaa_soccer_20182019)
summary(init_fit)

plot(init_fit$residuals~womens_ncaa_soccer_20182019$assists_gp, color = womens_ncaa_soccer_20182019$season)
plot(init_fit$residuals~womens_ncaa_soccer_20182019$points_gp)
plot(init_fit$residuals~womens_ncaa_soccer_20182019$sog_gp)
plot(init_fit$residuals~womens_ncaa_soccer_20182019$shots_gp)


#removing highest p-value
second_fit <- lm(win_pct ~ assists_gp + gaa + points_gp + shots_gp + sog_gp,
               data = womens_ncaa_soccer_20182019)
summary(second_fit)

#removing highest p-value
third_fit <- lm(win_pct ~ assists_gp + gaa + points_gp + sog_gp,
                 data = womens_ncaa_soccer_20182019)
summary(third_fit)

#remove goals against - too obvious
fourth_fit <- lm(win_pct ~ assists_gp + points_gp + sog_gp,
                 data = womens_ncaa_soccer_20182019)
summary(fourth_fit)

experimental_fit <- lm(win_pct ~ assists_gp + points_gp + 
                         sog_gp + assists_gp*points_gp,
                 data = womens_ncaa_soccer_20182019)
summary(experimental_fit)
plot(experimental_fit)

interaction_fit <- lm(win_pct~ assists_gp*points_gp*sog_gp, 
                      data = womens_ncaa_soccer_20182019)
summary(interaction_fit)

#create training and testing sets
womens_ncaa_soccer_2018 <- womens_ncaa_soccer_20182019 %>% 
  filter(season == 2018)

womens_ncaa_soccer_2019 <- womens_ncaa_soccer_20182019 %>% 
  filter(season == 2019)

#train on 2018 data
candidate_model_1 <- lm(win_pct ~ assists_gp + points_gp + sog_gp,
                data = womens_ncaa_soccer_2018)
summary(candidate_model_1)
plot(candidate_model_1)

plot(residuals)
plot(fourth_fit$residuals~womens_ncaa_soccer_20182019$assists_gp)
plot(fourth_fit$residuals~womens_ncaa_soccer_20182019$points_gp)
plot(fourth_fit$residuals~womens_ncaa_soccer_20182019$sog_gp)

model_1_preds <- predict(candidate_model_1, newdata = womens_ncaa_soccer_2019)
model_1_mse <- mean((model_1_preds - womens_ncaa_soccer_2019$win_pct)^2)
model_1_mse

plot(win_pct~assists_gp, data = womens_ncaa_soccer_20182019)
plot(assists_gp ~ points_gp, data = womens_ncaa_soccer_20182019)
plot(corners_gp~assists_gp, data = womens_ncaa_soccer_20182019)

womens_ncaa_soccer_20182019 <- womens_ncaa_soccer_20182019 %>% 
  mutate(goal_diff = goals - ga)

caleb_fit <- lm(goal_diff ~ corners_gp + save_pct + pk_pct +
                  assists_gp + sog_gp,
                data = womens_ncaa_soccer_20182019)
summary(caleb_fit)
plot(caleb_fit)

nooutlier <- womens_ncaa_soccer_20182019 %>% 
  filter(team != "Alcorn" && team !="Chicago St.")
nooutlierfit <- lm(goal_diff ~ corners_gp + save_pct + pk_pct +
                     assists_gp + sog_gp,
                   data = womens_ncaa_soccer_20182019)
plot(nooutlierfit)


caleb_fit$fitted.values
