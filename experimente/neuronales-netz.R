

dt <- sample(1:nrow(d), .7*nrow(d))
train <- d[dt,] # x Landkreise als Trainingsdatensatz
test <- d[-dt,] # x Landkreise als Testdatensatz

my.grid <- expand.grid(.decay = c(0.5, 0.1), .size = c(5, 6, 7))
prestige.fit <- train(income ~ prestige + education, data = d_clean,
                      method = "nnet", maxit = 1000, tuneGrid = my.grid, trace = F, linout = 1) 

prestige.predict <- predict(prestige.fit, newdata = prestige.test)
prestige.rmse <- sqrt(mean((prestige.predict - prestige.test$income)^2)) 