library(xgboost)

fitX <- model.matrix(~ 0 +
                     factor(pos) + year +
                     # Ensemble the sparse model here.
                     sparse.fr.hat +
                     age + height + weight +
                     forty + bench + vertical +
                     threecone + broad + shuttle +
                     games + seasons +
                     completions + attempts +
                     pass_yards + pass_ints + pass_tds + 
                     rec_yards + rec_td + receptions +
                     rush_att + rush_yds + rush_td +
                     solo_tackes + tackles + loss_tackles + ast_tackles +
                     fum_forced + fum_rec + fum_tds + fum_yds +
                     sacks + int + int_td + int_yards + pd +
                     punt_returns + punt_return_td + punt_return_yards +
                     kick_returns + kick_return_td + kick_return_yards
                     ,training)

b1.tuning <- expand.grid(depth = c(3, 4, 5, 6),
                         rounds = c(50, 100, 150, 200, 250)) %>%
  group_by(depth, rounds) %>%
  do({
    m <- xgboost(data = fitX[train.set,],
                 label = as.numeric(training$pick[train.set] <= 32),
                 max.depth = .$depth,
                 nround =.$rounds,
                 print.every.n = 50,
                 objective = 'binary:logistic')
    yhat <- predict(m, newdata = fitX)
    data_frame(test.set = test.set, yhat = yhat,
               label = as.numeric(training$pick <= 32))
  })

aucs <- b1.tuning %>%
  ungroup %>%
  filter(test.set) %>%
  group_by(depth, rounds) %>%
  do({
    auc <- performance(prediction(.$yhat, .$label), "auc")@y.values[[1]]
    data_frame(auc = auc)
  }) %>%
  ungroup %>%
  arrange(-auc)

best <- aucs %>% head(1)

# Fit the best model on the training data
b1.train <- xgboost(data = fitX[train.set,],
              label = first.round[train.set],
              max.depth = best$depth,
              nround = best$rounds,
              objective = "binary:logistic")

# Fit the best model on all the data
b1 <- xgboost(data = fitX[!holdout.set,],
              label = first.round[!holdout.set],
              max.depth = best$depth,
              nround = best$rounds,
              objective = "binary:logistic")

training$fr.hat <- predict(b1, newdata = fitX)
