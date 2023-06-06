### calcul des vraies valeurs associées aux données simulées

param.causal.model <- function(p_L1 = 0.50, 
                               p_L2 = 0.20, 
                               p_L3 = 0.70,       # baseline confounders
                               b_A1 = 0.10, 
                               b_L1_A1 = 0.15, 
                               b_L2_A1 = 0.25, # modèle de A1
                               b_A2 = 0.15, 
                               b_L1_A2 = 0.20, 
                               b_L3_A2 = 0.20, # modèle de A2
                               b_Y = 0.10,      # modèle de Y
                               b_L1_Y = 0.02,
                               b_L2_Y = 0.02,
                               b_L3_Y = -0.02,
                               b_A1_Y = 0.3,
                               b_A2_Y = 0.1,
                               b_A1A2_Y = 0.4 ) { # <- effet d'interaction Delta)
  
  # coefficients pour simuler l'exposition
  # exposition A1  # vérif
  try(if(b_A1 + b_L1_A1 + b_L1_A1 > 1) 
    stop("la somme des coefficient du modèle A1 dépasse 100%"))
  
  # exposition A2  # vérif
  try(if(b_A2 + b_L1_A2 + b_L3_A2 > 1) 
    stop("la somme des coefficients du modèle A2 dépasse 100%"))
  
  # coefficients pour simuler l'outcome, vérif
  try(if(b_Y + b_L1_Y + b_L2_Y + b_L3_Y + b_A1_Y + b_A2_Y + b_A1A2_Y > 1) 
    stop("la somme des coefficients du modèle Y dépasse 100%"))
  try(if(b_Y + b_L1_Y + b_L2_Y + b_L3_Y + b_A1_Y + b_A2_Y + b_A1A2_Y < 0) 
    stop("la somme des coefficients du modèle Y est inférieure à 0%"))
  
  coef <- list(c(p_L1 = p_L1, p_L2 = p_L2, p_L3 = p_L3),
               c(b_A1 = b_A1, b_L1_A1 = b_L1_A1, b_L2_A1 = b_L2_A1),
               c(b_A2 = b_A2, b_L1_A2 = b_L1_A2, b_L3_A2 = b_L3_A2),
               c(b_Y = b_Y, b_L1_Y = b_L1_Y, b_L2_Y = b_L2_Y, b_L3_Y = b_L3_Y,
                 b_A1_Y = b_A1_Y, b_A2_Y = b_A2_Y, b_A1A2_Y = b_A1A2_Y))
  return(coef)
}

b <- param.causal.model()

### calcul des "vraies" valeurs
S <- cbind(expand.grid(c(0,1),c(0,1),c(0,1)), rep(NA,n=3^2))
colnames(S) <- list("L1","L2","L3","sum")
for (k in 1:8) {
  S[k,"sum"] <- (b[[4]]["b_Y"] + 
                   b[[4]]["b_L1_Y"] * S[k,"L1"] + 
                   b[[4]]["b_L2_Y"] * S[k,"L2"] + 
                   b[[4]]["b_L3_Y"] * S[k,"L3"] + 
                   b[[4]]["b_A1_Y"] * 0 + 
                   b[[4]]["b_A2_Y"] * 0 + 
                   b[[4]]["b_A1A2_Y"] * 0 * 0 ) *
    (b[[1]]["p_L1"]^S[k,"L1"]) *
    ((1 - b[[1]]["p_L1"])^(1-S[k,"L1"])) *
    (b[[1]]["p_L2"]^S[k,"L2"]) *
    ((1 - b[[1]]["p_L2"])^(1-S[k,"L2"])) *
    (b[[1]]["p_L3"]^S[k,"L3"]) *
    ((1 - b[[1]]["p_L3"])^(1-S[k,"L3"]))
}
Psi.00 <- sum(S[,"sum"]) # [1] 0.1

for (k in 1:8) {
  S[k,"sum"] <- (b[[4]]["b_Y"] + 
                   b[[4]]["b_L1_Y"] * S[k,"L1"] + 
                   b[[4]]["b_L2_Y"] * S[k,"L2"] + 
                   b[[4]]["b_L3_Y"] * S[k,"L3"] + 
                   b[[4]]["b_A1_Y"] * 1 + 
                   b[[4]]["b_A2_Y"] * 0 + 
                   b[[4]]["b_A1A2_Y"] * 1 * 0 ) *
    (b[[1]]["p_L1"]^S[k,"L1"]) *
    ((1 - b[[1]]["p_L1"])^(1-S[k,"L1"])) *
    (b[[1]]["p_L2"]^S[k,"L2"]) *
    ((1 - b[[1]]["p_L2"])^(1-S[k,"L2"])) *
    (b[[1]]["p_L3"]^S[k,"L3"]) *
    ((1 - b[[1]]["p_L3"])^(1-S[k,"L3"]))
}
Psi.10 <- sum(S[,"sum"]) # [1] 0.4

for (k in 1:8) {
  S[k,"sum"] <- (b[[4]]["b_Y"] + 
                   b[[4]]["b_L1_Y"] * S[k,"L1"] + 
                   b[[4]]["b_L2_Y"] * S[k,"L2"] + 
                   b[[4]]["b_L3_Y"] * S[k,"L3"] + 
                   b[[4]]["b_A1_Y"] * 0 + 
                   b[[4]]["b_A2_Y"] * 1 + 
                   b[[4]]["b_A1A2_Y"] * 0 * 1 ) *
    (b[[1]]["p_L1"]^S[k,"L1"]) *
    ((1 - b[[1]]["p_L1"])^(1-S[k,"L1"])) *
    (b[[1]]["p_L2"]^S[k,"L2"]) *
    ((1 - b[[1]]["p_L2"])^(1-S[k,"L2"])) *
    (b[[1]]["p_L3"]^S[k,"L3"]) *
    ((1 - b[[1]]["p_L3"])^(1-S[k,"L3"]))
}
Psi.01 <- sum(S[,"sum"]) # [1] 0.2

for (k in 1:8) {
  S[k,"sum"] <- (b[[4]]["b_Y"] + 
                   b[[4]]["b_L1_Y"] * S[k,"L1"] + 
                   b[[4]]["b_L2_Y"] * S[k,"L2"] + 
                   b[[4]]["b_L3_Y"] * S[k,"L3"] + 
                   b[[4]]["b_A1_Y"] * 1 + 
                   b[[4]]["b_A2_Y"] * 1 + 
                   b[[4]]["b_A1A2_Y"] * 1 * 1 ) *
    (b[[1]]["p_L1"]^S[k,"L1"]) *
    ((1 - b[[1]]["p_L1"])^(1-S[k,"L1"])) *
    (b[[1]]["p_L2"]^S[k,"L2"]) *
    ((1 - b[[1]]["p_L2"])^(1-S[k,"L2"])) *
    (b[[1]]["p_L3"]^S[k,"L3"]) *
    ((1 - b[[1]]["p_L3"])^(1-S[k,"L3"]))
}
Psi.11 <- sum(S[,"sum"]) # [1] 0.9

## OK, c'est bien ça !

#### performances d'un modèle linéaire (non biaisé) par rapport à un modèle logistique (un peu biaisé)
generate.data <- function(N, b =  param.causal.model()) {
  
  L1 <- rbinom(N, size = 1, prob = b[[1]]["p_L1"]) 
  L2 <- rbinom(N, size = 1, prob = b[[1]]["p_L2"])
  L3 <- rbinom(N, size = 1, prob = b[[1]]["p_L3"])
  A1 <- rbinom(N, size = 1, prob = b[[2]]["b_A1"] + 
                 (b[[2]]["b_L1_A1"] * L1) + (b[[2]]["b_L2_A1"] * L2))
  A2 <- rbinom(N, size = 1, prob = b[[3]]["b_A2"] + 
                 (b[[3]]["b_L1_A2"] * L1) + (b[[3]]["b_L3_A2"] * L3))
  Y <- rbinom(N, size = 1, prob = (b[[4]]["b_Y"] + 
                                     (b[[4]]["b_L1_Y"] * L1) + 
                                     (b[[4]]["b_L2_Y"] * L2) +
                                     (b[[4]]["b_L3_Y"] * L3) +  
                                     (b[[4]]["b_A1_Y"] * A1) + 
                                     (b[[4]]["b_A2_Y"] * A2) + 
                                     (b[[4]]["b_A1A2_Y"] * A1 * A2)) )
  data.sim <- data.frame(L1, L2, L3, A1, A2, Y)
  return(data.sim)
}

library(SuperLearner)

set.seed(12345)
b = param.causal.model()
df <- generate.data(N = 10000, b = b)
summary(df)

SL.lm.step.interaction <- function (Y, X, newX, family, direction = "both", trace = 0, k = 2, 
                                    obsWeights, model = TRUE, ...) {
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }
  fit.lm <- stats::lm(Y ~ ., data = X, weights = obsWeights, model = model) # 
  fit.step <- step(fit.lm, scope = Y ~ .^2, direction = direction, 
                   trace = trace, k = k)
  pred <- predict(fit.step, newdata = newX, type = "response")
  if (family$family == "binomial") {
    pred = pmin(pmax(pred, 0), 1)
  }
  fit <- list(object = fit.step)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.step")
  return(out)
}


sl <- SuperLearner(Y = df$Y,
                   X = subset(df, select = -c(Y)), 
                   family = binomial(),
                   SL.library = c("SL.mean", "SL.step.interaction", "SL.lm.step.interaction"))
sl
#                                 Risk        Coef
# SL.mean_All                0.1827797 0.002784409
# SL.step.interaction_All    0.1309858 0.000000000
# SL.lm.step.interaction_All 0.1308922 0.997215591  # il retient bien le modèle linéaire plutôt que la régression logistique
                                                    # mais l'écart de performances entre les deux est assez faible

sl$fitLibrary$SL.lm.step.interaction_All
# Call:
#   stats::lm(formula = Y ~ L1 + L2 + L3 + A1 + A2 + A1:A2, data = X)
# Coefficients:
#   (Intercept)           L1           L2           L3           A1           A2        A1:A2  
#       0.10383      0.02068      0.01948     -0.01957      0.30080      0.09228      0.39403  
# il trouve bien le bon modèle avec uniquement l'interaction A1*A2

sl$fitLibrary$SL.step.interaction_All
# Call:  glm(formula = Y ~ L1 + L2 + L3 + A1 + A2 + A1:A2 + L1:A1 + L3:A1, 
#            family = family, data = X)
# Coefficients:
#   (Intercept)           L1           L2           L3           A1           A2        A1:A2        L1:A1        L3:A1  
#       -2.1508       0.2143       0.1342      -0.2111       1.7445       0.7543       1.7824      -0.1996       0.1968  
# en régression logistique, il ajoute une interaction L1*A1 et L3*A1, en plus de l'interaction A1*A2
