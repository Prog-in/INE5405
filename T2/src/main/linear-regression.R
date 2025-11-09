if (!requireNamespace("readr", quietly = TRUE)) { install.packages("readr") }
library(readr)
if (!requireNamespace("dplyr", quietly = TRUE)) { install.packages("dplyr") }
library(dplyr)
if (!requireNamespace("stringr", quietly = TRUE)) { install.packages("stringr") }
library(stringr)
if (!requireNamespace("leaps", quietly = TRUE)) { install.packages("leaps") }
library(leaps)
if (!requireNamespace("ggplot2", quietly = TRUE)) { install.packages("ggplot2") }
library(ggplot2)
if (!requireNamespace("car", quietly = TRUE)) { install.packages("car") }
library(car)
if (!requireNamespace("effects", quietly = TRUE)) { install.packages("effects") }
library(effects)
if (!requireNamespace("lmtest", quietly = TRUE)) { install.packages("lmtest") }
library(lmtest)
if (!requireNamespace("sandwich", quietly = TRUE)) { install.packages("sandwich") }
library(sandwich)

# --- Preparação dos dados ---
data <- read_csv("../resources/data.csv")

data <- data %>%
    filter(
        original_language == "en",
        str_detect(genre, regex("Horror", ignore_case = TRUE)),
    )

data <- data %>%
    mutate(
        release_year = as.numeric(format(release_date, "%Y")),
        title_word_count = str_count(title, "\\S+"),
        title_char_count = nchar(title),
        popularity = log(popularity + 1),
        vote_count = log(vote_count + 1),
    )

# --- Correlação e inspeção ---
cor_matrix <- cor(
    data %>% select(popularity, vote_count, release_year,
                    title_word_count, title_char_count, vote_average),
    use = "complete.obs"
)

# Visualização simples
#pairs(data[, c("vote_average", "popularity", "vote_count",
#               "release_year", "title_word_count", "title_char_count")],
#      main = "Matriz de Dispersão")
heatmap(cor_matrix, symm = TRUE, main = "Mapa de Calor das Correlações")

# --- Modelos inicial e completo ---
MN <- lm(vote_average ~ 1, data = data)  # Modelo Nulo (apenas média)
MC <- lm(vote_average ~ popularity + vote_count + release_year +
             title_word_count + title_char_count,
         data = data)                     # Modelo Completo

# --- Seleção de modelos ---

## Passo Atrás (Backward) - Critério AIC (k=2)
cat("\n### STEP BACKWARD (AIC) ###\n")
step_backward_AIC <- step(MC, direction = "backward", k = 2, trace = 1)

# ## Passo Atrás (Backward) - Critério BIC
# cat("\n### STEP BACKWARD (BIC) ###\n")
# step_backward_BIC <- step(MC, direction = "backward", k = log(nrow(data)), trace = 1)
#
# ## Passo à Frente (Forward) - Critério AIC (k=2)
# cat("\n### STEP FORWARD (AIC) ###\n")
# step_forward_AIC <- step(MN,
#                          scope = list(lower = MN, upper = MC),
#                          direction = "forward", k = 2, trace = 1)
#
# ## Passo à Frente (Forward) - Critério BIC
# cat("\n### STEP FORWARD (BIC) ###\n")
# step_forward_BIC <- step(MN,
#                          scope = list(lower = MN, upper = MC),
#                          direction = "forward", k = log(nrow(data)), trace = 1)
#
# ## Passo a Passo (Stepwise) - Critério AIC (k=2)
# cat("\n### STEPWISE (AIC) ###\n")
# step_both_AIC <- step(MC, direction = "both", k = 2, trace = 1)
#
# ## Passo a Passo (Stepwise) - Critério BIC
# cat("\n### STEPWISE (BIC) ###\n")
# step_both_BIC <- step(MC, direction = "both", k = log(nrow(data)), trace = 1)
#
# # --- Comparação dos Modelos ---
# cat("\n### COMPARAÇÃO DE MODELOS (AIC/BIC) ###\n")
# AIC(MC, step_backward_AIC, step_forward_AIC, step_both_AIC)
# BIC(MC, step_backward_BIC, step_forward_BIC, step_both_BIC)

# --- Seleção de Modelo - Maior R² Ajustado ---
cat("\n### SELEÇÃO PELO R² AJUSTADO ###\n")
selr2 <- with(data,
              leaps(x = cbind(popularity, vote_count, release_year,
                              title_word_count, title_char_count),
                    y = vote_average,
                    method = c("adjr2")))
nparr2 <- selr2$size
r2.aj <- selr2$adjr2
r2_results <- cbind(nparr2, r2.aj, selr2$which)[order(r2.aj, decreasing = TRUE),]
print(r2_results[1:10, ])  # Mostra os 10 melhores modelos

# --- Diagnóstico e Análise Final ---
cat("\n### ANÁLISE FINAL DO MODELO COMPLETO ###\n")
summary(step_backward_AIC)

cat("\n### ANÁLISE FINAL DO MELHOR MODELO (AIC) ###\n")
summary(step_backward_AIC)

# --- Multicolinearidade ---
cat("\n### VIF - Fator de Inflação da Variância ###\n")
print(vif(step_backward_AIC))

# Teste de Normalidade dos Resíduos
# Teste de Shapiro-Wilk. H0: Os resíduos seguem uma distribuição Normal
shapiro.test(residuals(step_backward_AIC))

# Teste de Homogeneidade de Variâncias (Homocedasticidade)
# Teste de Breusch-Pagan. H0: Variância dos erros é constante (Homocedasticidade)
cat("\n--- Teste de Homocedasticidade (Breusch-Pagan) ---\n")
print(bptest(step_backward_AIC))

# Teste de Independência das Observações
# Teste de Durbin-Watson. H0: Resíduos são não correlacionados (Ausência de autocorrelação)
cat("\n--- Teste de Independência (Durbin-Watson) ---\n")
print(durbinWatsonTest(step_backward_AIC))

cat("\n### Resultados utilizando HAC ###\n")
# O vcovHAC calcula a matriz de covariância robusta (HAC)
# O coeftest usa essa matriz para recalcular Erros Padrão e p-valores válidos.
resultados_robustos_hac <- coeftest(step_backward_AIC, vcov = vcovHAC(step_backward_AIC))
print(resultados_robustos_hac)

# --- Diagnóstico Gráfico ---
par(mfrow = c(2, 2))
plot(MC)
par(mfrow = c(1, 1))

# --- Efeitos Parciais ---
plot(allEffects(step_backward_AIC))

# --- Visualização: Valores Observados vs Previstos ---
data$predicted <- predict(step_backward_AIC)
ggplot(data, aes(x = predicted, y = vote_average)) +
    geom_point(color = "steelblue", alpha = 0.5) +
    geom_abline(slope = 1, intercept = 0,
                color = "red", linetype = "dashed") +
    labs(title = "Predito vs Observado - Vote Average",
         x = "Predito", y = "Observado") +
    theme_minimal()

# Análise de Resíduos
plot(rstandard(step_backward_AIC),pch=19)
hist(residuals(step_backward_AIC))
plot(predict(step_backward_AIC),residuals(step_backward_AIC),pch=19)

# --- Teste de Hipótese (Conjunta - Igualdade) ---
cat("\n### Teste de Hipótese: H0: O efeito da Popularidade (log) é igual ao efeito da Contagem de Votos (log) ###\n")
# H0: beta_popularity - beta_vote_count = 0

teste_hip <- linearHypothesis(
    step_backward_AIC,
    hypothesis.matrix = "popularity - vote_count = 0"
)
print(teste_hip)
