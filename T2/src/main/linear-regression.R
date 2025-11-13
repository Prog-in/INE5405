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
if (!requireNamespace("gplots", quietly = TRUE)) { install.packages("gplots") }
library(gplots)

# --- Preparação dos dados ---
data <- read_csv("../resources/data.csv")

data <- data %>%
    filter(
        original_language == "en",
        str_detect(genre, regex("Horror", ignore_case = TRUE)),
        as.numeric(format(release_date, "%Y")) > 1980
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
#heatmap(cor_matrix, symm = TRUE, main = "Mapa de Calor das Correlações", key = TRUE)
heatmap.2(
    cor_matrix,
    symm = TRUE,
    trace = "none",
    col = colorRampPalette(c("white", "yellow", "orange", "brown"))(200),
    main = "Mapa de Calor das Correlações",
    key = TRUE,
    key.title = "Correlação",
    key.xlab = "Valor de r",
    density.info = "none"
)

# --- Modelos inicial e completo ---
MN <- lm(vote_average ~ 1, data = data)  # Modelo Nulo (apenas média)
MC <- lm(vote_average ~ popularity + vote_count + release_year +
             title_word_count + title_char_count,
         data = data)                     # Modelo Completo

# --- Seleção de modelos ---

# --- Diagnóstico e Análise Final ---
cat("\n### ANÁLISE FINAL DO MODELO COMPLETO ###\n")
print(summary(MC))

## Passo Atrás (Backward) - Critério AIC (k=2)
cat("\n### STEP BACKWARD (AIC) ###\n")
step_backward_AIC <- step(MC, direction = "backward", k = 2, trace = 1)

cat("\n### ANÁLISE FINAL DO MELHOR MODELO (AIC) ###\n")
print(summary(step_backward_AIC))

# --- Seleção de Modelo - Maior R² Ajustado ---
cat("\n### SELEÇÃO PELO R2 AJUSTADO ###\n")
selr2 <- with(data,
              leaps(x = cbind(popularity, vote_count, release_year,
                              title_word_count, title_char_count),
                    y = vote_average,
                    method = c("adjr2")))
nparr2 <- selr2$size
r2.aj <- selr2$adjr2
r2_results <- cbind(nparr2, r2.aj, selr2$which)[order(r2.aj, decreasing = TRUE),]
print(r2_results[1:5, ])  # Mostra os 5 melhores modelos

# --- Multicolinearidade ---
#cat("\n### VIF - Fator de Inflação da Variância ###\n")
#print(vif(step_backward_AIC))

# Teste de Normalidade dos Resíduos
# Teste de Shapiro-Wilk. H0: Os resíduos seguem uma distribuição Normal
cat("\n--- Teste de Normalidade dos Resíduos (Shapiro-Wilk) ---\n")
print(shapiro.test(residuals(step_backward_AIC)))

# Teste de Homogeneidade de Variâncias (Homocedasticidade)
# Teste de Breusch-Pagan. H0: Variância dos erros é constante (Homocedasticidade)
cat("\n--- Teste de Homocedasticidade (Breusch-Pagan) ---\n")
print(bptest(step_backward_AIC))

# Teste de Independência das Observações
# Teste de Durbin-Watson. H0: Resíduos são não correlacionados (Ausência de autocorrelação)
cat("\n--- Teste de Independência (Durbin-Watson) ---\n")
print(durbinWatsonTest(step_backward_AIC))

# --- Diagnóstico Gráfico ---
par(mfrow = c(2, 2))
plot(step_backward_AIC)
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
par(mfrow = c(1, 2))
plot(rstandard(step_backward_AIC),pch=19)
hist(residuals(step_backward_AIC))
#plot(predict(step_backward_AIC),residuals(step_backward_AIC),pch=19)
par(mfrow = c(1, 1))

# --- Teste de Hipótese (Conjunta - Igualdade) ---
cat("\n### Teste de Hipótese: H0: O efeito da Popularidade (log) é igual ao efeito da Contagem de Votos (log) ###\n")
# H0: beta_popularity - beta_vote_count = 0

#teste_hip <- linearHypothesis(
#    step_backward_AIC,
#    hypothesis.matrix = "popularity - vote_count = 0"
#)
teste_hip <- linearHypothesis(step_backward_AIC,
                 c("vote_count = 0", "release_year = 0"))
print(teste_hip)
