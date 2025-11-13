# 1. Definir os dados com base na sua análise
# Proporção de filmes com média > 8
prop_maior_8 <- 0.0185659411011524

# Proporção do restante (média <= 8)
prop_restante <- 1 - prop_maior_8

# Juntar os valores em um vetor
valores <- c(prop_maior_8, prop_restante)

# 2. Definir os rótulos (labels)
# Calcular porcentagens formatadas para os rótulos
pct_maior_8 <- round(prop_maior_8 * 100, 2)
pct_restante <- round(prop_restante * 100, 2)

# Criar os rótulos de texto
rotulos <- c(paste("Média > 8 (", pct_maior_8, "%)", sep=""),
             paste("Média <= 8 (", pct_restante, "%)", sep=""))

# 3. Definir cores
# (Usando cores mais distintas para destacar a fatia pequena)
cores <- c("deepskyblue", "lightgray")

# 4. Salvar o gráfico em um arquivo PNG
# Isso cria o arquivo "grafico_proporcao_filmes.png" no seu diretório de trabalho
png("grafico_proporcao_filmes.png", width = 800, height = 600, res = 100)

# 5. Criar o gráfico de pizza
pie(valores, 
    labels = rotulos, 
    col = cores, 
    main = "Proporção de Filmes em Inglês por Nota Média",
    init.angle = 90,  # Inicia a primeira fatia (Média > 8) no topo
    border = "white"  # Adiciona uma borda branca entre as fatias
)

# 6. Fechar o dispositivo gráfico (importante para salvar o arquivo)
dev.off()

print("Gráfico salvo como 'grafico_proporcao_filmes.png'")
