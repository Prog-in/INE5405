# Estraimos os dois vetores x e y:
dados <- read_csv("../resources/data.csv")
amostra_popularity <- dados$popularity # X
amostra_vote_avarage <- dados$vote_average # Y
# Teste 
cor.test(amostra_popularity,amostra_vote_avarage)



