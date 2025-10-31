# 1. Carregamento dos Dados
# 1.a) 
# Usamos read.csv2() que é o padrão para arquivos com delimitador ';' e decimal ','
# O argumento fileEncoding="latin1" ou similar pode ser necessário dependendo do ambiente
# e do arquivo, mas tentaremos sem ele primeiro.

df_bgs <- read.csv2(
  file = "BGSgirls.csv",
  header = TRUE,
  sep = ";",
  dec = ","
  row.names = 1 # Usa a primeira coluna como identificador de linha
)

# Exploração da Estrutura dos Dados com str()
print("--- Estrutura do DataFrame (str()) ---")
str(df_bgs)

# Exploração das Primeiras Linhas com head()
print("\n--- Primeiras linhas do DataFrame (head()) ---")
head(df_bgs)

# 1.b)
# Certificando-se de que o DataFrame foi carregado corretamente
df_bgs <- read.csv2(
  file = "BGSgirls.csv",
  header = TRUE,
  sep = ";",
  dec = ",",
  row.names = 1 # Usa a primeira coluna como identificador de linha
)

# Definir as variáveis de interesse
variaveis <- c("HT18", "WT9")

# Calcular as medidas descritivas para cada variável
resultados <- sapply(df_bgs[variaveis], function(x) {
  c(
    Media = mean(x, na.rm = TRUE),
    Mediana = median(x, na.rm = TRUE),
    Desvio_Padrao = sd(x, na.rm = TRUE),
    Minimo = min(x, na.rm = TRUE),
    Maximo = max(x, na.rm = TRUE)
  )
})

# Exibir os resultados
print("--- Medidas Descritivas para HT18 e WT9 ---")
print(resultados)

#1.c)
# GRÁFICO DE DISPERSÃO (HT18 vs. WT9)

# Define o título e os rótulos
titulo_disp <- "Relação entre Altura aos 18 anos (HT18) e Peso aos 9 anos (WT9)"
eixo_x_disp <- "Peso aos 9 anos (WT9) [kg]"
eixo_y_disp <- "Altura aos 18 anos (HT18) [cm]"

# Constrói o gráfico de dispersão
plot(
  x = df_bgs$WT9, 
  y = df_bgs$HT18, 
  main = titulo_disp,
  xlab = eixo_x_disp,
  ylab = eixo_y_disp,
  pch = 19, # Tipo de ponto (círculo sólido)
  col = "blue" # Cor dos pontos
)

# HISTOGRAMA PARA HT18

# Define o título e os rótulos
titulo_hist_ht18 <- "Histograma da Altura das Meninas aos 18 anos (HT18)"
eixo_x_hist_ht18 <- "Altura (cm)"

# Constrói o histograma para HT18
hist(
  df_bgs$HT18,
  main = titulo_hist_ht18,
  xlab = eixo_x_hist_ht18,
  ylab = "Frequência",
  col = "lightblue", 
  border = "darkblue"
)


# HISTOGRAMA PARA WT9

# Define o título e os rótulos
titulo_hist_wt9 <- "Histograma do Peso das Meninas aos 9 anos (WT9)"
eixo_x_hist_wt9 <- "Peso (kg)"

# Constrói o histograma para WT9
hist(
  df_bgs$WT9,
  main = titulo_hist_wt9,
  xlab = eixo_x_hist_wt9,
  ylab = "Frequência",
  col = "lightgreen",
  border = "darkgreen"
)

# 1.d)
# Resposta:A variável HT18 é bastante homogênea (baixo desvio padrão e distribuição Normal), indicando que, apesar das diferenças de peso na infância, a altura final do grupo é consistente. 
  A variável WT9 apresenta maior dispersão, o que é esperado, pois o peso na infância é mais sensível a fatores ambientais, dietéticos e de desenvolvimento individual. 
  O peso aos 9 anos (WT9) é um preditor fraco ou moderado da altura final (HT18). A correlação não será perfeita, pois a altura final é um resultado genético complexo, e o peso aos 9 anos reflete o estágio de desenvolvimento atual e o status nutricional.


# 2. Calculos Manuais das Estimativas
# 2.a)
# Montar a matriz de design X
# Adicionar a coluna de 1s para o intercepto e selecionar as variáveis preditoras
# A fórmula do modelo é: HT18 ~ 1 + HT2 + WT2 + HT9 + WT9 + ST9
# O '1' garante a coluna do intercepto.
X <- model.matrix( ~ HT2 + WT2 + HT9 + WT9 + ST9, data = df_bgs)

# Exibir a estrutura e as primeiras 10 linhas
print("--- Estrutura da Matriz de Design X ---")
str(X)

print("\n--- Primeiras 10 linhas da Matriz de Design X ---")
head(X, n = 10)

# 2.b)
# Vetor da Variável Resposta Y (HT18)
Y <- as.matrix(df_bgs$HT18)

# Componentes da Fórmula Matricial

# Transposta de X (X')
Xt <- t(X)

# Produto X'X (X' %*% X)
XtX <- Xt %*% X

# Inversa de X'X
# O solve() calcula a inversa da matriz
XtX_inv <- solve(XtX)

# Produto X'Y (X' %*% Y)
XtY <- Xt %*% Y

# Cálculo dos Coeficientes Estimados (Beta chapéu)
# Beta_chapeu = (X'X)^-1 %*% X'Y
beta_chapeu <- XtX_inv %*% XtY

# Exibir os resultados
print("--- Coeficientes do Modelo (Beta Chapéu) Calculados por Matrizes ---")
print(beta_chapeu)

# 2.c)
# Cálculo dos Coeficientes usando a função lm()
# A função lm() estima o mesmo modelo
modelo_lm <- lm(HT18 ~ HT2 + WT2 + HT9 + WT9 + ST9, data = df_bgs)
beta_chapeu_lm <- coef(modelo_lm)

# Comparação dos Resultados
print("--- Comparação dos Coeficientes Estimados ---")
print("Cálculo Manual (Matricial):")
print(beta_chapeu)

print("\nCálculo R (função lm()):")
print(beta_chapeu_lm)

# 3.a) Valores Preditos e Resíduos

# Y_pred Valores preditos pelo modelo

Y_pred <- X %*% beta_chapeu

residuos <- Y - Y_pred


print("--- Os primeiros 10 Valores Preditos (Y_pred) ---")
head(Y_pred, n = 10)

print("\n--- Primeiros 10 Resíduos (e) ---")
head(residuos, n = 10)

#3.b) 
#A média de Y 
Y_media <- mean(Y)

#1.Soma dos Quadrados Total 

SQ_Total <- sum((Y - Y_media)^2)

#2 (SQ_Residuos)
#(Y - Y_pred)^2 = (residuos)^2

SQ_Residuos <- sum(residuos^2)

# SQ_Regressao:

SQ_Regressao <- sum((Y_pred - Y_media)^2)

# Resultados:
print(paste("SQ_Total:", SQ_Total))
print(paste("SQ_Regressao:", SQ_Regressao))
print(paste("SQ_Residuos:", SQ_Residuos))

# Sumário 
print("\n--- Sumário Completo do Modelo lm() ---")
summary(modelo_lm)

# 3.c) Verificação da Decomposição da Variância

# Soma da Regressão e dos Resíduos
soma_componentes <- SQ_Regressao + SQ_Residuos

print(paste("SQ_Total (calculado):", SQ_Total))
print(paste("SQ_Regressao + SQ_Residuos (soma):", soma_componentes))

# Usamos all.equal() para comparar números de ponto flutuante (evita erros de arredondamento)
verificacao <- all.equal(SQ_Total, soma_componentes)

print(paste("A identidade SQ_Total = SQ_Regressao + SQ_Residuos é verdadeira?", verificacao))

# 4.a Cálculo do R-quadrado 

R_quadrado <- SQ_Regressao / SQ_Total

print(paste("R² (Manual):", R_quadrado))


# 4.b)R-quadrado Ajustado (R²_adj)

# n (número de observações) e p (número de parâmetros)
n <- nrow(X) 
p <- ncol(X) 

# Aplicando a fórmula
R_quadrado_ajustado <- 1 - (((1 - R_quadrado) * (n - 1)) / (n - p))

print(paste("Número de observações (n):", n))
print(paste("Número de parâmetros (p):", p))
print(paste("R² Ajustado (Manual):", R_quadrado_ajustado))


