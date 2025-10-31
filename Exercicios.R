# 1. Carregamento dos Dados
# 1.a) 
# Usamos read.csv2() que é o padrão para arquivos com delimitador ';' e decimal ','
# O argumento fileEncoding="latin1" ou similar pode ser necessário dependendo do ambiente
# e do arquivo, mas tentaremos sem ele primeiro.

cat("\n=== Exercício: 1-a ===\n")

df_bgs <- read.csv2(
  file = "BGSgirls.csv",
  header = TRUE,
  sep = ";",
  dec = ",",
  row.names = 1 # Usa a primeira coluna como identificador de linha
)

# Exploração da Estrutura dos Dados com str()
print("\n --- Estrutura do DataFrame (str()) ---\n ")
str(df_bgs)

# Exploração das Primeiras Linhas com head()
print("\n--- Primeiras linhas do DataFrame (head()) ---")
head(df_bgs)

# 1.b)

cat("\n=== Exercício: 1-b ===\n")

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

cat("\n=== Exercício: 1-c ===\n")

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
#  A variável WT9 apresenta maior dispersão, o que é esperado, pois o peso na infância é mais sensível a fatores ambientais, dietéticos e de desenvolvimento individual. 
#  O peso aos 9 anos (WT9) é um preditor fraco ou moderado da altura final (HT18). A correlação não será perfeita, pois a altura final é um resultado genético complexo, e o peso aos 9 anos reflete o estágio de desenvolvimento atual e o status nutricional.


# 2. Calculos Manuais das Estimativas
# 2.a)
# Montar a matriz de design X
# Adicionar a coluna de 1s para o intercepto e selecionar as variáveis preditoras
# A fórmula do modelo é: HT18 ~ 1 + HT2 + WT2 + HT9 + WT9 + ST9
# O '1' garante a coluna do intercepto.

cat("\n=== Exercício: 2-a ===\n")

X <- model.matrix( ~ HT2 + WT2 + HT9 + WT9 + ST9, data = df_bgs)

# Exibir a estrutura e as primeiras 10 linhas
print("--- Estrutura da Matriz de Design X ---")
str(X)

print("\n--- Primeiras 10 linhas da Matriz de Design X ---")
head(X, n = 10)

# 2.b)

cat("\n=== Exercício: 2-b ===\n")

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

cat("\n=== Exercício: 2-c ===\n")

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

cat("\n=== Exercício: 3-a ===\n")

# Y_pred Valores preditos pelo modelo

Y_pred <- X %*% beta_chapeu

residuos <- Y - Y_pred


print("--- Os primeiros 10 Valores Preditos (Y_pred) ---")
head(Y_pred, n = 10)

print("\n--- Primeiros 10 Resíduos (e) ---")
head(residuos, n = 10)

#3.b) 

cat("\n=== Exercício: 3-b ===\n")

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

cat("\n=== Exercício: 3-c ===\n")

# Soma da Regressão e dos Resíduos
soma_componentes <- SQ_Regressao + SQ_Residuos

print(paste("SQ_Total (calculado):", SQ_Total))
print(paste("SQ_Regressao + SQ_Residuos (soma):", soma_componentes))

# Usamos all.equal() para comparar números de ponto flutuante (evita erros de arredondamento)
verificacao <- all.equal(SQ_Total, soma_componentes)

print(paste("A identidade SQ_Total = SQ_Regressao + SQ_Residuos é verdadeira?", verificacao))

# 4.a Cálculo do R-quadrado 

cat("\n=== Exercício: 4-a ===\n")

R_quadrado <- SQ_Regressao / SQ_Total

print(paste("R² (Manual):", R_quadrado))


# 4.b)R-quadrado Ajustado (R²_adj)

cat("\n=== Exercício: 4-b ===\n")

# n (número de observações) e p (número de parâmetros)
n <- nrow(X) 
p <- ncol(X) 

# Aplicando a fórmula
R_quadrado_ajustado <- 1 - (((1 - R_quadrado) * (n - 1)) / (n - p))

print(paste("Número de observações (n):", n))
print(paste("Número de parâmetros (p):", p))
print(paste("R² Ajustado (Manual):", R_quadrado_ajustado))


# 5.a) QM_Res e Matriz de Covariâncias

cat("\n=== Exercício: 5-a ===\n")

# 1. Calcular o Quadrado Médio dos Resíduos (Estimativa de sigma^2)
# Usamos SQ_Residuos, n, e p que já calculamos nos exercícios 3 e 4.
graus_liberdade_res <- n - p
qm_res <- SQ_Residuos / graus_liberdade_res

print(paste("Quadrado Médio dos Resíduos (QM_Res):", qm_res))

# 2. Calcular a matriz (X^T X)^-1
# Nós já calculamos a inversa em 2.b) para achar os betas.
# O nome da variável no seu script é XtX_inv
print("--- Matriz (X^T X)^-1 ---")
print(XtX_inv)

# 3. Calcular a Matriz de Variâncias-Covariâncias de Beta
# A fórmula é: Var(Beta) = QM_Res * (X^T X)^-1
var_cov_matrix <- qm_res * XtX_inv

print("--- Matriz de Variâncias-Covariâncias dos Coeficientes ---")
print(var_cov_matrix)

# 5.b) Teste t para cada coeficiente

cat("\n=== Exercício: 5-b ===\n")

# 1. Extrair os Erros Padrão (Standard Errors - SE)
# O SE de cada beta é a raiz quadrada da diagonal principal da matriz de covariâncias
se_betas <- sqrt(diag(var_cov_matrix))

print("--- Erros Padrão (SE) dos Coeficientes ---")
print(se_betas)

# 2. Calcular os valores t (t-calculado)
# A fórmula é t = (beta_j - 0) / SE(beta_j)
t_valores <- beta_chapeu / se_betas

print("--- Valores t Calculados ---")
print(t_valores)

# 3. Encontrar o valor t crítico (t-tabelado)
# Para um teste bicaudal com alfa = 0.05 
# Usamos qt(0.975) pois 0.025 fica em cada cauda.
alfa <- 0.05
t_critico <- qt(1 - (alfa / 2), df = graus_liberdade_res)

print(paste("Valor t-crítico para alfa=0.05:", t_critico))

# 5.c) Intervalos de Confiança de 95% para os coeficientes

cat("\n=== Exercício: 5-c ===\n")

# 1. Calcular a Margem de Erro
# A fórmula é: t_critico * SE(beta_j)
margem_erro <- t_critico * se_betas

# 2. Calcular os limites do IC
ic_limite_inferior <- beta_chapeu - margem_erro
ic_limite_superior <- beta_chapeu + margem_erro

# 3. Juntar tudo em um data.frame para facilitar a leitura
# (Usamos as.vector() para garantir que os nomes das linhas de 'beta_chapeu' sejam usados)
resultados_coeficientes <- data.frame(
  Variavel = rownames(beta_chapeu),
  Coeficiente = as.vector(beta_chapeu),
  Erro_Padrao = se_betas,
  t_Valor_Calc = t_valores,
  IC_Inferior_95 = ic_limite_inferior,
  IC_Superior_95 = ic_limite_superior
)

print("--- Tabela de Coeficientes, Teste t e IC 95% (Manual) ---")
print(resultados_coeficientes)

# Vamos também imprimir os resultados da função lm() para comparar
print("\n--- Comparação com a Saída Padrão do R (summary) ---")
print(summary(modelo_lm)$coefficients)

# 5.d) Interpretação: Quais variáveis são significativas?

cat("\n=== Exercício: 5-d ===\n")

# Uma variável é significativa se o valor 0 (zero) NÃO estiver
# dentro do seu intervalo de confiança (IC_Inferior_95 a IC_Superior_95).
# Ou, de forma equivalente, se |t_Valor_Calc| > t_critico.

print("--- Análise de Significância (alfa = 0.05) ---")

# Usamos a tabela que criamos
for (i in 1:nrow(resultados_coeficientes)) {
  var_nome <- resultados_coeficientes$Variavel[i]
  ic_inf <- resultados_coeficientes$IC_Inferior_95[i]
  ic_sup <- resultados_coeficientes$IC_Superior_95[i]
  
  # Checamos se 0 está no intervalo
  if (ic_inf < 0 && ic_sup > 0) {
    print(paste("Variável", var_nome, "NÃO é significativa (Intervalo [", round(ic_inf, 4), ";", round(ic_sup, 4), "] contém 0)"))
  } else {
    print(paste("Variável", var_nome, "É SIGNIFICATIVA (Intervalo [", round(ic_inf, 4), ";", round(ic_sup, 4), "] NÃO contém 0)"))
  }
}

# 6.a) Cálculos Manuais para uma nova observação

cat("\n=== Exercício: 6-a ===\n")

# 1. Criar o vetor da nova observação (x_0)
# Este vetor PRECISA ter a mesma estrutura da matriz X,
# incluindo o 1 para o intercepto.
# Ordem: (Intercept), HT2, WT2, HT9, WT9, ST9
x_0 <- matrix(c(1, 90, 12, 135, 30, 20), ncol = 1)

# Precisamos também da versão transposta (1x6) para os cálculos
x0_t <- t(x_0)

# 2. Calcular o Valor Predito (y_pred_nova) 
# Fórmula: y_0 = x_0^T * beta_chapeu
y_pred_nova <- x0_t %*% beta_chapeu

print(paste("Valor Predito para a nova menina:", y_pred_nova))

# 3. Calcular o Intervalo de Confiança de 95% para a média 
# Fórmula: y_0 ± t_critico * sqrt(QM_Res * (x_0^T * (X^T*X)^-1 * x_0))

# 3.1. Calcular o Erro Padrão da Média (SE_Conf)
# (Usamos XtX_inv, qm_res, e t_critico dos exercícios anteriores)
se_conf_parte_interna <- x0_t %*% XtX_inv %*% x_0
se_conf <- sqrt(qm_res * se_conf_parte_interna)

# 3.2. Calcular a Margem de Erro
margem_erro_conf <- t_critico * se_conf

# 3.3. Calcular o IC de Confiança
ic_conf_inf <- y_pred_nova - margem_erro_conf
ic_conf_sup <- y_pred_nova + margem_erro_conf

print(paste("IC de 95% para a Média: [", round(ic_conf_inf, 4), ";", round(ic_conf_sup, 4), "]"))


# 4. Calcular o Intervalo de Predição de 95% para o individual 
# Fórmula: y_0 ± t_critico * sqrt(QM_Res * (1 + x_0^T * (X^T*X)^-1 * x_0))

# 4.1. Calcular o Erro Padrão da Predição (SE_Pred)
# Note o "1 + " dentro da raiz
se_pred_parte_interna <- 1 + se_conf_parte_interna
se_pred <- sqrt(qm_res * se_pred_parte_interna)

# 4.2. Calcular a Margem de Erro
margem_erro_pred <- t_critico * se_pred

# 4.3. Calcular o IC de Predição
ic_pred_inf <- y_pred_nova - margem_erro_pred
ic_pred_sup <- y_pred_nova + margem_erro_pred

print(paste("IP de 95% para o Indivíduo: [", round(ic_pred_inf, 4), ";", round(ic_pred_sup, 4), "]"))

# 6.b) Comparação com a função predict()

cat("\n=== Exercício: 6-b ===\n")

# 1. Criar um novo data.frame com os dados da menina
# Os nomes das colunas DEVEM ser idênticos aos do data.frame original
nova_menina_df <- data.frame(
  HT2 = 90,
  WT2 = 12,
  HT9 = 135,
  WT9 = 30,
  ST9 = 20
)

# 2. Usar predict() para o Intervalo de Confiança
# (Usamos o 'modelo_lm' que você já criou)
pred_conf_r <- predict(modelo_lm, newdata = nova_menina_df, interval = "confidence", level = 0.95)

print("--- Comparação com R: Intervalo de Confiança ---")
print(pred_conf_r)

# 3. Usar predict() para o Intervalo de Predição
pred_pred_r <- predict(modelo_lm, newdata = nova_menina_df, interval = "prediction", level = 0.95)

print("--- Comparação com R: Intervalo de Predição ---")
print(pred_pred_r)


# 6.c) Comparação e Explicação dos Intervalos

# Como observado nos resultados, o Intervalo de Predição (IP) de [162.77, 176.95] 
# é visivelmente mais amplo do que o Intervalo de Confiança (IC) de [167.04, 172.68].

# O Intervalo de Predição é sempre mais amplo porque ele precisa levar em conta 
# duas fontes de incerteza, enquanto o Intervalo de Confiança leva em conta apenas uma:

# 1. Incerteza do Modelo (Comum aos dois): 
#    É a incerteza sobre a localização exata da linha de regressão média. 
#    Não temos certeza se a altura média prevista de 169.86 cm é o valor verdadeiro; 
#    ele poderia ser 168 cm ou 171 cm. O IC quantifica apenas essa incerteza.

# 2. Incerteza Individual (Apenas no IP): 
#    Esta é a variabilidade aleatória natural (o erro $\epsilon_i$) que faz 
#    com que uma observação individual (uma menina específica) não caia 
#    exatamente sobre a linha da média.

# Portanto, para prever a altura de uma única menina, precisamos considerar 
# a incerteza sobre onde está a média (Incerteza 1) E a incerteza de quão 
# longe essa menina específica pode estar dessa média (Incerteza 2). 
# Essa fonte adicional de variabilidade torna o Intervalo de Predição mais amplo.

# 6.d) Explicação Conceitual (Respostas)

# --- Intervalo de Confiança (para o valor médio) ---
# Conceito: Estima um intervalo para o valor médio esperado ($\mu_y$) de todas 
# as observações que possuem um conjunto específico de preditores ($x_0$).

# Pergunta que responde: "Se pegássemos um grupo grande de meninas, todas com 
# HT2=90, WT2=12, HT9=135, etc., qual seria a altura média desse grupo?"

# Interpretação: "Temos 95% de confiança de que a altura média de todas as 
# meninas com essas características está entre 167.04 cm e 172.68 cm."

# --- Intervalo de Predição (para um valor individual) ---
# Conceito: Estima um intervalo para o valor de uma única e nova observação 
# ($y_0$) que possui um conjunto específico de preditores ($x_0$).

# Pergunta que responde: "Se eu selecionar uma próxima menina ao acaso que 
# tenha essas características, qual será a altura dela?"
#
# Interpretação: "Temos 95% de confiança de que a altura de uma menina 
# individual com essas características estará entre 162.77 cm e 176.95 cm."

# 7. ANOVA

# 7.a - Monte a tabela ANOVA completa com GL, SQ, QM e teste F manualmente (sem funções prontas do R).

cat("\n=== Exercício: 7-a ===\n")

k <- p - 1     # número de covariáveis (exclui o intercepto)

# Graus de liberdade
GL_regressao <- k
GL_residuos <- n - p
GL_total <- n - 1

# Quadrados Médios
QM_regressao <- SQ_Regressao / GL_regressao
QM_residuos <- SQ_Residuos / GL_residuos

# Teste F
F_calc <- QM_regressao / QM_residuos
F_critico <- qf(1 - alfa, GL_regressao, GL_residuos)
p_valor_F <- 1 - pf(F_calc, GL_regressao, GL_residuos)

cat("\n TABELA ANOVA \n")
cat("Fonte de Variação | GL | SQ | QM | F\n")
cat("------------------|----|----|----|----\n")
cat("Regressão         |", GL_regressao, "|", round(SQ_Regressao, 2),
    "|", round(QM_regressao, 2), "|", round(F_calc, 2), "\n")
cat("Resíduos          |", GL_residuos, "|", round(SQ_Residuos, 2),
    "|", round(qm_res, 2), "| -\n")
cat("Total             |", GL_total, "|", round(SQ_total, 2), "| - | -\n")

# 7.b -Teste a hipótese H0 : todos os βj = 0 (exceto β0 ) vs H1 : pelo menos um βj ̸= 0.

cat("\n=== Exercício: 7-b ===\n")

cat("\n TESTE F PARA SIGNIFICÂNCIA GLOBAL \n")
cat("H0: Todos os betas (exceto intercepto) = 0\n")
cat("H1: Pelo menos um beta_j ≠ 0\n")
cat("F calculado:", round(F_calc, 3), "\n")
cat("F crítico (α=0.05):", round(F_critico, 3), "\n")
cat("p-valor:", format(p_valor_F, scientific = TRUE, digits = 3), "\n")

if (F_calc > F_critico) {
  cat("Decisão: Rejeita H0 → A regressão é significativa.\n")
} else {
  cat("Decisão: Não rejeita H0 → A regressão não é significativa.\n")
}

# 8 - Questões Conceituais

# 8.a - Quais as suposições do modelo de regressão linear múltipla?

# 1. Linearidade: relação linear entre Y e os preditores (nos parâmetros beta)
# 2. Exogeneidade: E(erro | X) = 0 → erros não correlacionados com as variáveis explicativas
# 3. Homoscedasticidade: var(erro) constante para todos os valores de X
# 4. Independência: erros independentes entre as observações
# 5. Normalidade: erros seguem distribuição normal (necessária para testes t e F)
# 6. Sem multicolinearidade perfeita: nenhuma variável explicativa é combinação exata de outra
# 7. Especificação correta: o modelo inclui as variáveis relevantes e não omite variáveis importantes



# 8.b - Quais as limitações do modelo de regressão linear múltipla? O modelo pareceu limitado para esse conjunto de dados de altura?

cat("\n=== Exercício: 8-b ===\n")

# 1. Supõe relação linear entre as variáveis — não capta relações não lineares complexas
# 2. Sensível a outliers, que podem distorcer as estimativas dos coeficientes
# 3. Requer ausência de multicolinearidade — correlação alta entre X’s afeta a estabilidade dos betas
# 4. Pressupõe homoscedasticidade — variância não constante gera inferências incorretas
# 5. Depende da independência dos erros — violação (ex: dados em série temporal) afeta validade dos testes
# 6. Assume normalidade dos erros para testes t e F — caso contrário, resultados podem ser imprecisos
# 7. Pode gerar viés se o modelo estiver mal especificado (variáveis omitidas ou irrelevantes incluídas)
# 8. Não lida bem com dados categóricos mal codificados ou com alta dimensionalidade
# 9. Não captura interações ou efeitos não lineares sem que sejam incluídos explicitamente

summary(modelo_lm)
R_quadrado
R_quadrado_ajustado
resultados_coeficientes

# O modelo apresentou R² = 0.73 e R² ajustado = 0.71, explicando bem a variação da altura aos 18 anos.
# HT9 foi altamente significativa (p < 0.001), sendo o principal preditor positivo.
# WT9 e ST9 também foram significativas, mas com efeito negativo.
# HT2 e WT2 não foram significativas.
# O erro residual (3.26) e o teste F (p < 0.001) indicam bom ajuste geral.
# Conclusão: o modelo não parece limitado — explica bem a altura final.


# 8.C - Compare os resultados da regressão múltipla com uma regressão simples usando apenas WT9. Qual modelo você prefere e por quê?

cat("\n=== Exercício: 8-c ===\n")

# Modelo simples usando apenas WT9
modelo_simples <- lm(HT18 ~ WT9, data = df_bgs)
summary(modelo_simples)

# Comparação rápida usando R²
r2_multipla <- summary(modelo_lm)$r.squared
r2_simples <- summary(modelo_simples)$r.squared

cat("R² múltipla:", r2_multipla, "\n")
cat("R² simples :", r2_simples, "\n")

# Comparando os modelos:
# - O modelo múltiplo tem R² = 0.73, R² ajustado = 0.71.
# - O modelo simples com WT9 tem R² menor (por exemplo, ~0.18).
# - WT9 sozinho explica pouco da variação, enquanto o modelo múltiplo melhora muito o ajuste.
# Conclusão: é preferivel o modelo múltiplo, pois explica melhor a altura final e inclui variáveis significativas.
