#Importando lib do dplyr
install.packages("dplyr")
install.packages("stringr")
library(dplyr)
library(stringr)

#Importando dataset com os dados de medicamentos de A a Z comercializados na india.
dados <- read.csv('A_Z_medicines_dataset_of_India.csv')
dados

#Modificando nome da coluna de preço
dados <- dados %>%
  rename(price = `price...`)

dados$price <- as.numeric(dados$price)

#Listando os top medicamentos mais caros da india.
top_maiores_precos <- dados %>%
  arrange(desc(price)) %>%
  slice_max(n = 5, order_by = price)
  
top_maiores_precos

#Listando os top medicamentos mais utilizados na india.
frequencia <- table(dados$short_composition1)
frequencia_ordenada <- sort(frequencia, decreasing = TRUE)
strings_mais_frequentes <- names(head(frequencia_ordenada, 4))

print(strings_mais_frequentes)

aceclofenac <- dados %>% filter(short_composition1 == "Aceclofenac (100mg) ") 
aceclofenacQuant <- unlist(count(aceclofenac))

domperidone <- dados %>% filter(short_composition1 == "Domperidone (30mg) ") 
domperidoneQuant <- unlist(count(domperidone))

cefixime <- dados %>% filter(short_composition1 == "Cefixime (200mg) ") 
cefiximeQuant <- unlist(count(cefixime))

diclofenac <- dados %>% filter(short_composition1 == "Diclofenac (50mg) ") 
diclofenacQuant <- unlist(count(diclofenac))

quantPorMedicamento <- c(aceclofenacQuant, domperidoneQuant, cefiximeQuant, diclofenacQuant)

#Listando os preços dos medicamentos destinados ao tratamento de cancer de prostata
#comercializados na india.
listaPrecosProstata <- dados %>% 
  filter(short_composition1 == 'Abiraterone Acetate (250mg)') %>%
  select(price)
  
listaPrecosProstata

#Listando os preços dos medicamentos destinados ao tratamento de cancer de mama
#comercializados na india.
listaPrecosMama <- dados %>% 
  filter(short_composition1 == 'Trastuzumab (440mg)') %>%
  select(price)

listaPrecosMama

#Soma dos preços dos medicamentos destinados ao tratamento do cancer de prostata
somaPrecoProstata <- dados %>% filter(short_composition1 == 'Abiraterone Acetate (250mg)') %>% summarise(soma = sum(price))
somaPrecoProstata

#Soma dos preços dos medicamentos destinados ao tratamento do cancer de mama
somaPrecoMama <- dados %>% filter(short_composition1 == 'Trastuzumab (440mg)') %>% summarise(soma = sum(price))
somaPrecoMama

#Teste de hiposte
#Medicamentos destinados ao tratamento de cancer de mama são mais caros que os medicamentos destinados 
#ao tratamento de cancer de prostata?

# Dados do exemplo  # Amostra observada
n_mama <- length(unlist(listaPrecosMama))
n_prostata <- length(unlist(listaPrecosProstata)) # Tamanho da amostra
media_amostral <- mean(unlist(listaPrecosMama))  # M?dia da amostra
media_prostata <- mean(unlist(listaPrecosProstata))  # M?dia da amostra
desvio_padrao <- sd(unlist(listaPrecosMama))  # Desvio padr?o populacional conhecido

# Nivel de significancia
nivel_significancia <- 0.05

# Estatistica de teste
z <- (media_amostral - media_prostata) / (desvio_padrao / sqrt(n_mama))

# Valor critico
valor_critico <- abs(qt(0.05, n_mama - 1))

# Região critica
regiao_critica <- z > valor_critico

# Resultados
cat("Estat?stica de teste:", z, "\n")
cat("Valor cr?tico:", valor_critico, "\n")
cat("Regi?o cr?tica:", regiao_critica, "\n")


#Gráficos

#Medicamentos mais comercializados(Rastreabilidade)

par(mar = c(4, 4, 4, 4))

nomes <- c("Aceclofenac", "Domperidone", "Cefixime", "Diclofenac")
barplot(quantPorMedicamento, main="Medicamentos mais comercializados na india",
          names.arg=nomes, xlab="Compostos", ylab="Frequência",
          col="purple")

#Boxplots de cancer de prostata e de mama(Hipotese)

options(scipen = 999)

par(mfrow = c(1,2))

boxplot(listaPrecosProstata, main="Prostata", col="red")
boxplot(listaPrecosMama, main="Mama", col="lightblue")
                            
par(mfrow = c(1,1)) 





