#RStudio>Environment>Import Dataset>From Excel...>Browse...>BD_Alunos.xlsx

#instalação de pacotes
#install.packages("descr")
#install.packages("tidyverse",dependencies = TRUE)

#verificando a ausência de erros na instalação 
library(descr)
library(tidyverse)

# a) Descrever passo a passo a metodologia utilizada na seleção da amostra, indicando qual método utilizado:

#tamanho da amostra estabelecida previamente = 80

#criando os grupos para a amostragem
meteoro <- BD_Alunos[BD_Alunos$Curso=="Meteorologia",]
licenciatura <- BD_Alunos[BD_Alunos$Curso=="Licenciatura em Computação",]
eng <- BD_Alunos[BD_Alunos$Curso=="Engenharia",]

#amostragem sistemática precisa do número de elementos que virá de cada grupo, no final a soma tem que dar 80 
tamanhoeng <- round((80/294)*(length(eng$Registros)),digits=0)
tamanhometeoro <- round((80/294)*(length(meteoro$Registros)),digits=0)
tamanholic <- round((80/294)*(length(licenciatura$Registros)),digits=0)

#digits=0 é para transformar em um número inteiro, é como dizer quantas casas decimais existirão

#agora que temos o tamanho, precisamos retirar aleatoriamente essa quantidade de pessoas dos cursos
amostrameteoro <- meteoro[sample(nrow(meteoro),tamanhometeoro),]
amostralic <- licenciatura[sample(nrow(licenciatura),tamanholic),]
#antes de fazer a amostra de eng, verificou-se que a soma dos tamanhos (66+8+7) dá 81, então:
tamanhoeng <- tamanhoeng - 1
#agora sim, 65+8+7 = 80
amostraeng <-  eng[sample(nrow(eng),tamanhoeng),]

#unindo as amostras
superamostra <- rbind(amostraeng,amostralic,amostrameteoro)

# b) Construir tabelas e gráficos apropriados para as variáveis: “Curso”, “Cursou_EnsinoMedio”, “Genero” e “Trabalha”;

#Curso - tabela e gráfico

frequencias <- c(tamanhoeng,tamanholic,tamanhometeoro)
#100/80 = 1.25, tudo isso para usar na função:
percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(1.25 * x, format = format, digits = digits, ...), "%")
}
porcentagem <- percent(frequencias)
tabelacurso <- data.frame(Cursos=c("Engenharia","Lic. em Computação","Meteorologia"),Frequencias=c(tamanhoeng,tamanholic,tamanhometeoro),Porcentagem=porcentagem)
View(tabelacurso)
barplot(table(superamostra$Curso), ylim = c(0,80), main="Gráfico 1 - Cursos", ylab="Quantidade de alunos")

#Cursou_EnsinoMedio - tabela e gráfico
tabelacursou <- freq(superamostra$Cursou_EnsinoMedio)
View(tabelacursou)
barplot(table(superamostra$Cursou_EnsinoMedio), ylim = c(0,40), main="Gráfico 2 - Instituição de ensino do ensino médio", ylab="Quantidade de alunos")

#Genero - tabela e gráfico
tabelagenero <- freq(superamostra$Genero)
View(tabelagenero)
pie(table(superamostra$Genero),main = "Gráfico 3 - Gênero")

#Trabalho - tabela e gráfico
tabelatrabalho <- freq(superamostra$Trabalha)
View(tabelatrabalho)
pie(table(superamostra$Trabalha),main = "Gráfico 4 - Trabalho")

# c) Construir tabelas e gráficos apropriados para as variáveis: “Horas_EstudoSemana” e “Conhecimento_Matematica”;

#Horas_EstudoSemana - tabela e gráfico 
#numero de classes pela formula de sturges:
k <- round(1+3.3*log10(length(superamostra$Horas_EstudoSemana)))
amp <- max(as.numeric(superamostra$Horas_EstudoSemana)) - min(as.numeric(superamostra$Horas_EstudoSemana))
h <- round(amp/k)
int <- seq(1,60,8)
classes <- c("1|-9","9|-17","17|-25","25|-33","33|-41","41|-49","49|-60")
tabelahoras <- table(cut(as.numeric(superamostra$Horas_EstudoSemana),breaks = int,right=FALSE,labels = classes))
View(tabelahoras)
hist(as.numeric(superamostra$Horas_EstudoSemana),breaks = h,main="Gráfico 5 - Horas de estudo semanal",col="grey",xlab="Horas",ylab="Alunos")

#Conhecimento_Matematica - tabela e gráfico
tabelamatematica <- freq(superamostra$Conhecimento_Matematica) 
View(tabelamatematica)
hist(as.numeric(superamostra$Conhecimento_Matematica),breaks = 5,main="Gráfico 6 - Conhecimento em matemática",col="grey",xlab="Nota",ylab="Alunos")

# d) Calcular a média, mediana, moda, desvio-padrão, coeficiente de variação e simetria das variáveis: “Idade” e “Horas_EstudoSemana”;

#IDADE
idademedia <- mean(superamostra$Idade)
#r = 18.9
idademediana <- median(superamostra$Idade)
#r = 18
#função para conseguir a moda
getmode <- function(v) {
     uniqv <- unique(v)
     uniqv[which.max(tabulate(match(v, uniqv)))]
 }
idademoda <- getmode(superamostra$Idade)
#r = 18
idadedesvio <-  round(sd(superamostra$Idade),digits=1)
#r = 3.6
#CV = SD/MEAN*100
idadecv <- round((idadedesvio/idademedia)*100,digits=1)
#r = 19
#coeficiente de assimetria de pearson: (media - moda)/desviopadrao
idadeassimetria <- round((idademedia - idademoda)/idadedesvio,digits=1)
#r = 0.2

#HORAS 
horamedia <-  mean(as.numeric(superamostra$Horas_EstudoSemana))
horamediana <- median(as.numeric(superamostra$Horas_EstudoSemana))
#r = 10
horamedia <- round(mean(as.numeric(superamostra$Horas_EstudoSemana)),digits=1)
#r = 13.6
horamoda <- as.numeric(getmode(superamostra$Horas_EstudoSemana))
#r = 20
horadesvio <- sd(superamostra$Horas_EstudoSemana)
horadesvio <- round(sd(superamostra$Horas_EstudoSemana),digits=1)
#r = 12.6
horacv <- round((horadesvio/horamedia)*100,digits=1)
#r = 92.6
horaassimetria <- round((horamedia - horamoda)/horadesvio,digits=1)
#r = -0.5

# e) Na análise da “Idade”, “Horas_EstudoSemana” e “Conhecimento_Matematica”, qual variável tem maior variabilidade? Justifique;
matcv <- ((sd(superamostra$Conhecimento_Matematica))/(mean(superamostra$Conhecimento_Matematica)))*100
maior <- max(c(idadecv,horacv,matcv))
#maior = 92.6 = horacv

# f) Comparar a média de “Horas_EstudoSemana” em relação a variável “Trabalha” e descreva qual a conclusão da análise;
trabalha_sim <- BD_Alunos[BD_Alunos$Trabalha=="Sim",]
trabalha_nao <- BD_Alunos[BD_Alunos$Trabalha=="Não",]
horamedia_sim <-  mean(as.numeric(trabalha_sim$Horas_EstudoSemana))
horamedia_nao <-  mean(as.numeric(trabalha_nao$Horas_EstudoSemana))

#A média de horas de estudo de quem não trabalha é maior que a de quem trabalha. 13.8 > 9.8


# g) Relacione a variável “Genero” em relação ao “Cursou_EnsinoMedio” e 
#    responda: Qual a probabilidade de sortear um aluno que estudou em escola particular e
#    ser mulher? Dado que estuda em Escola Pública, qual a probabilidade de ser do gênero
#    masculino?

total_alunos <- 80
homem <- superamostra[superamostra$Genero=="Masculino", ]
mulher <- superamostra[superamostra$Genero=="Feminino", ]

homem_em_esc_part <- homem[homem$Cursou_EnsinoMedio=="Escola Particular", ]
mulher_em_esc_part <- mulher[mulher$Cursou_EnsinoMedio=="Escola Particular", ]
total_alunos_em_part <- as.numeric(length(homem_em_esc_part$Registros) + length(mulher_em_esc_part$Registros))

prob_esc_particular <- total_alunos_em_part / total_alunos

total_mulher <- as.numeric(length(mulher$Registros))
prob_mulher <- total_mulher / total_alunos

prob_mulher_em_particular <- prob_mulher * prob_esc_particular * 100

homem_em_esc_pub <- homem[homem$Cursou_EnsinoMedio=="Escola Pública Normal", ]
mulher_em_esc_pub <- mulher[mulher$Cursou_EnsinoMedio=="Escola Pública Normal", ]

total_alunos_em_pub <- as.numeric(length(homem_em_esc_pub$Registros) + length(mulher_em_esc_pub$Registros))
total_homem <- as.numeric(length(homem$Registros))
prob_homem <- total_homem / total_alunos

prob_esc_publica <- total_alunos_em_pub / total_alunos

prob_homem_em_publica <- prob_homem * prob_esc_publica * 100


# h) Relacione a variável “Trabalha” com “Curso” e responda: Qual a
#    probabilidade de sortear um aluno de Licenciatura em Computação que não trabalha?
#    Sabendo-se que o aluno é de Engenharia, qual a probabilidade de ele trabalhar?

total_licenciatura <- as.numeric(length(amostralic$Registros))
prob_licenciatura <- 1 / 3
prob_eng <- 1 / 3
prob_meteoro <- 1/3

aluno_q_nao_trabalha <- superamostra[superamostra$Trabalha=="Não", ]
qnt_aluno_q_nao_trabalha <- as.numeric(length(aluno_q_nao_trabalha$Registros))
qnt_aluno_q_nao_trabalha_lic <- as.numeric(length(amostralic$Trabalha=="Não"))

prob_nao_trabalha <- qnt_aluno_q_nao_trabalha_lic / qnt_aluno_q_nao_trabalha

prob_aluno_nao_trabalha <- prob_licenciatura * prob_nao_trabalha * 100

#_______________________________________________________________________________________

aluno_q_sim_trabalha <- superamostra[superamostra$Trabalha=="Sim", ]
qnt_total_sim <- as.numeric(length(aluno_q_sim_trabalha$Registros))
qnt_total_nao <- qnt_aluno_q_nao_trabalha

aluno_q_trabalha_eng_nao <- amostraeng[amostraeng$Trabalha=="Não", ]
aluno_q_trabalha_eng_sim <- amostraeng[amostraeng$Trabalha=="Sim", ]
qnt_aluno_q_trabalha_eng_sim <- as.numeric(length(aluno_q_trabalha_eng_sim$Registros))
qnt_aluno_q_trabalha_eng_nao <- as.numeric(length(aluno_q_trabalha_eng_nao$Registros))

prob_eng_trab_sim <- qnt_aluno_q_trabalha_eng_sim / qnt_total_sim
prob_eng_trab_nao <- qnt_aluno_q_trabalha_eng_nao / qnt_total_nao

prob_aluno_eng_trab_sim <- ((prob_eng_trab_sim * prob_eng) / ((prob_eng_trab_sim * prob_eng) + (prob_eng_trab_nao* prob_eng)))*100

# i) Sabendo que o aluno estuda mais de 10 horas por semana, qual a
#    probabilidade de ter conhecimento em matemática maior ou igual a 4?

hrs_por_semana <- superamostra[superamostra$Horas_EstudoSemana > 10]