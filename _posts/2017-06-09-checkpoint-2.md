---
title: "Agrupando os departamentos da UFCG"
author: "Luiz Fonseca"
date: "27 de junho de 2017"
layout: post
published: true
tags: [htmlwidgets, r]
---




{% highlight r %}
# Bibliotecas utilizadas
library(tidyverse)
library(gridExtra)
library(factoextra)
library(cluster)
library(GGally)
library(plotly)
{% endhighlight %}

## Objetivo

Nesta análise irei fazer um agrupamento com múltiplas variáveis utilizando o algoritmo K-means. A intenção é agrupar Unidades Acadêmicas (departamentos) da Universidade Federal de Campina Grande. 

Os dados vêm do portal da transparência do governo federal e são sobre os funcionários dos departamentos da UFCG. Após algumas derivações e transformações nos dados, eles passaram a ser organizados em função dos departamentos, que é o nosso objeto de estudo.

## Os dados


{% highlight r %}
departamentos.ufcg <- read.csv("https://raw.githubusercontent.com/nazareno/tamanhos-da-ufcg/master/ufcg-201704-sumario-UAs-wide.csv")
{% endhighlight %}

O dataframe derivado contém 7 colunas, que estão descritas abaixo.


{% highlight r %}
names(departamentos.ufcg) <- c("unidade_academica", "nao_professor", "professor_20h", "professor_40h", "idade_25perc", "idade_mediana", "idade_75perc")

colnames(departamentos.ufcg)
{% endhighlight %}



{% highlight text %}
## [1] "unidade_academica" "nao_professor"     "professor_20h"    
## [4] "professor_40h"     "idade_25perc"      "idade_mediana"    
## [7] "idade_75perc"
{% endhighlight %}

<b>unidade_academica:</b> Unidade acadêmica de lotação do funcionário

<b>nao_professor:</b> Número de funcionários na UA que não são professores

<b>professor_20h:</b> Número de professores com carga horária 20h/semana na UA

<b>professor_40h:</b> Número de professores com carga horária 40h/semana na UA, sejam eles Dedicação Exclusiva ou não

<b>idade_25perc, idade_mediana e idade_75perc:</b> 25, 50 e 75 percentil da idade dos funcionários no cargo (ex: idade_25perc = 10 significa que 25% dos funcionários, sejam eles professores ou não, da UA tem 10 anos ou menos na profissão).

Eu decidi retirar a idade do 25 percentil e do 75 percentil e utilizar somente a idade mediana, já que estas três variáveis dizem respeito ao mesmo dado (idade) e a mediana é uma métrica boa de sumarização.

Os gráficos abaixo mostram as distribuições de cada variável. Cada ponto representa uma unidade acadêmica (46 unidades acadêmicas no total).


{% highlight r %}
dados.agrupamento <- departamentos.ufcg %>%
  select(-idade_25perc, -idade_75perc)

plot1 <- dados.agrupamento %>% 
    ggplot(aes(x = "unidades acadêmicas", y = nao_professor)) + 
    geom_jitter(width = .025, height = 0, size = 2, alpha = .5) +
  labs(x = "", y="funcinários não professores")

plot2 <-  dados.agrupamento %>%
  ggplot(aes(x = "unidades acadêmicas", y = professor_20h)) + 
    geom_jitter(width = .02, height = 0, size = 2, alpha = .6) +
  labs(x = "", y="professores 20 h semanais")

plot3 <-  dados.agrupamento %>%
  ggplot(aes(x = "unidades acadêmicas", y = professor_40h)) + 
    geom_jitter(width = .02, height = 0, size = 2, alpha = .6) +
  labs(x = "", y="professores 40 h semanais")

plot4 <-  dados.agrupamento %>%
  ggplot(aes(x = "unidades acadêmicas", y = idade_mediana)) + 
    geom_jitter(width = .02, height = 0, size = 2, alpha = .6) +
  labs(x = "", y="mediana do tempo no cargo")

grid.arrange(plot1, plot2, plot3, plot4)
{% endhighlight %}

![plot of chunk unnamed-chunk-4](/portfolio-data-analysisfigure/source/problem-3/checkpoint-2/2017-06-09-checkpoint-2/unnamed-chunk-4-1.png)

Cada variável tem um comportamento diferente e poderíamos enxergar grupos de unidades acadêmicas de acordo com cada variável individualmente, mas este não é o nosso propósito.

Todas as variáveis serão utilizadas no agrupamento numa escala linear. A escala logarítmica não é tão útil nesse caso porque a faixa de valores das variáveis não é muito grande.

## Agrupamento

Como dito anteriormente, o algoritmo de agrupamento que será utilizado é o K-means. Os paramêtros de entrada do algorítmo são os dados do agrupamento e um valor k, que é o número de grupos em que se pretende agrupar os dados. 

Conceitualmente, o algorítmo funciona da seguinte forma:

<ol>
<li> Seleciona K centroides randomicamente; </li>
<li> Relaciona cada ponto dos dados com o centroide mais próximo em um espaço n-dimensional onde n é o número de variáveis do agrupamento. Depois disso cada ponto pertencerá a um grupo;</li>
<li> Recalcula o centroide como sendo o ponto médio de todos os outros pontos do grupo;</li>
<li> Continua executando os passos dois e três até que nenhum ponto tenha sido realocado para outro centroide ou até atingir o número máximo de iterações (o R usa 10 como default).</li>
</ol>

Quanto mais grupos (maior o valor de k) você escolher, menor será a variância dos grupos no seu agrupamento. No limite, cada observação será um grupo e a variância será 0. É interessante encontrar um equilíbrio entre o número de grupos e a variância destes.

Como vamos saber qual valor de k escolher para econtrar esse equilíbrio?

Para responder essa questão vamos executar o K-means para um k arbitrário, 3, por exemplo.


{% highlight r %}
# Como os centroides iniciais são definidos randomicamente, 
# nós definimos uma semente para fins de reprodutabilidade.
set.seed(123)

# Retira a coluna com o nome das UAs
input <- dados.agrupamento[,2:5]

# nstart indica que queremos que o algoritmo seja executado 20 vezes. A função irá selecionar
# a execução com menor variância no agrupamento
kmeans(input, centers = 3, nstart = 20)
{% endhighlight %}



{% highlight text %}
## K-means clustering with 3 clusters of sizes 30, 14, 2
## 
## Cluster means:
##   nao_professor professor_20h professor_40h idade_mediana
## 1       5.50000     0.8333333      19.86667     13.418448
## 2      10.42857     3.2857143      45.85714     11.399798
## 3       8.50000    53.5000000      24.00000      7.613105
## 
## Clustering vector:
##  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2
## [34] 2 2 2 2 2 2 3 2 2 2 2 2 3
## 
## Within cluster sum of squares by cluster:
## [1] 6069.3426 3519.5704  617.8244
##  (between_SS / total_SS =  54.1 %)
## 
## Available components:
## 
## [1] "cluster"      "centers"      "totss"        "withinss"    
## [5] "tot.withinss" "betweenss"    "size"         "iter"        
## [9] "ifault"
{% endhighlight %}

A função do R nos mostra os resultados do agrupamento. Podemos ver os centroides do agrupamento (cluster means), o grupo a qual cada observação foi atribuída (clustering vector) e a uma porcentagem que representa a eficiência do agrupamento (54.1%). 

Iremos utilizar essa porcentagem para encontrar um número de grupos que tenha uma variância satisfatória.

A função abaixo produz um gráfico da soma total dos quadrados (das distâncias entre pontos) internos de cada grupo pelo o número de grupos.


{% highlight r %}
wssplot <- function(data, nc=15, seed=123){
               wss <- (nrow(data)-1)*sum(apply(data,2,var))
               for (i in 2:nc){
                    set.seed(seed)
                    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
                plot(1:nc, wss, type="b", xlab="Número de grupos",
                     ylab="Soma dos quadrados dos grupos")}

wssplot(input, nc = 20)
{% endhighlight %}

![plot of chunk unnamed-chunk-6](/portfolio-data-analysisfigure/source/problem-3/checkpoint-2/2017-06-09-checkpoint-2/unnamed-chunk-6-1.png)

Analisando o gráfico da direita para a esquerda, vemos que de seis grupos para cinco grupos há um aumento maior na soma dos quadrados do que qualquer outro aumento anterior. Isso significa que quando passamos de seis grupos para cinco grupos há uma diminuição na eficiência do agrupamento. Nosso objetivo não é chegar a 100% de eficiência - para isso bastaria tomar cada observação como um grupo. O objetivo é encontrar um número satisfatório de grupos que explique uma quantidade satisfatória dos dados. 

Vamos, então, escoher k = 6 e executar o K-means novamente.


{% highlight r %}
set.seed(123)
agrupamento <- kmeans(input, centers = 6, nstart = 20)
agrupamento
{% endhighlight %}



{% highlight text %}
## K-means clustering with 6 clusters of sizes 20, 2, 10, 5, 2, 7
## 
## Cluster means:
##   nao_professor professor_20h professor_40h idade_mediana
## 1      5.200000     1.5500000     30.550000      8.830845
## 2     30.500000     0.0000000     46.500000     28.304886
## 3      9.400000     0.2000000     20.600000     25.871872
## 4      8.200000     7.4000000     57.000000      7.373516
## 5      8.500000    53.5000000     24.000000      7.613105
## 6      1.571429     0.1428571      6.142857      4.762518
## 
## Clustering vector:
##  [1] 6 6 6 6 6 6 3 3 6 3 3 3 1 1 3 1 3 1 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1
## [34] 1 1 2 1 1 2 5 4 4 4 4 4 5
## 
## Within cluster sum of squares by cluster:
## [1] 1393.87403   68.24838  660.61852  318.39167  617.82438  261.43115
##  (between_SS / total_SS =  85.1 %)
## 
## Available components:
## 
## [1] "cluster"      "centers"      "totss"        "withinss"    
## [5] "tot.withinss" "betweenss"    "size"         "iter"        
## [9] "ifault"
{% endhighlight %}

Com três grupos tínhamos 51,4% de dados bem agrupados. Com seis grupos, esse índice aumentou para 85,1%, que é um valor que nos satisfaz.

O gráfico de silhueta abaixo nos dá indícios de que o nosso agrupamento com seis grupos é um bom agrupamento, pois não há nenhum valor de silhueta negativo e a maioria deles está acima de 0,5.


{% highlight r %}
sil <- silhouette(agrupamento$cluster, dist(input))
fviz_silhouette(sil)
{% endhighlight %}



{% highlight text %}
##   cluster size ave.sil.width
## 1       1   20          0.43
## 2       2    2          0.65
## 3       3   10          0.47
## 4       4    5          0.58
## 5       5    2          0.30
## 6       6    7          0.66
{% endhighlight %}

![plot of chunk unnamed-chunk-8](/portfolio-data-analysisfigure/source/problem-3/checkpoint-2/2017-06-09-checkpoint-2/unnamed-chunk-8-1.png)

O gráfico abaixo mostra o resultado final do nosso agrupamento. Você pode isolar um grupo para ver suas características.


{% highlight r %}
dados.agrupamento$grupo <- as.factor(agrupamento$cluster)

p <- ggparcoord(data = dados.agrupamento, columns = c(2:5), groupColumn = "grupo", scale = "std") + labs(x = "Variável", y = "valor (em unidade de desvios-padrões)", title = "Resultado do agrupamento")
ggplotly(p)
{% endhighlight %}



{% highlight text %}
## Error in loadNamespace(name): there is no package called 'webshot'
{% endhighlight %}

Vamos identificar os padrões de cada grupo e dar nome a esses grupos.

<ul>
<li> <b>Os normais:</b> o grupo 1 parece ser o grupo com mais valores em torno do valor médio para todas as 4 variáveis. Também é o grupo com mais integrantes. A UASC está nesse grupo</li>
<li> <b>Os não-acadêmicos:</b> o grupo 2 é o das UAs que possuem mais funcionários que não são professores, possuem um número de professores 20h um pouco abaixo da média e as outras duas variáveias são acima da média. </li>
<li> <b>Os veteranos:</b> a principal característica do grupo 3 é que ele engloba a grande maioria das UAs com funcionários a mais tempo no cargo. Este grupo possui um número de professores abaixo da média e um número de funcionários não professores que varia em torno da média.</li>

<li> <b>Os integrais:</b> a principal característica do grupo 4 é que ele engloba as UAs com o maior número de professores com carga-horária de 40 horas. Outra característica do grupo é que todas as UAs possuem idade mediana do cargo abaixo da média.</li>

<li> <b>Meio-expediente:</b> o grupo cinco só consta com 2 UAs. Ele se destaca pois há um pico no número de professores com carga horária de 20 horas, superando em mais de 4 vezes o desvio padrão em um dos casos. </li>

<li> <b>Os pequenos:</b> o grupo seis se caracteriza por possuir o valor de todas as variáveis abaixo da média.</li>

</ul>
<br>
<br>
<br>
