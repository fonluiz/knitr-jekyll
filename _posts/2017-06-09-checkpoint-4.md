---
title: "Analisando séries de Televisão"
author: "Luiz Alberto Fonseca"
date: "24 de maio de 2017"
layout: post
published: true
tags: [htmlwidgets, r]
---



### Qual o objetivo?

Para esta análise iremos continuar utilizando dados do [IMDB](http://www.imdb.com/) sobre séries de televisão conforme descritos anteriormente neste [link](https://fonluiz.github.io/ad1/lab1/checkpoint-1.html).

O objetivo é responder as perguntas abaixo, embasando minhas respostas em análises dos dados:

<ol>
<li> Existe alguma relação entre a sequência de um episódio na temporada e a sua nota? </li>
<li> Uma série com mais episódios é mais bem avaliada do que uma série com menos episódios? </li>
</ol>

Ao longo deste relatório irei explicar e explorar os atributos que utilizarei para responder às questões. Mas de uma forma geral, dentre as informações sobre as séries, temos o nome da série, o nome de cada episódio, a temporada correspondente a cada episódio, a nota para cada episódio, o número de votos e algumas outras que serão comentadas mais adiante.

Diferentemente da análise anterior, nesta irei utilizar todas as séries presentes nos dados. Entretanto, percebi que há inconsistências nos dados e por isso utilizarei apenas as séries com oito temporadas ou menos.


{% highlight r %}
library(tidyverse)

# Lendo os dados
dados.series <- read_csv("series_from_imdb.csv")
{% endhighlight %}



{% highlight text %}
## Error: 'series_from_imdb.csv' does not exist in current working directory ('/home/luiz/Faculdade/portfolio-data-analysis/_source/problem-1/checkpoint-4').
{% endhighlight %}



{% highlight r %}
# Séries com mais de oito temporadas
series.mais.que.oito.temp <- dados.series %>%
  filter(season > 8) %>%
  select(series_name)
{% endhighlight %}



{% highlight text %}
## Error in eval(expr, envir, enclos): objeto 'dados.series' não encontrado
{% endhighlight %}



{% highlight r %}
series.mais.que.oito.temp <- unlist(unique(series.mais.que.oito.temp))
{% endhighlight %}



{% highlight text %}
## Error in unique(series.mais.que.oito.temp): objeto 'series.mais.que.oito.temp' não encontrado
{% endhighlight %}



{% highlight r %}
# definindo operador not in
'%!in%' <- function(x,y)!('%in%'(x,y))

# Os mesmos dados de antes excluindo 4 séries que possuiam mais de 8 temporadas
dados.series <- dados.series %>% 
  filter(series_name %!in% series.mais.que.oito.temp)
{% endhighlight %}



{% highlight text %}
## Error in eval(expr, envir, enclos): objeto 'dados.series' não encontrado
{% endhighlight %}


### Pergunta 1
#### Existe alguma relação entre a sequência de um episódio na temporada e a sua nota?

Vamos utilizar dois métodos para responder essa questão. O primeiro será através de visualizações dos dados e o segundo através da estatística. 

#### Visualizações


{% highlight r %}
heatmap.episodios.nota <- dados.series %>%
  mutate(series_season = paste(series_name, season, sep = "-"))
{% endhighlight %}



{% highlight text %}
## Error in eval(expr, envir, enclos): objeto 'dados.series' não encontrado
{% endhighlight %}



{% highlight r %}
ggplot(heatmap.episodios.nota, aes(series_season, season_ep)) + 
  geom_tile(aes(fill = UserRating), colour = "white") + 
  scale_fill_gradient(low = "red", high = "green") + 
  coord_flip() +
  labs(title = 'Avaliação de episódios ao longo de temporadas', y = 'Índice do episódio na temporada', x = 'Temporadas', fill = 'Notas dos \n usuários') +
  theme(axis.text.y=element_blank(), axis.ticks.y = element_blank()) 
{% endhighlight %}



{% highlight text %}
## Error in ggplot(heatmap.episodios.nota, aes(series_season, season_ep)): objeto 'heatmap.episodios.nota' não encontrado
{% endhighlight %}

O gráfico acima nos mostra a avaliação dos usuários ao longo de cada temporada das séries.
Observando o gráfico podemos desconfiar que a resposta para a nossa pergunta é "não". Se a resposta fosse "sim", os retângulos mais vermelhos estariam mais concentrados em um lado (direito ou esquerdo) e os retângulos maisverdes estariam mais concentrados do lado oposto, formando um efeito de gradiente de cor. Porém, podemos ver que os retângulos de cores diferentes se misturam e nos fazem entender que não há um padrão entre os episódios iniciais ou finais e as notas boas ou ruins.

#### Utilizando a correlação

Existe uma métrica estatística que mede exatamente o que procuramos: o grau de relação entre duas variáveis. Essa métrica é a correlação, que é um valor entre -1 e 1. A interpretação da correlação é que quanto mais perto de 0 mais fraca é a relação entre as duas variáveis e quanto mais próximo dos extremos (1 e -1) mais forte é essa relação. Se for um valor positivo quer dizer que ambas as variáveis crescem ou decrescem juntas, já se for um valor negativo indica que enquanto uma cresce a outra decresce. Traremos essa interpretação para o nosso problema após calcular a correlação entre o índice de um episódio na série e a sua nota.


{% highlight r %}
# Correlação entre  o índice de um episódio na série e a sua nota
cor(dados.series$season_ep, dados.series$UserRating)
{% endhighlight %}



{% highlight text %}
## Error in is.data.frame(y): objeto 'dados.series' não encontrado
{% endhighlight %}

O valor da correlação entre as variáveis estudadas foi -0.01. É um valor bem próximo de 0 e indica que podemos considerar que não há nenhuma relação entre as variáveis. O sinal da correlação não é realmente importante para este caso já que não há relação entre as variáveis.

<i>A saber: Há outros tipos de correlação que são calculados de formas diferentes. Para essa análise foi utilizada a correlação de Pearson.</i>

#### Conclusão

O que foi visto no gráfico também foi visto na correlação e então sabemos que a resposta para a pergunta é não. Não há uma relação entre a sequência de um episódio na temporada e a sua nota.


### Pergunta 2
#### Uma série com mais episódios é mais bem avaliada do que uma série com menos episódios?

Essa pergunta também será respondida com visualiações e cálculos. Vamos primeiro às visualizações.

#### Visualizações


{% highlight r %}
summary.series <- dados.series %>%
  group_by(series_name) %>%
  summarise(num_eps = n(), median = median(UserRating), mean = mean(UserRating))
{% endhighlight %}



{% highlight text %}
## Error in eval(expr, envir, enclos): objeto 'dados.series' não encontrado
{% endhighlight %}



{% highlight r %}
ggplot(data = summary.series, aes(x = num_eps, y = median)) +
  geom_point()+
  geom_smooth(method=lm) +
  labs(x = 'Número de episódios', y = 'Mediana das avaliações', title = 'Relação entre a quantidade de episódios e a avaliação de uma série')
{% endhighlight %}



{% highlight text %}
## Error in ggplot(data = summary.series, aes(x = num_eps, y = median)): objeto 'summary.series' não encontrado
{% endhighlight %}

No gráfico acima, cada ponto representa uma série e temos informações da quantidade de episódios e da mediana das avaliações. A reta azul é a reta de regressão entre essas variávies. Ela indica um tipo de relação entre as variáveis, similar à correlação. Pela interpretação da reta e dos pontos podemos ver que parece que quanto mais episódios menor é a mediana das avaliações. Vamos calcular a correlação entre essas duas variáveis para podermos apurar nossas conclusões.

#### Cálculo da correlação


{% highlight r %}
# Correlação entre o número de episódios
cor(summary.series$num_eps, summary.series$median)
{% endhighlight %}



{% highlight text %}
## Error in is.data.frame(y): objeto 'summary.series' não encontrado
{% endhighlight %}

A correlação entre as variáveis foi de -0.586, o que é considerada uma correlação media-alta. Isso significa que há indícios para acreditarmos que há uma relação entre o número de episódios de uma série e as suas avaliações. Diferentemente da pergunta anterior, o sinal da correlação para este caso é relevante. O sinal negativo indica que enquanto uma variável cresce a outra decresce. Através da visualização percebemos que é a mediana que decresce conforme o número de episódios cresce. 

#### Conclusão

Neste exemplo também foi utilizada a correlação de Pearson. É importante lembrar que a correlação não implica causalidade. Então não podemos afirmar que um número maior de episódios causa avaliações mais baixas nas séries. O que podemos concluir do gráfico e do cálculo da correlação de Pearson é que há uma relação mediana/forte entre as variáveis estudadas, mas não sabemos ao certo o porquê.

### Novas perguntas

A partir das análises anteriores formulamos mais duas questões a serem pensadas e respondidas com novas análises dos dados. Na conclusão da pergunta 1 vimos que não há correlação entre o índice de um episódio na temporada e sua avaliação. A partir disso, surgiu uma nova dúvida:

3. Será que as avaliações dos episódios estão bem relacionadas com alguma variável? E com quais variáveis elas estão melhor relacionadas.

Na conclusão da pergunta 2 vimos que o número de episódios de uma série está correlacionado com a mediana das avaliações dos episódios daquela série. A partir desses resultados formulamos a seguinte questão:

4. Para séries com várias temporadas, a temporada inicial é melhor avaliada do que a temporada final? Já que pressupomos que há uma relação entre o número de episódios e a avaliação dos episódios.

### Pergunta 3
#### Será que as avaliações dos episódios estão bem relacionadas com alguma variável? E com quais variáveis elas estão melhor relacionadas.

Para responder essa questão, iremos nos basear novamente na correlação, já que esta mede o grau de relação entre duas variáveis. O gráfico a seguir se chama matrix de correlação e indica a correlação entre algumas variáveis que selecionei.


{% highlight r %}
library(ggcorrplot)

dados.cor <- dados.series %>%
  select(series_ep, season, season_ep, UserRating, UserVotes) %>%
  plyr::rename(c("series_ep" = 'indice na serie',
         "season" = 'temporada',
         'season_ep' = 'indice na temporada',
         'UserRating' = 'avaliação',
         'UserVotes' = 'votos'))
{% endhighlight %}



{% highlight text %}
## Error in eval(expr, envir, enclos): objeto 'dados.series' não encontrado
{% endhighlight %}



{% highlight r %}
corr <- round(cor(dados.cor), 3)
{% endhighlight %}



{% highlight text %}
## Error in is.data.frame(x): objeto 'dados.cor' não encontrado
{% endhighlight %}



{% highlight r %}
ggcorrplot(corr, hc.order = TRUE, type = "lower",
   lab = TRUE)
{% endhighlight %}



{% highlight text %}
## Error in ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE): objeto 'corr' não encontrado
{% endhighlight %}

Como o foco da nossa pergunta é na variável "avaliação", podemos olhar apenas para a última linha da matrix de correlação, a qual nos mostra a relação da variável "avaliação" com as demais variáveis. Podemos notar que todas são correlações fracas e nenhuma está acima de 0.35 ou abaixo de -0.35. 

Então, a resposta para esta pergunta é que nenhuma das variáveis está bem relacionada com as avaliações dos usuários. A que está melhor correlacionada é o índice do episódio na série (-0.31), porém ainda assim é uma correlação considerada fraca. Em uma pergunta anterior, havíamos achado uma correlação moderada entre a mediana das avaliações e o número de episódios das séries. Ambas as variáveis foram derivadas dos dados originais e no escopo da pergunta a intenção era analisar a correlação com variáveis originais. Poderíamos encontrar alguma correlação forte entre as avaliações e alguma variável derivada. Isso fica como questionamento para análises futuras.

### Pergunta 4
#### Para séries com várias temporadas, a temporada inicial é melhor avaliada do que a temporada final? Já que pressupomos que há uma relação entre o número de episódios e a avaliação dos episódios.

Para responder esta pergunta, primeiro vamos filtrar nossos dados para retermos apenas séries com quatro ou mais temporadas. Realizaremos esse filtro por pressupormos que para séries com quatro ou mais episódios já haja uma diferença considerável da quantidade de episódios da série entre a primeira e a última temporada.


{% highlight r %}
# Séries com mais de quatro temporadas
series.mais.que.quatro.temp <- dados.series %>%
  filter(season >= 4) %>%
  select(series_name)
{% endhighlight %}



{% highlight text %}
## Error in eval(expr, envir, enclos): objeto 'dados.series' não encontrado
{% endhighlight %}



{% highlight r %}
series.mais.que.quatro.temp <- unlist(unique(series.mais.que.quatro.temp))
{% endhighlight %}



{% highlight text %}
## Error in unique(series.mais.que.quatro.temp): objeto 'series.mais.que.quatro.temp' não encontrado
{% endhighlight %}



{% highlight r %}
# Filtrando apenas episódios de séries com mais de quatro temporadas
dados.series <- dados.series %>% 
  filter(series_name %in% series.mais.que.quatro.temp)
{% endhighlight %}



{% highlight text %}
## Error in eval(expr, envir, enclos): objeto 'dados.series' não encontrado
{% endhighlight %}

Nos sobraram 17 séries para analisarmos, todas com mais de quatro temporada. Vamos utilizar a mediana das avaliações dos episódios da primeira e da última temporadas para julgarmos qual a melhor.


{% highlight r %}
# Medianas da primeira temporada
medianas.primeira.temp <- dados.series %>%
  ungroup() %>%
  filter(season == 1) %>%
  group_by(series_name) %>%
  summarise(mediana_prim = median(UserRating))
{% endhighlight %}



{% highlight text %}
## Error in eval(expr, envir, enclos): objeto 'dados.series' não encontrado
{% endhighlight %}



{% highlight r %}
# Medianas da última temporada
medianas.ultima.temp <- dados.series %>%
  ungroup() %>%
  group_by(series_name) %>%
  filter(season == max(season)) %>%
  summarise(mediana_ulti = median(UserRating))
{% endhighlight %}



{% highlight text %}
## Error in eval(expr, envir, enclos): objeto 'dados.series' não encontrado
{% endhighlight %}



{% highlight r %}
# Juntando as medianas em um único dataframe
medianas.prim.ult <- inner_join(medianas.primeira.temp, medianas.ultima.temp) %>%
 plyr::rename(c(series_name = "séries", mediana_prim = 'primeira temporada', mediana_ulti = "última temporada"))
{% endhighlight %}



{% highlight text %}
## Error in inner_join(medianas.primeira.temp, medianas.ultima.temp): objeto 'medianas.primeira.temp' não encontrado
{% endhighlight %}



{% highlight r %}
library(reshape2)
# Dataframe 'derretido' (melted) para visualização
data.m <- melt(medianas.prim.ult, id.vars='séries') 
{% endhighlight %}



{% highlight text %}
## Error in melt(medianas.prim.ult, id.vars = "séries"): objeto 'medianas.prim.ult' não encontrado
{% endhighlight %}



{% highlight r %}
ggplot(data.m, aes(x = séries, value)) +   
  geom_bar(aes(fill = variable), position = position_dodge(width=0.5), stat="identity") +
  coord_flip() +
  labs(title = "Avaliação da primeira e últia temporada das séries", y = "mediana das avaliações", fill = "Temporada")
{% endhighlight %}



{% highlight text %}
## Error in ggplot(data.m, aes(x = séries, value)): objeto 'data.m' não encontrado
{% endhighlight %}

Analisando o gráfico acima, vemos que dentre as 17 séries avalidadas 7 possuem a mediana da última temporada maior que a mediana da primeira, 9 possuem a mediana da primeira maior que a da última e 1 série possui medianas iguais para ambas as temporadas. Entao concluímos que não há uma regra. A avaliação da última temporada as vezes é melhor e as vezes é pior do que a da primeira e podemos ver também que quase sempre as medianas das notas são bem parecidas.
