
#Teste *F* de Graybill 

Resolvemos desenvolver este app, já que não existe nenhum software atualmente que realize este teste de forma nativa. 

Este teste é muito utilizado, principalmente na área florestal, sendo muitas vezes mais indicado do que os testes de média. Isto porque o teste *F* de Graybill considera todos os dados, e não só a média, que pode não ser representativa dos dados, principalmente com a presença de outliers.

Este app roda a linguagem R internamente, sendo portanto preciso e de código fonte aberto. O script utilizado neste app pode ser encontrado [aqui](https://github.com/sollano/Graybill_R).

A validação da predição constitui-se em ajustar um modelo linear de 1º
grau dos valores preditos em função dos valores observados.

A significância da regressão é avaliada aplicando-se o teste F para as
estimativas dos parâmetros, conforme metodologia descrita por GRAYBILL
(1976).

### Script criado por:

#### Sollano Rabelo Braga e Marcio Leles R. de Oliveira

##### Refêrencias:

CAMPOS, J. C. C.; LEITE, H. G. Mensuração florestal: perguntas e respostas. 3ª. ed. Viçosa: Editora UFV, 2013. 605 p.