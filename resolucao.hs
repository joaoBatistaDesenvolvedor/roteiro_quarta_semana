--1) Abra o ambiente interativo GHCi e avalie as seguintes expressões.
--testei todos mais para printar era muito
--1:[2,3,4]--
--'a':['b','c','d']--
--head [1,2,3]--
--tail [1,2,3]--
--[1,5,2,3]!!1--
--[1,5,2,3]!!3--
--elem 2 [1,5,2,3]
--take 2 [1,5,2,3,7]
--drop 2 [1,5,2,3,7]
--[1,2] ++ [3,4]
--[1..10]
--[7,6..3]
--['b'..'g']
--take 5 [1,3..]
--sum [1..10]
--maximum [1,5,2,3,7]
--minimum [1,5,2,3,7]--

--2) Gere por enumeração as seguintes listas:
--a) [5,4,3,2,1]
--b) [a,c,e]
--c) [1,4,7,10,13,16]
--d) [(1,1),(-2,5),(-5,9),(-8,13),(-11,17)] *obs: também é necessário usar
--a função zip para criar as tuplas--

contador::Integer->[Integer ]
contador numero = [numero,numero-1..1]

letras :: Char ->[Char]
letras l  =['a','c'..'e']

c:: [Int ]
c =[1,4..16]

d::[(Int ,Int )]
d =zip [1,-2..(-11)] [1,5..17]


--3) Funções que utilizam listas enumeradas
--a) Utilizando enumeração, construir uma função que dados dois inteiros a e b construa a
--lista dos inteiros contidos no intervalo fechado [a,b]. Quando a for igual a b, a função
--devolve a lista unitária [a]. Quando a > b a função deverá devolver a lista vazia.
de_ate_b::Integer->Integer->[Integer ]
de_ate_b a b | a==b=[a]
             |a>b=[]
             |otherwise =[a..b]




--b) Utilizando enumeração, construir uma função que dados dois inteiros a e b construa a
--lista dos inteiros pares contidos no intervalo aberto (a,b). Quando a for igual a b ou a > b
--a função devolve a lista vazia. (*Dica: verificar se a é par ou ímpar)
de_a_ate_b::Integer->Integer->[Integer ]
de_a_ate_b a b| a>b || a==b=[]
              |otherwise =[x| x<-[a+1..b-1], a `mod` x == 0]



--4) Abra o ambiente interativo GHCi e avalie as seguintes expressões:
--lst1 = [x*2 | x <- [1..10], x*2 >= 12]
--lst2 = [ x | x <- [50..100], mod x 7 == 3]
--lst3 = [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]
--lst4=[(x,y)| x <- [1..4], y <- [x..5]]
-- testei




--5) Usando lista por compreensão, escreva a função quadrados que recebe dois inteiros e
--retorna os quadrados dos números entre eles. E.g.:
-- > quadrados 4 9
--[16,25,36,49,64,81]

quadrado::Integer->Integer->[Integer ]

quadrado a b=[x*x| x<-[a..b]]


--6) Usando lista por compreensão, escreva a função seleciona_ímpares que recebe um
--lista de inteiros e retorna uma nova lista com todos os números ímpares presentes na lista
-- de entrada.
-- > seleciona_ímpares [2,5,1,4,7]
-- [5,1,7]

seleciona_ímpares::[Integer ]->[Integer]
seleciona_ímpares xs=[x| x<-xs , x `mod` 2 ==1]








--7) Escreva a função tabuada que recebe um valor inteiro e retorna a lista de seus dez
-- primeiros múltiplos. E.g.:
-- > tabuada 7
--[7,14,21,28,35,42,49,56,63,70]---

tabuada::Integer->[Integer] 
tabuada n=[n*x| x<-[1..10]]



--8) Escreva a função bissextos a seguir que recebe uma lista de inteiros e retorna uma
--lista com os valores que representam anos bissextos. Dica: use a função bissexto do
--roteiro anterior.
-- > bissextos [100,400,2020,2021,2022,2024]
--[400,2020,2024
ano_bisexto::Integer->Bool

ano_bisexto x | (mod x 400 == 0) = True 
              | (mod x 4 == 0) && (mod x 100 /= 0) = True
              | otherwise = False 




lista_bisexto::[Integer]->[Integer]
lista_bisexto xs=[x| x<-xs, ano_bisexto x ] 





-- 9) Usando lista por compreensão, escreva a função sublistas que recebe uma lista
-- formada por sublistas de um mesmo tipo e retorna uma lista com todos os elementos da
--lista de entrada na mesma ordem, mas no nível da lista principal, sem sublistas.
-- > sublistas [[2,5],[1],[4,7,2]]
--[2,5,1,4,7,2]

sublistas::[[Integer]]->[Integer ]

sublistas xss=[x| xs<-xss,x<-xs ]



--10) Sejam os tipos Data, Emprestimo, Emprestimos e a variável bdEmprestimo do
--exemplo da Biblioteca. Escreva a função atrasados que recebe um parâmetro do tipo
--Emprestimos e a Data atual, e retorna uma lista com todos os empréstimos atrasados. 


type Data = (Int, Int, Int)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]
bdEmprestimo::Emprestimos
bdEmprestimo =
 [("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),
 ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),
 ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]
 

 
 


atrasados::Emprestimos->Data
atrasados xs (d,m,a)=[x| x<-xs, ((x,y,z):xs),(d,m,a)>(x,y,z)]


--  11) Usando compreensão de listas, escreva a função uniaoNRec a seguir que faz a união
-- de duas listas de modo que ela mantenha todos os elementos da 1a lista na mesma
-- ordem e no final acrescenta apenas os elementos da 2a lista que não estejam presentes
-- na 1a lista.
-- > uniaoNRec [1,2,3,4,5,6,7] [2,9,7,10,4]
--[1,2,3,4,5,6,7,9,10]




uniaoNRec::[Integer]->[Integer]->[Integer]
uniaoNRec xs1 xs2=[x|x<-xs1++xs2 ]
 
 