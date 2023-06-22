# Project-LP
Projeto da disciplina de Linguagem de Programação

## Execução
Dentro da pasta `project` existem alguns casos de teste, sendo os principais `fullTest.sml` e `testParser.sml`. Basta usar um interpretador de sml e rodar (selecionar as partes desejadas e apertar ctrl+enter) os arquivos para obter os resultados.
A seguir um exemplo para `testParser.sml` e seu respectivo output:
```
fromString "print x; true";
```
```
val it = Prim2 (";",Prim1 ("print",Var "x"),ConB true) : ?.expr
```

Ainda não é possível utilizar a parte do `fullTest.sml` pois está sendo implementado.

## Funcionamento (resumo)
Parte 1 - Convesão do input, quando possível, para o formato da liguagem PLC. (Como visto no exemplo).

Parte 2 [Em progresso]- Evaluate do output da Parte 1. (Resultado da função + seu tipo).
