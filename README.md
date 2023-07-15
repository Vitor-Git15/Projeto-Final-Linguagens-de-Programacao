# Projeto de Linguagem de Programação em SML
Este repositório contém o projeto desenvolvido durante a disciplina de Linguagem de Programação, focado na criação de um lexer e um parser relacionados para a linguagem PLC. A linguagem PLC é uma extensão da linguagem "micro-ML".

## Objetivo do Projeto
O objetivo principal deste projeto é construir um analisador léxico (lexer) e um analisador sintático (parser) em SML para a linguagem PLC. Além disso, o projeto inclui a implementação de um verificador de tipos, que garantirá a correta atribuição de tipos a expressões na linguagem, e um interpretador, permitindo a execução dos programas escritos em PLC.

# Funcionalidades
Lexer: O analisador léxico irá transformar o código fonte da linguagem PLC em uma sequência de tokens, facilitando o processo de análise sintática posterior.

Parser: O analisador sintático será responsável por analisar a sequência de tokens gerados pelo lexer e construir a estrutura de árvore de sintaxe abstrata (AST) correspondente à entrada do programa.

Verificador de Tipos: Essa etapa será responsável por analisar a AST e verificar se as expressões estão de acordo com as regras de tipos da linguagem PLC.

Interpretador: O interpretador executará os programas em PLC, seguindo a estrutura construída na AST, e produzirá os resultados esperados.
