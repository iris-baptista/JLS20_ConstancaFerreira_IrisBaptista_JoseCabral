Caso o IntelliJ não reconheça o projeto, mesmo tendo o Scala instalado, ir:
- Project Structure -> Modules -> + -> new module;
- Ou então definir pasta src/main/scala como sources root:
    * Clique com o botão direito do rato sobre a pasta -> Mark directory as -> Sources Root
- Em princípio passará a funcionar, senão é necessário reiniciar.

Caso hajam problemas com o compilador, certificar-se de que estamos a usar a versão 1.8 do Java