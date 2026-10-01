# Estudo de Simulação Monte Carlo (`simulations/`)

Este diretório contém os scripts e os resultados pré-computados do estudo de simulação Monte Carlo para processos de degradação de Wiener sob ações de manutenção imperfeita.

## Conteúdo do Diretório

- **`run_simulation_study.R`**: Script R contendo a definição do desenho fatorial (`Design`), as funções de geração (`Generate`), análise (`Analyse`) e consolidação (`Summarise`), além das chamadas para geração dos gráficos de mérito utilizando o pacote `WienerRS`.
- **`SimDesign4.rds`**: Resultados consolidados da simulação principal utilizada nos gráficos do artigo e dissertação (1000 replicações Monte Carlo).
- **`SimDesign.rds`**, **`SimDesign2.rds`**, **`SimDesign3.rds`**: Rodadas intermediárias e configurações complementares do estudo de simulação.

## Parâmetros do Estudo de Simulação

- **Número de sistemas monitorados ($n_{\text{system}}$)**: 1, 10, 20, 50.
- **Drift ($\mu$)**: 4, 16.
- **Variância de difusão ($\sigma^2$)**: 1, 25.
- **Ações de manutenção ($k$)**: 3, 4, 5.
- **Inspeções intermediárias ($n_j$)**: 0, 2, 4.
- **Horizonte de tempo ($\tau$)**: 20.
- **Replicações**: 1000 por cenário.

> **Aviso:** A reexecução completa de `runSimulation()` demanda múltiplos dias de processamento. Para reproduzir os gráficos do artigo, carregue diretamente os resultados pré-computados via `readRDS("simulations/SimDesign4.rds")`.
