library(ggplot2, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(tidyverse, warn.conflicts = FALSE)
library(readxl, warn.conflicts = FALSE)
library(arrow, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(shiny, warn.conflicts = FALSE)
library(shinydashboard, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)
library(scales, warn.conflicts = FALSE)
library(forcats, warn.conflicts = FALSE)
library(DT, warn.conflicts = FALSE)
library(janitor, warn.conflicts = FALSE)
library(vctrs, warn.conflicts = FALSE)
library(RColorBrewer, warn.conflicts = FALSE)
library(factoextra, warn.conflicts = FALSE)



# UI

ui <- dashboardPage(
  dashboardHeader(title = "Aplicativo com os dados do número de professores por Departamento na UFRGS", titleWidth = 1200),
  dashboardSidebar(
    sidebarMenu(
      width = 7,
      menuItem("UFRGS", tabName = "intro", icon = icon("users")),
      menuItem("Unidade", tabName = "unit", icon = icon("people-roof"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "intro",
        fluidPage(
          fluidRow(
            style = "text-align:center",
            img(
              src = "logos.jpg",
              width = "45%"
            )
          ),
          h1("Bem-vindo(a) ao aplicativo com os dados do número de professores da UFRGS!", style = "text-align:center"),
          fluidRow(
            p(
              style = "font-size:18px",
              "Este aplicativo foi desenvolvido por ",
              tags$a(
                "Gabriel Varani Martinewski",
                href = "https://github.com/Gabriel-Martinewski",
                target = "_blank"
              ),
              "e",
              tags$a(
                "Kevim Amorim Gomes,",
                href = "https://github.com/kevin-agomes",
                target = "_blank"
              ),
              "estudantes do Bacharelado em Estatística da UFRGS e
              bolsistas do Núcleo de Avaliação da Unidade do
              Instituto de Matemática e Estatística da UFRGS; e por
              Márcia Helena Barbian, professora do Departamento
              de Estatística da UFRGS e membro do NAU."
            )
          ),
          box(
            p(
              style = "font-size:20px;text-align:justify",
              "Este aplicativo utiliza uma simulação dos dados do número
                    de professores por departamento ou unidade da UFRGS, bem
                    como a relação entre o número de matrículas e o número de
                    professores por departamento.",
              br(),
              "O aplciativo foi montado originalmente a partir dos dados reais e sigilosos."
            ),
            background = "light-blue",
            width = 12
          ),
          h2("Gráfico de barras do número de matrículas na UFRGS",
             style = "text-align:center"
          ),
          box(
            width = 12,
            plotlyOutput("matri_ufrgs")
          ),
          h2("Gráfico de barras do número de professores por
                   Unidade na UFRGS",
             style = "text-align:center"
          ),
          box(
            width = 12,
            plotlyOutput("prof_ufrgs")
          ),
          h2("Gráfico de barras do número de matrículas por
                   professor em cada Departamento da UFRGS -
                   Departamentos com mais Matrículas/Professor",
             style = "text-align:center"
          ),
          box(
            width = 12,
            plotlyOutput("prof_matri_ufrgs")
          ),
          h2("Gráfico de barras do número de professores por
                   matrículas em cada Departamento da UFRGS -
                   Departamentos com menos Matrículas/Professor",
             style = "text-align:center"
          ),
          box(
            width = 12,
            plotlyOutput("less_prof_matri_ufrgs")
          ),
          h2("Série para o número de Matrículas por Professor a cada semestre",
             style = "text-align:center"
          ),
          box(
            width = 12,
            h3("Departamentos"),
            selectizeInput("seletor",
                           "Escolha os departamentos para o gráfico:",
                           choices = unique(simulated_data$DEPARTAMENTO),
                           multiple = TRUE,
                           selected =
                             c(
                               "Departamento de Direito Público e Filosofia do Direito",
                               "Departamento de Direito Econômico e do Trabalho",
                               "Departamento de Ciências Penais",
                               "Departamento de Direito Privado e Processo Civil",
                               "Departamento de Ciências Contábeis e Atuariais",
                               "Departamento de Matemática Pura e Aplicada",
                               "Departamento de Estatística",
                               "Departamento de Estudos Básicos",
                               "Departamento de Zootecnia",
                               "Departamento de Economia e Relações Internacionais"
                             )
            )
          ),
          box(
            width = 12,
            plotlyOutput("timeline")
          ),
          h2("Tabela da relação Matrículas por professor para cada Departamento",
             style = "text-align:center"
          ),
          box(
            width = 12,
            DT::dataTableOutput("ProfxAluno")
          ),
        ) # fluidPage
      ), # tabItem
      tabItem(
        "unit",
        h1("Dados por unidade"),
        selectInput("Escolha uma unidade:",
                    inputId = "nitnit",
                    choices = simulated_data$UNIDADE,
                    selected = "Instituto de Matemática e Estatística"
        ),
        valueBoxOutput("nprof"),
        valueBoxOutput("mean_matri"),
        valueBoxOutput("mean_matri_proof"),
        fluidRow(
          box(
            title = "Número de Professores por departamento",
            width = 6, plotlyOutput("undeprof")
          ),
          box(
            title = "Número de Matrículas por semestre",
            width = 6, plotlyOutput("UnitSemestre")
          ),
          box(
            title = "Razão do Número de matrículas por professor através dos semestres",
            width = 6, plotlyOutput("UnitTL")
          ),
          box(
            title = "Média de matrículas por professor para cada departamento",
            width = 6, plotlyOutput("UnitPercent")
          )
        ),
      ) # tabItem
    ) # tabItems
  ) # dashboardBody
) # dashboardPage
