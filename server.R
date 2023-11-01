library(shiny)

###################################################################

server <- function(input, output, session) {
  simulated_data <- read.csv('simulated_data.csv')
  
  # GRÁFICOS ----
  # Histograma Matrículas ######################################################
  output$matri_ufrgs <- renderPlotly({
    matriculas_semestre <- simulated_data %>%
      group_by(semestre) %>%
      summarise(matriculas = sum(total_matri_ufrgs))
    
    plotmatri_ufrgs <- ggplot(matriculas_semestre) +
      geom_col(
        aes(
          x = semestre, y = matriculas,
          text = paste(
            "Semestre:", semestre,
            "<br>Matrículas:", matriculas
          )
        ),
        fill = "#08306B"
      ) +
      labs(x = "Semestre", y = "Número de matriculas") +
      scale_y_continuous(labels = scales::number_format(
        big.mark = ".",
        decimal.mark = ","
      ))
    
    ggplotly(plotmatri_ufrgs, tooltip = "text")
  })
  
  # Numero de Professores Barras ################################################
  
  output$prof_ufrgs <- renderPlotly({
    h3 <- hcl.colors(3, palette = "Berlin")
    
    result <- simulated_data %>%
      group_by(UNIDADE, DEPARTAMENTO, total_prof_ufrgs) %>%
      summarise(n_prof = sum(total_prof_ufrgs)) %>%
      group_by(UNIDADE) %>%
      summarise(n_prof = sum(total_prof_ufrgs)) %>%
      arrange(desc(n_prof))
    
    plotprof_ufrgs <- ggplot(
      result,
      aes(
        x = reorder(UNIDADE, n_prof),
        y = n_prof,
        text = paste(UNIDADE, "<br>Professores:", n_prof)
      )
    ) +
      geom_col(fill = "#08306B") +
      labs(x = "Unidade", y = "Número de professores") +
      coord_flip() +
      theme(
        axis.text.x = element_text(angle = 45, size = 7),
        legend.position = "none"
      )
    
    ggplotly(plotprof_ufrgs, tooltip = "text")
  })
  
  # Mais Matrículas por professor################################################
  output$prof_matri_ufrgs <- renderPlotly({
    top10 <- simulated_data %>%
      group_by(DEPARTAMENTO) %>%
      summarise(
        total_prof_ufrgs = round(mean(total_prof_ufrgs), 2),
        total_matri_ufrgs = round(mean(total_matri_ufrgs), 2),
        mean_matri = round(mean(num_matri_por_prof), 2)
      ) %>%
      arrange(desc(mean_matri)) %>%
      top_n(10)
    
    plotnum_matri_prof_ufrgs <- ggplot(top10) +
      geom_col(aes(
        x = reorder(DEPARTAMENTO, +mean_matri), y = mean_matri,
        text = paste(
          DEPARTAMENTO,
          "<br>Professores:", total_prof_ufrgs,
          "<br>Matrículas por semestre:", total_matri_ufrgs,
          "<br>Matrículas por Professor:", mean_matri
        )
      ), fill = "#08306B") +
      labs(x = "Departamento", y = "Número de matrículas por professor") +
      coord_flip() +
      theme(
        axis.text.x = element_text(angle = 45, size = 7),
        legend.position = "none"
      )
    
    ggplotly(plotnum_matri_prof_ufrgs, tooltip = "text")
  })
  
  # Menos Matrículas por professor #############################################
  
  output$less_prof_matri_ufrgs <- renderPlotly({
    less10 <- simulated_data %>%
      group_by(DEPARTAMENTO) %>%
      summarise(
        total_prof_ufrgs = round(mean(total_prof_ufrgs), 2),
        total_matri_ufrgs = round(mean(total_matri_ufrgs), 2),
        mean_matri = round(mean(num_matri_por_prof), 2)
      ) %>%
      arrange(desc(mean_matri)) %>%
      top_n(-10)
    
    
    plotnum_matri_prof_ufrgs <- ggplot(less10) +
      geom_col(aes(
        x = reorder(DEPARTAMENTO, -mean_matri), y = mean_matri,
        text = paste(
          DEPARTAMENTO,
          "<br>Professores:", total_prof_ufrgs,
          "<br>Matrículas por semestre:", total_matri_ufrgs,
          "<br>Matrículas por Professor:", mean_matri
        )
      ), fill = "#08306B") +
      labs(x = "Departamento", y = "Número de matrículas por professor") +
      coord_flip() +
      theme(
        axis.text.x = element_text(angle = 45, size = 7),
        legend.position = "none"
      )
    
    ggplotly(plotnum_matri_prof_ufrgs, tooltip = "text")
  })
  
  
  # Tabela #####################################################################
  output$ProfxAluno <- renderDataTable({
    output$ProfxAluno <- renderDataTable({
      datatable(
        head(simulated_data[-1]),
        colnames = c(
          "Departamentos", "Número de professores", "Matrículas",
          "Matrículas por professor"
        )
      )
      simulated_data[-1]
    })
  })
  
  # Série para Matriculas/profs ################################################
  
  evo <- reactive({
    gg <- simulated_data %>%
      filter(DEPARTAMENTO %in% input$seletor)
  })
  
  output$timeline <- renderPlotly({
    linhasds <- ggplot(evo(), aes(
      x = semestre, y = num_matri_por_prof, color = DEPARTAMENTO,
      text = paste(
        DEPARTAMENTO,
        "<br>Semestre", semestre,
        "<br>Professores:", total_prof_ufrgs,
        "<br>Matrículas por semestre:", total_matri_ufrgs,
        "<br>Matrículas por Professor:", num_matri_por_prof
      )
    )) +
      geom_point() +
      geom_line(aes(group = DEPARTAMENTO)) +
      labs(x = "Semestres", y = "Número de matrículas por professor")
    
    ggplotly(linhasds, tooltip = "text")
  })
  
  
  # ABA UNIDADES###############################################################
  # Value Boxes ################################################################
  
  aoao <- reactive({
    ff <- simulated_data %>%
      group_by(UNIDADE, DEPARTAMENTO, total_prof_ufrgs) %>%
      summarise(n_prof = sum(total_prof_ufrgs)) %>%
      group_by(UNIDADE) %>%
      summarise(n_prof = sum(total_prof_ufrgs)) %>%
      arrange(desc(n_prof)) %>%
      filter(UNIDADE %in% input$nitnit)
  })
  miaomiao <- reactive({
    ii <- simulated_data %>%
      group_by(UNIDADE, DEPARTAMENTO, num_matri_por_prof) %>%
      summarise(n_prof = sum(total_prof_ufrgs)) %>%
      group_by(UNIDADE) %>%
      summarise(mean_matri_prof = round(mean(num_matri_por_prof))) %>%
      filter(UNIDADE %in% input$nitnit)
  })
  
  masd <- reactive({
    jj <- simulated_data %>%
      group_by(UNIDADE, DEPARTAMENTO, total_matri_ufrgs) %>%
      summarise(n_prof = sum(total_prof_ufrgs)) %>%
      group_by(UNIDADE) %>%
      summarise(mean_matri = round(mean(total_matri_ufrgs))) %>%
      filter(UNIDADE %in% input$nitnit)
  })
  
  output$mean_matri <- renderValueBox({
    valueBox(masd()$mean_matri,
             subtitle = "Média de Matrículas por semestre"
    )
  })
  
  output$nprof <- renderValueBox({
    valueBox(aoao()$n_prof,
             subtitle = "Numero de Professores"
    )
  })
  
  output$mean_matri_proof <- renderValueBox({
    valueBox(miaomiao()$mean_matri_prof,
             subtitle = "Média de matrículas por professor"
    )
  })
  
  # Numero de Profs #############################################################
  reactive2 <- reactive({
    tt <- simulated_data %>%
      group_by(UNIDADE, DEPARTAMENTO, total_prof_ufrgs) %>%
      summarise(n_prof = sum(total_prof_ufrgs)) %>%
      group_by(UNIDADE, DEPARTAMENTO) %>%
      summarise(n_prof = sum(total_prof_ufrgs)) %>%
      filter(UNIDADE %in% input$nitnit)
  })
  
  output$undeprof <- renderPlotly({
    numdeprof <- ggplot(reactive2()) +
      geom_col(aes(
        x = reorder(DEPARTAMENTO, +n_prof), y = n_prof,
        text = paste(
          DEPARTAMENTO,
          "<br>Professores:", n_prof
        )
      ), fill = "#08306B") +
      coord_flip() +
      labs(y = "Número de Professores", x = "Departamentos")
    
    ggplotly(numdeprof, tooltip = "text")
  })
  
  # matriculas por prof por semestre ###########################################
  asas <- reactive({
    xx <- simulated_data %>%
      filter(UNIDADE %in% input$nitnit)
  })
  
  output$UnitTL <- renderPlotly({
    asasa <- ggplot(asas(), aes(
      x = semestre, y = num_matri_por_prof, color = DEPARTAMENTO,
      text = paste(
        "Departamento:", DEPARTAMENTO,
        "<br>Semestre:", semestre,
        "<br>Professores:", total_prof_ufrgs,
        "<br>Matrículas:", total_matri_ufrgs,
        "<br>Matrículas por Professor:", num_matri_por_prof
      )
    )) +
      geom_point() +
      geom_line(aes(group = DEPARTAMENTO)) +
      labs(x = "Semestre", y = "Número de Matrículas/Professor")
    
    ggplotly(asasa, tooltip = "text")
  })
  
  # Matriculas por semestre#####################################################
  hmm <- reactive({
    xx <- simulated_data %>%
      group_by(semestre) %>%
      filter(UNIDADE %in% input$nitnit)
  })
  
  PaletteBlues <- brewer.pal(n = 8, name = "Blues")
  
  output$UnitSemestre <- renderPlotly({
    unit <- ggplot(hmm()) +
      geom_col(
        aes(
          x = reorder(semestre, +total_matri_ufrgs), y = total_matri_ufrgs, fill = DEPARTAMENTO,
          text = paste(
            "Departamento:", DEPARTAMENTO,
            "<br>Semestre:", semestre,
            "<br>Matrículas:", total_matri_ufrgs
          )
        ),
        position = "stack"
      ) +
      coord_flip() +
      labs(x = "Semestre", y = "Matrículas") +
      scale_fill_manual(values = c(
        "#9ECAE1", "#08306B", "#DEEBF7", "#C6DBEF", "#6BAED6",
        "#4292C6", "#2171B5", "#08519C",
        "darkblue"
      )) # ar palheta de cores decente (ou criar?)
    
    ggplotly(unit, tooltip = "text")
  })
  
  # Matriculas por prof ###########################
  novamente <- reactive({
    uu <- simulated_data %>%
      group_by(UNIDADE, DEPARTAMENTO) %>%
      summarise(mean_matri_prof = round(mean(num_matri_por_prof), 2)) %>%
      filter(UNIDADE %in% input$nitnit)
  })
  
  output$UnitPercent <- renderPlotly({
    meanmatri <- ggplot(novamente()) +
      geom_col(aes(
        x = reorder(DEPARTAMENTO, +mean_matri_prof), y = mean_matri_prof,
        text = paste(
          "Departamento:", DEPARTAMENTO,
          "<br>Matrículas por professor:", mean_matri_prof
        )
      ), fill = "#08306B") +
      labs(y = "Média de matrículas por professor", x = "Departamentos", ) +
      coord_flip()
    
    ggplotly(meanmatri, tooltip = "text")
  })
}
