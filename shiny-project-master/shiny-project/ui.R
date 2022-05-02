

header <- dashboardHeader(title = "Projeto de Estatística")

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Métricas", tabName = "m", icon = icon("chart-line")),
        menuItem('Comparando Ações', tabName = 'comp', icon = icon('chart-bar'))
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = 'm',
                fluidRow(
                    box(title = 'Selecione suas opções', width=12, solidHeader = TRUE, status = "primary",
                        selectInput('stock', 'Estado', stock_list, multiple=FALSE),
                        selectInput('stock_ano_inicial', 'Ano Inicial', time_list, multiple=FALSE),
                        selectInput('stock_ano_final', 'Ano Final', time_list, multiple=FALSE),
                        actionButton('go', 'Submeter')
                        )
                ),
                fluidRow(
                    box(title = "Informações sobre o estado", width = 12, solidHeader = TRUE, status = "primary",
                        DTOutput('info')

                    )
                ),
                fluidRow(
                    box(title = "Queimadas ao longo dos anos", width = 12, solidHeader = TRUE, status = "primary",
                        plotOutput('sh'),
                        plotOutput('hist'),
                        plotOutput('box')
                    )
                ),
        ),
        tabItem(tabName = 'comp',
                fluidRow(
                    box(title = 'Selecione suas opções', width=12, solidHeader = TRUE, status='primary',
                        selectInput('Estado1', 'Ação', stock_list, multiple=FALSE),
                        selectInput('Estado2', 'Ação', stock_list, multiple=FALSE),
                        selectInput('stock_comp_ano_inicial', 'Ano Inicial', time_list, multiple=FALSE),
                        selectInput('stock_comp_ano_final', 'Ano Final', time_list, multiple=FALSE),
                        actionButton('go_comp', 'Submeter')
                    )
                ),            
                
                fluidRow(
                  box(title = "Queimadas ao longo dos anos", width = 12, solidHeader = TRUE, status = "primary",
                      plotOutput('grafo'),
                      plotOutput('barra'),
                      plotOutput('scat'),
                ),
        )
      )
    )
)

ui <- dashboardPage(
    skin = 'purple',
    header, sidebar, body)
