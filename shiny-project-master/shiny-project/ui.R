

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
                    box(title = 'Selecione suas opções', width=12, solidHeader = TRUE, status='warning',
                        selectInput('stock', 'Ação', stock_list, multiple=FALSE),
                        selectInput('stock_ano_inicial', 'Ano Inicial', time_list, multiple=FALSE),
                        selectInput('stock_ano_final', 'Ano Final', time_list, multiple=FALSE),
                        #uiOutput("timedate"),
                        actionButton('go', 'Submeter')
                        )
                ),
                fluidRow(
                    box(title = "Informações sobre a ação", width = 12, solidHeader = TRUE,
                        DTOutput('info')

                    )
                ),
                fluidRow(
                    box(title = "Série de Preços", width = 12, solidHeader = TRUE,
                        plotOutput('sh')
                    )
                ),
        ),
        tabItem(tabName = 'comp',
                fluidRow(
                    box(title = 'Selecione suas opções', width=12, solidHeader = TRUE, status='warning',
                        selectInput('stock_comp', 'Ação', stock_list, multiple=TRUE),
                        selectInput('stock_comp_ano_inicial', 'Ano Inicial', time_list, multiple=FALSE),
                        selectInput('stock_comp_ano_final', 'Ano Final', time_list, multiple=FALSE),
                        #uiOutput("timedate_comp"),
                        actionButton('go_comp', 'Submeter')
                    )
                ),            
        )
    )
)

ui <- dashboardPage(
    skin = 'red',
    header, sidebar, body)
