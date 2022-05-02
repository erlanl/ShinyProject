
# Define server logic required to draw a histogram
server <- function(input, output) {
    ################### INPUT ####################
    select_stock <- eventReactive(input$go, {
        
        stock_name <- input$stock
        twin <- input$true_date
        
        df_stock <- master_df %>% 
            filter(Estado == stock_name)
        
        #Recebendo os dados da tela
        ano_inicial <- input$stock_ano_inicial
        ano_final <- input$stock_ano_final
        #Convertendon para inteiro
        ano_inicial <- as.numeric(ano_inicial)
        ano_final <- as.numeric(ano_final)
        
        ano_atual <- ano_inicial + 1
        
        z <- df_stock %>% 
          filter(Ano == ano_inicial)
        
        #Função que filtra os dados
        while(ano_atual <= ano_final){
          y <- df_stock %>% 
            filter(Ano == ano_atual)
          
          z <- bind_rows(z, y)
          ano_atual <- ano_atual + 1
        }
    
   
        
        master_df <- read.csv('amazon.csv', stringsAsFactors = T) %>%
          group_by(Ano, Estado) %>%
          summarise(Numero = sum(Numero, na.rm = T)) %>%
          ungroup()

        print(z)
        return(z)
    })
    
    
    ################ OUTPUT #####################
    Info_DataTable <- eventReactive(input$go,{
        df <- select_stock()
        

        min <- min(df["Numero"])
        max <- max(df["Numero"])
        mean <- mean(df$Numero)
        median <- median(df$Numero)
        
        #obtem a tabela com frequencia das variaveis
        freq <- table(df["Numero"]);
        #obtem o nome da variavel que mais se repete
        moda <- names(table(df["Numero"]))[table(df["Numero"]) == max(table(df["Numero"]))]
        moda <- as.numeric(moda)
        
        variancia <- var(df$Numero)
        desvio <- variancia^(1/2)
        
        
        Stock <- input$stock
        
        df_tb <-  data.frame(Stock, mean, moda, median, min, max, desvio)
        
        df_tb <- as.data.frame(t(df_tb))


        return(df_tb)
    })
    
    output$info <- renderDT({
        Info_DataTable() %>%
            as.data.frame() %>% 
            DT::datatable(options=list(
                language=list(
                    url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
                )
            ))
    })
    
    #output$sh <- renderPlot({
        ## All the inputs
        #df <- select_stock()
        
        #aux <- df$Close %>% na.omit() %>% as.numeric()
        #aux1 <- min(aux)
        #aux2 <- max(aux)
        
        #df$Date <- ymd(df$Date)
        #a <- df %>% 
        #    ggplot(aes(Date, Close, group=1)) +
        #    geom_path() +
        #    ylab('Preço da Ação em $') +
        #    coord_cartesian(ylim = c(aux1, aux2)) +
        #    theme_bw() +
        #    scale_x_date(date_labels = "%Y-%m-%d")
        
        #a
    #})
}
