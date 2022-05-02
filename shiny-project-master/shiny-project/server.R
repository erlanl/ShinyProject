
# Define server logic required to draw a histogram
server <- function(input, output) {
    ################### INPUT ####################
    select_stock <- eventReactive(input$go, {
        
        stock_name <- input$stock
        twin <- input$true_date
        
        master_df <- read.csv('amazon.csv', stringsAsFactors = T) %>%
          group_by(Ano, Estado) %>%
          summarise(Numero = sum(Numero, na.rm = T)) %>%
          ungroup()
        
        df_stock <- master_df %>% 
            filter(Estado == stock_name)
        
        #Recebendo os dados da tela
        ano_inicial <- input$stock_ano_inicial
        ano_final <- input$stock_ano_final
        #Convertendon para inteiro
        ano_inicial <- as.numeric(ano_inicial)
        ano_final <- as.numeric(ano_final)
        
        ano_atual <- ano_inicial + 1
        
        #z <- df_stock %>% 
          #filter(between(Ano, ano_inicial, ano_final))
        
        z <- df_stock %>% 
          filter(Ano == ano_inicial)
        
        #Função que filtra os dados
        while(ano_atual <= ano_final){
          y <- df_stock %>% 
            filter(Ano == ano_atual)
          
          z <- bind_rows(z, y)
          ano_atual <- ano_atual + 1
        }
    
        return(z)
    })
    
    select_stock2 <- eventReactive(input$go, {
      
      stock_name <- input$stock
      twin <- input$true_date
      
      #Recebendo os dados da tela
      ano_inicial <- input$stock_ano_inicial
      ano_final <- input$stock_ano_final
      
      #Convertendon para inteiro
      ano_inicial <- as.numeric(ano_inicial)
      ano_final <- as.numeric(ano_final)
      
      master_df <- read.csv('amazon.csv', stringsAsFactors = T)
      
      df_stock <- master_df %>% 
        filter(Estado == stock_name) %>% 
        filter(between(Ano, ano_inicial, ano_final))
      
      print(df_stock)
      return(df_stock)
    })
    
    select_2_stocks <- eventReactive(input$go_comp, {
      
      stock_name1 <- input$Estado1

      stock_name2 <- input$Estado2

      #Recebendo os dados da tela
      ano_inicial <- input$stock_comp_ano_inicial
      ano_final <- input$stock_comp_ano_final
      #Convertendon para inteiro
      ano_inicial <- as.numeric(ano_inicial)
      ano_final <- as.numeric(ano_final)
      
      master1_df <- read.csv('amazon.csv', stringsAsFactors = T) %>%
        group_by(Ano, Estado) %>%
        summarise(Numero = sum(Numero, na.rm = T)) %>%
        ungroup()
  
      
      df1_stock <- master1_df %>% 
        filter(Estado == stock_name1)
      
      df2_stock <- master1_df %>% 
        filter(Estado == stock_name2)
      
      
      z <- df1_stock %>% 
      filter(between(Ano, ano_inicial, ano_final))

      z1 <- df2_stock %>% 
        filter(between(Ano, ano_inicial, ano_final))    
      
      print(z)
      print(z1)
      
      tabela <- bind_rows(z, z1)
      return(tabela)
      
    })
    
    ################ OUTPUT #####################
    Info_DataTable <- eventReactive(input$go,{
        df <- select_stock()
        

        Min <- min(df["Numero"])
        Max <- max(df["Numero"])
        Média <- mean(df$Numero)
        Mediana <- median(df$Numero)
        
        #obtem a tabela com frequencia das variaveis
        freq <- table(df["Numero"]);
        #obtem o nome da variavel que mais se repete
        moda <- names(table(df["Numero"]))[table(df["Numero"]) == max(table(df["Numero"]))]
        Moda <- as.numeric(moda)
        
        variancia <- var(df$Numero)
        Desvio <- variancia^(1/2)
        
        
        Estado <- input$stock
        
        df_tb <-  data.frame(Estado, Média, Moda, Mediana, Min, Max, Desvio)
        
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
    
    output$hist <- renderPlot({
        #All the inputs
        df <- select_stock()
        
        num <- df$Numero %>% na.omit() %>% as.numeric()
        num_min <- min(num)
        num_max <- max(num)
        
        ggplot(df, aes(x = Numero)) +
          geom_histogram(group = 1, binwidth = 100, color = "black", bins = 10000) + geom_bar(start = "count") + scale_x_continuous(limits = c(num_min, num_max), breaks = seq(0,num_max,500)) +
          theme(axis.text = element_text(angle = 90, vjust = 0))

    })
    
    output$sh <- renderPlot({
      df <- select_stock()
      
      ano_limite <- df$Ano %>% na.omit() %>% as.numeric()
      ano_limite_min <- min(ano_limite)
      ano_limite_max <- max(ano_limite)
      
      num <- df$Numero %>% na.omit() %>% as.numeric()
      num_min <- min(num)
      num_max <- max(num)
      
      ggplot(df, aes(x= Ano, y=Numero)) +
        geom_line(color = "red", size = 2)
    })
    
    output$box <- renderPlot({
      df <- select_stock2()
      
      ggplot(df, aes(x=as.factor(Ano), y=Numero)) + 
        geom_boxplot(fill="slateblue", alpha=0.5) + 
        xlab("Numero de Quimadas")
    })
    
    output$grafo <- renderPlot({
      df <- select_2_stocks()
      print(df)
      ggplot(df, aes(x= Ano, y=Numero, group = Estado, color = Estado)) +
        geom_line(size = 2)
      
    })
    
    output$barra <- renderPlot({
      df <- select_2_stocks()
      
      ggplot(df, aes(fill = Estado, y = Numero / 12, x = Ano)) +
        geom_bar(position = "dodge", stat="identity")
    })
    
    output$scat <- renderPlot({
      df <- select_2_stocks()
      
      ggplot(df, aes(x = Ano, y = Numero, color = Estado)) +
        geom_point(size = 6)
    })
}
