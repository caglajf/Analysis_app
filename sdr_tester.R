gc()
memory.limit(9999999999)
options(shiny.maxRequestSize=10000*1024^2)
library("openxlsx")
library(htmlwidgets) 
library(manipulateWidget)
library(rmarkdown)
library(shinycssloaders)
library(readr)
library(devtools)
library(data.table)
library(ggplot2)
library(summaryBox)
library(dataPreparation)
library(RColorBrewer)
library(dplyr)
library(shinyTime)
library(janitor)

library(shinydashboard)
library(plotly)
options(scipen=5) 



#train1_org <- read.xlsx('C:\\Users\\cagla.odabasi\\Desktop\\Battery Tester\\01_202_1.xlsx', 1, colNames = TRUE, sep.names =' ')
soc_ocv <- read.csv(file= 'soc_ocv.csv',header=T, sep=",")

ui <- dashboardPage(
  dashboardHeader(title="SDR Analysis"),
  
  dashboardSidebar(
   #img(src = "siro_logo.png", height = 40, width = 60),
    
    fileInput("csvs_file1",
              label="Upload first CSV here",
              multiple = TRUE), 
    
    fileInput("csvs_file2",
              label="Upload second CSV here ",
              multiple = TRUE), 
    
    # Input: Select separator ----
    radioButtons("sep", "CSV List Separator",
                 choices = c(Comma = ",",
                             Semicolon = ";"),
                 selected = ";"),
    # add download button
    downloadButton("downloadreport", "Download SDR Report"),
    downloadButton("download_plotly_v", "Download SDR based on V"),
    downloadButton("download_plotly_soc", "Download SDR based on SOC"),
    
    fluidPage(h3("Instructions:"),
             
              tags$li("Dataset file should be in CSV format. Time (s), current and voltage values should be included."),
    
            #  tags$li("Please rename your columns as indicated below:"), 
   
           #   tags$div(  "Pack voltage (V):BMS_Voltage",tags$br(),"Pack current (A):BMS_Current",tags$br(),"Cell Voltage (V, n=1:108):Voltage_n", tags$br(),"Cell Temperature (C, n=1:40):CELL_Temp_n", style = "font-size:12px;")
  
 )),
  
    
    
    

  
  # uiOutput(('result'))
  dashboardBody(
    
    verbatimTextOutput("contents"),
    fluidRow( box(status = "warning", "Current Profile File 1", shinycssloaders::withSpinner( plotlyOutput('current_file1', height=250))),box(status = "warning", "Current Profile File 2", shinycssloaders::withSpinner( plotlyOutput('current_file2', height=250)))),
    # fluidRow( box(status = "warning", "Min-Max Voltage Difference", shinycssloaders::withSpinner( plotlyOutput('min_max_V_diff', height=250))),box(status = "warning", "Voltage Standard Deviation",  shinycssloaders::withSpinner(plotlyOutput('voltage_std', height=250)))), 
    fluidRow(numericInput("time_select_file1", "Select time for File 1(sec):", 10, min = 0.1, max = 1000000), numericInput("time_select_file2", "Select time for File 2 (sec):", 10, min = 0.1, max = 1000000), numericInput("day_num", "Number of rest days:", 50, min = 1, max = 1000000)),
    fluidRow( box(status = "warning", "Voltage Values at Selected Time Stamp File 1", shinycssloaders::withSpinner( plotlyOutput('voltage_plot_inst_file1', height=250))), box(status = "warning", "Voltage Values at Selected Time Stamp File 2", shinycssloaders::withSpinner( plotlyOutput('voltage_plot_inst_file2', height=250)))),
    fluidRow( box(status = "warning", "SOC Values at Selected Time Stamp File 1", shinycssloaders::withSpinner( plotlyOutput('soc_plot_inst_file1', height=250))), box(status = "warning", "SOC Values at Selected Time Stamp File 2", shinycssloaders::withSpinner( plotlyOutput('soc_plot_inst_file2', height=250))),
    fluidRow( box(status = "warning", "SDR (mV/day)", shinycssloaders::withSpinner( plotlyOutput('sdr_plot', height=250))) , box(status = "warning", "SDR (%SOC/day)", shinycssloaders::withSpinner( plotlyOutput('soc_plot', height=250))) ))))

    
     #tabPanel("Temperature Information", 
   # fluidRow(box(status = "warning", "Min-Max Temperature",  shinycssloaders::withSpinner(plotlyOutput('min_max_T', height=250))), box(status = "warning", "Min-Max Temperature Difference", shinycssloaders::withSpinner( plotlyOutput('min_max_T_diff', height=250)))),
    #fluidRow( box(status = "warning", "Temperature Standard Deviation", shinycssloaders::withSpinner( plotlyOutput('temp_std', height=250)))))))
     #fluidRow( box(status = "warning", "Min-Max Temperature (Cold part)", shinycssloaders::withSpinner( plotlyOutput('min_max_T_cold', height=250))),box(status = "warning", "Min-Max Temperature (Hot part)", shinycssloaders::withSpinner( plotlyOutput('min_max_T_hot', height=250)))), 
     #fluidRow( box(status = "warning", "Min-Max Temperature Difference (Cold part)", shinycssloaders::withSpinner( plotlyOutput('min_max_T_cold_diff', height=250))),box(status = "warning", "Min-Max Temperature Difference (Hot part)", shinycssloaders::withSpinner( plotlyOutput('min_max_T_hot_diff', height=250)))),
   # fluidRow( box(status = "warning", "Temperature Standard Deviation (Cold part)", shinycssloaders::withSpinner( plotlyOutput('temp_std_cold', height=250))),box(status = "warning", "Temperature Standard Deviation (Hot part)",  shinycssloaders::withSpinner(plotlyOutput('temp_std_hot', height=250)))))))
   #  fluidRow( box(status = "warning", "Normalized temperature distribution after charge/discharge (Cold part)",  plotlyOutput('T_dist_cold', height=250)),box(status = "warning", "Normalized temperature distribution after charge/discharge (Hot part)",  plotlyOutput('T_dist_hot', height=250))),
   #  fluidRow( box(status = "warning", "Normalized temperature distribution during charge/discharge (All pack)",  plotlyOutput('T_dist_all', height=250)))
   # # fluidRow( box(status = "warning", "Normalized voltage final distribution",  plotlyOutput('V_dist_rest', height=250)),fluidRow(box(title = "Rest after Charge/Discharge (sec)",status = "primary", solidHeader = TRUE, collapsible = TRUE,box(status = "primary", textOutput('rest_time'),height=60), height = 115))), 
   #  ),
   # tabPanel("Coolant Analysis", 
   #          fluidRow( box(status = "warning", "Coolant Outlet Temperature",  plotlyOutput('coolant_outlet', height=250)),box(status = "warning", "Transferred Q to Coolant",  plotlyOutput('q', height=250)))) 
   #  
    

  
    


server <- function(input, output){
  
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
 
    train1_upload<-reactive({ validate(
      need(input$csvs_file1 != "", "Please upload a data file") )
      
      data.frame(rbindlist(lapply(input$csvs_file1$datapath, fread, sep=input$sep),
                use.names = TRUE, fill = TRUE))
     
    })
    
    
    train2_upload<-reactive({ validate(
      need(input$csvs_file2 != "", "Please upload a data file") )
      
      data.frame(rbindlist(lapply(input$csvs_file2$datapath, fread, sep=input$sep),
                           use.names = TRUE, fill = TRUE))
      
    })
    
    train1=reactive({df=train1_upload()
   
    for ( col in 1:ncol(df)){
      
      colnames(df)[col] <-  gsub("^[^..]*..", "", colnames(df)[col])
      colnames(df)[col] <-  gsub("^[^..]*..", "", colnames(df)[col])
      #colnames(df)[col] <-  sub("\\.[*", "", colnames(df)[col])
      
    }
    
    for ( col in 1:ncol(df)){
      colnames(df)[col] <-  gsub("\\..*", "", colnames(df)[col])
      
    }
    
    colnames(df)[1]=c("Time")
  df  = df[seq(1, nrow(df), 10), ] 
    return(df)
    
    })
    
    train2=reactive({df=train2_upload()
    
    for ( col in 1:ncol(df)){
      
      colnames(df)[col] <-  gsub("^[^..]*..", "", colnames(df)[col])
      colnames(df)[col] <-  gsub("^[^..]*..", "", colnames(df)[col])
      #colnames(df)[col] <-  sub("\\.[*", "", colnames(df)[col])
      
    }
    
    for ( col in 1:ncol(df)){
      colnames(df)[col] <-  gsub("\\..*", "", colnames(df)[col])
      
    }
    
    colnames(df)[1]=c("Time")
    df  = df[seq(1, nrow(df), 10), ] 
    return(df)
    
    })
    
    
    # train1_sorted= reactive({ a=train1()
    #  a[,'real_time']=as.POSIXct(paste(a[,'Date'], a[,'Time']), format="%d.%m.%Y %H:%M:%S")
    # a=a[ order(a[,'real_time']),]
    #  return(a)})
   #  train1_cols_others=reactive({train1()[c("Time","BMS_Voltage", "BMS_Current", "BMS_CoolantTemperatureOutlet", "BMS_SoC")]})
   # train1_cols_T=reactive({a=train1()[,  grepl( "CELL_Temp" , names( train1() ) )]
   #   a[a == "SNA"] <- 255
   #   return(a)})
   
   train1_V=reactive({voltage=train1()[,  grepl( "Voltage_" , names( train1() ) )]
     #remove constant value columns
  # voltage <- voltage[,-which_are_constant(voltage, verbose=FALSE)]
      return(voltage)})
   
   
   train2_V=reactive({voltage=train2()[,  grepl( "Voltage_" , names( train2() ) )]
   #remove constant value columns
  # voltage <- voltage[,-which_are_constant(voltage, verbose=FALSE)]
   return(voltage)})
  # train1_cols=reactive({cbind(train1_cols_others(), train1_cols_V(), train1_cols_T())})
   
   #  train1_org=reactive({a=as.data.frame(train1_cols())
   #  a$Time=a$Time-a$Time[1]
   # # a$time_row=1:nrow(a)
   #  a=na.omit(a)
   
   # a <-as.data.frame( apply(apply(a, 2, gsub, patt=",", replace="."), 2, as.numeric))
    
    
    # a_new = a[seq(1, nrow(a), 5), ] 
    
 #  a_new=a
    ####diff time in hour format
  #   a_new$diff_time=NA
  #   for (i in 1:(nrow(a_new)-1)){
  #     a_new$diff_time[i+1]= (a_new$Time[i+1]-a_new$Time[i])/3600
  #     
  #   }
  #   
  #   a_new$diff_time[1]=0
  #   
  #   a_new$capacity=NA
  #   a_new$energy = NA
  #   for (i in 1:(nrow(a_new)-1)){
  #     a_new$capacity[i]=a_new$diff_time[i+1]*a_new$BMS_Current[i]
  #     a_new$energy[i]=a_new$diff_time[i+1]*a_new$BMS_Current[i]*a_new$BMS_Voltage[i]
  #     #data$cap_step[i+1]=data$cap_step[i]+data$cap_step[i+1]
  #   }
  #   a_new$capacity[nrow(a_new)]=0
  #   a_new$capacity=round(cumsum(a_new$capacity),2)
  #   
  #   a_new$energy[nrow(a_new)]=0
  #   a_new$energy=round(cumsum(a_new$energy),2)
  #   
  #   
  #   #a_new$energy=round(a_new$capacity*a_new$BMS_Voltage,2)
  #   a_new$BMS_CoolantTemperatureInlet=25
  # return(a_new)})
  #   
  #   soc_start=reactive({head(train1_org()$BMS_SoC,1)})
  #   soc_final=reactive({tail(train1_org()$BMS_SoC,1)})
  #   output$start_soc=renderText({soc_start()})
  #   output$final_soc=renderText({soc_final()})
  #   pulse=reactive({ a=train1_org()[train1_org()$BMS_Current>0|train1_org()$BMS_Current<0,]
  #   
  #   if (sum(a$BMS_Current[1:50])>0){ a= a[a$BMS_Current>0 , ]} else {a=a[a$BMS_Current<0 , ]}
  #   
  #  # if (head(a$BMS_Current,1)>0){ a= a[a$BMS_Current>0 , ]} else {a=a[a$BMS_Current<0 , ]}
  #   return(a)
  #   })
  #   
  #   
    
    output$current_file1 <- renderPlotly({  p= qplot( train1()$Time, train1()$BMS_Current,  xlab= "Time (s)", ylab = "Current (A)")+
      geom_point(aes(x = train1()$Time, y= train1()$BMS_Current ))+
      theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0))+
      theme(legend.position = "none")
    
    
    ggplotly()%>% config(toImageButtonOptions = list( format = "svg", filename = "time_current_plot", width =1000,  height = 600))
    
    
    })
    
    output$current_file2 <- renderPlotly({  p= qplot( train2()$Time, train2()$BMS_Current,  xlab= "Time (s)", ylab = "Current (A)")+
      geom_point(aes(x = train2()$Time, y= train2()$BMS_Current ))+
      theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0))+
      theme(legend.position = "none")
    
    
    ggplotly()%>% config(toImageButtonOptions = list( format = "svg", filename = "time_current_plot", width =1000,  height = 600))
    
    
    })
    
    # pulse_end=reactive({tail(pulse(),1)})
    # 
    # data_end=reactive({tail(train1_org(),1)})
    # 
    # output$rest_time=reactive({data_end()$Time-pulse_end()$Time})
    # 
    # output$final_capacity=renderText({pulse_end()$capacity})
    # output$final_energy=renderText({max(pulse()$energy)})
    
    # output$V_dist_pulse=renderPlotly({ plot_ly(x=as.numeric(pulse_end()[ , grepl( "Voltage_" , names( pulse_end() ) )]), type="histogram",histnorm = "probability" ) })
    # output$V_dist=renderPlotly({ plot_ly(x=as.numeric(data_end()[ , grepl( "Voltage_" , names( data_end() ))]), type="histogram",histnorm = "probability" ) })
    # output$V_dist_rest=renderPlotly({ plot_ly(x=as.numeric(data_end()[ , grepl( "Voltage_" , names( data_end() ) )]), type="histogram", histnorm = "probability")})
    # output$T_dist=renderPlotly({ plot_ly(x=as.numeric(pulse_end()[ , grepl( "CELL_Temp" , names( pulse_end() ) )]), type="histogram", histnorm = "probability")})
    # 
    # output$T_dist_cold=renderPlotly({ plot_ly(x=as.numeric(tail(data_T_cold()[,1:20],1)), type="histogram" ,histnorm = "probability")})
    # output$T_dist_hot=renderPlotly({ plot_ly(x=as.numeric(tail(data_T_hot()[,1:20],1)), type="histogram", histnorm = "probability") })
    # output$T_dist_all=renderPlotly({
    #   plot_ly(x=pulse(), type="histogram" ,histnorm = "probability")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_001"], name="CELL_Temp_001")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_002"],name="CELL_Temp_002") %>% 
    #   add_histogram(x= pulse()[,"CELL_Temp_003"], name="CELL_Temp_003")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_004"], name="CELL_Temp_004")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_005"], name="CELL_Temp_005")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_006"], name="CELL_Temp_006")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_007"], name="CELL_Temp_007")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_008"], name="CELL_Temp_008")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_009"], name="CELL_Temp_009")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_010"], name="CELL_Temp_010")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_011"], name="CELL_Temp_011")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_012"], name="CELL_Temp_012")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_013"],name="CELL_Temp_013") %>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_014"], name="CELL_Temp_014")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_015"], name="CELL_Temp_015")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_016"], name="CELL_Temp_016")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_017"], name="CELL_Temp_017")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_018"], name="CELL_Temp_018")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_019"], name="CELL_Temp_019")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_020"], name="CELL_Temp_020")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_021"], name="CELL_Temp_021")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_022"], name="CELL_Temp_022")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_023"], name="CELL_Temp_023")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_024"],name="CELL_Temp_024") %>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_025"], name="CELL_Temp_025")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_026"], name="CELL_Temp_026")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_027"], name="CELL_Temp_027")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_028"], name="CELL_Temp_028")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_029"], name="CELL_Temp_029")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_030"], name="CELL_Temp_030")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_031"], name="CELL_Temp_031")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_032"], name="CELL_Temp_032")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_033"], name="CELL_Temp_033")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_034"], name="CELL_Temp_034")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_035"],name="CELL_Temp_035") %>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_036"], name="CELL_Temp_036")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_037"], name="CELL_Temp_037")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_038"], name="CELL_Temp_038")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_039"], name="CELL_Temp_039")%>% 
    #     add_histogram(x= pulse()[,"CELL_Temp_040"], name="CELL_Temp_040")
    #   
    #   })
    # 
    data_V_file1= reactive({voltage=train1()[,  grepl( "Voltage_" , names( train1() ) )] 
    #voltage <- voltage[,-which_are_constant(voltage, verbose=FALSE)]
    voltage$std=apply(voltage[,1:ncol(voltage)],1,sd)
   # voltage$avg=rowMeans(voltage[,1:ncol(voltage)])
    for (i in 1:nrow(voltage)){
      voltage$min_V[i]=min(voltage[i,1:(ncol(voltage)-3)])
      
      voltage$max_V[i]=max(voltage[i,1:(ncol(voltage)-3)])
    }
    voltage$min_V_colname  <- apply(voltage[,1:(ncol(voltage)-3)], 1, function(x) colnames(voltage[,1:(ncol(voltage)-3)])[which.min(x)])
    voltage$max_V_colname  <- apply(voltage[,1:(ncol(voltage)-3)], 1, function(x) colnames(voltage[,1:(ncol(voltage)-3)])[which.max(x)])
    voltage_all=cbind(voltage, train1()$Time)
    colnames(voltage_all)[ncol(voltage_all)] <- "Time"
    return(voltage_all)
    })
    
    
    data_V_file2= reactive({voltage=train2()[,  grepl( "Voltage_" , names( train2() ) )] 
   # voltage <- voltage[,-which_are_constant(voltage, verbose=FALSE)]
    voltage$std=apply(voltage[,1:ncol(voltage)],1,sd)
    # voltage$avg=rowMeans(voltage[,1:ncol(voltage)])
    for (i in 1:nrow(voltage)){
      voltage$min_V[i]=min(voltage[i,1:(ncol(voltage)-3)])
      
      voltage$max_V[i]=max(voltage[i,1:(ncol(voltage)-3)])
    }
    voltage$min_V_colname  <- apply(voltage[,1:(ncol(voltage)-3)], 1, function(x) colnames(voltage[,1:(ncol(voltage)-3)])[which.min(x)])
    voltage$max_V_colname  <- apply(voltage[,1:(ncol(voltage)-3)], 1, function(x) colnames(voltage[,1:(ncol(voltage)-3)])[which.max(x)])
    voltage_all=cbind(voltage, train2()$Time)
    colnames(voltage_all)[ncol(voltage_all)] <- "Time"
    return(voltage_all)
    })
  
    time_row_select_file1=reactive({v_select=t(data_V_file1()[data_V_file1()$Time==input$time_select_file1,1:(ncol(data_V_file1())-6)])
    v_select <- cbind(rownames(v_select), data.frame(v_select, row.names=NULL))
    colnames(v_select)=c("Voltage_sensor", "Voltage") 
    v_select=v_select[v_select$Voltage<4.5,]
    v_select$soc=NA
     for (i in 1:nrow(v_select)){
    # Use predict to estimate the values for aim.
    # Note that predict expects a data.frame and the col 
    # names need to match
       if (v_select$Voltage[i] >3.337){
         v_select$soc[i] <-116.82* v_select[i,]$Voltage- 385.94} else if (3.2<v_select$Voltage[i] & v_select$Voltage[i] <=3.337){
         v_select$soc[i] <-(939.84*(v_select[i,]$Voltage^3) - 9169*(v_select[i,]$Voltage^2) + 29836*(v_select[i,]$Voltage) - 32381) } else if (v_select$Voltage[i] <=3.2){
           v_select$soc[i] <-(11.928*v_select[i,]$Voltage - 36.614 )}
    # for (i in 1:nrow(v_select)){
    #   
    #   v_select$soc[i]=soc_ocv[which.min(abs(v_select[i,]$Voltage-soc_ocv$OCV)),]$SOC
     }
    return(v_select)})
    
    time_row_select_file2=reactive({v_select=t(data_V_file2()[data_V_file2()$Time==input$time_select_file2,1:(ncol(data_V_file2())-6)])
    v_select <- cbind(rownames(v_select), data.frame(v_select, row.names=NULL))
    colnames(v_select)=c("Voltage_sensor", "Voltage")
    v_select=v_select[v_select$Voltage<4.5,]
    v_select$soc=NA
    for (i in 1:nrow(v_select)){
      if (v_select$Voltage[i] >3.337){
        v_select$soc[i] <-116.82* v_select[i,]$Voltage- 385.94} else if (3.2< v_select$Voltage[i] & v_select$Voltage[i]<=3.337){
          v_select$soc[i] <-(939.84*(v_select[i,]$Voltage^3) - 9169*(v_select[i,]$Voltage^2) + 29836*(v_select[i,]$Voltage) - 32381) } else if (v_select$Voltage[i] <=3.2){
            v_select$soc[i] <-(11.928*v_select[i,]$Voltage - 36.614 )}
      # for (i in 1:nrow(v_select)){
      #   
      #   v_select$soc[i]=soc_ocv[which.min(abs(v_select[i,]$Voltage-soc_ocv$OCV)),]$SOC
    }
    return(v_select)})
    
    session_store_v <- reactiveValues()
    session_store_soc <- reactiveValues()
    output$soc_plot_inst_file1 =renderPlotly({
       validate(
        need(input$csvs_file1 != "", "Please upload a data file") )      
     validate( need(time_row_select_file1(), "No data found"
      ))

      p=qplot( time_row_select_file1()$Voltage_sensor, time_row_select_file1()$soc,  xlab= "Voltage sensors", ylab = "SOC(%)")+
        geom_point(aes(x = time_row_select_file1()$Voltage_sensor, y= time_row_select_file1()$soc, colour=time_row_select_file1()$soc<0 ))+
        scale_colour_manual(values = c("black", "red"))+
        theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0))+
        theme(legend.position = "none",axis.text.x=element_blank())
      
      
      ggplotly(p)%>% config(toImageButtonOptions = list( format = "svg", filename = "instant_voltage_dist", width =1000,  height = 600))})
    
    output$soc_plot_inst_file2 =renderPlotly({
      validate(
        need(input$csvs_file1 != "", "Please upload a data file") )
      p=qplot( time_row_select_file2()$Voltage_sensor, time_row_select_file2()$soc,  xlab= "Voltage sensors", ylab = "SOC(%)")+
        geom_point(aes(x = time_row_select_file2()$Voltage_sensor, y= time_row_select_file2()$soc, colour=time_row_select_file2()$soc<0))+
        scale_colour_manual(values = c("black", "red"))+
        theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0))+
        theme(legend.position = "none",axis.text.x=element_blank())
      
      
      ggplotly(p)%>% config(toImageButtonOptions = list( format = "svg", filename = "instant_voltage_dist", width =1000,  height = 600))})
    
    output$soc_plot =renderPlotly({
      validate(
        need(input$csvs_file1 != "", "Please upload a data file") )
      p=qplot( sdr_soc()$Voltage_sensor, sdr_soc()$sdr_soc,  xlab= "Voltage sensors", ylab = "SDR (%SOC/day)")+
        geom_point(aes(x = sdr_soc()$Voltage_sensor, y= sdr_soc()$sdr_soc, colour=sdr_soc()$sdr_soc>0.016))+
        scale_colour_manual(values = c("black", "red"))+
        theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0))+
        theme(legend.position = "none",axis.text.x=element_blank())
      # convert the graph to plotly graph and put it in session store
      session_store_soc$plt <- ggplotly(p)
      
      ggplotly(p)%>% config(toImageButtonOptions = list( format = "svg", filename = "instant_voltage_dist", width =1000,  height = 600))})
    
    
    sdr=reactive({sdr=as.data.frame(abs(time_row_select_file1()$Voltage-time_row_select_file2()$Voltage)*1000/input$day_num)
      sdr=data.frame(cbind(time_row_select_file1()$Voltage_sensor, sdr))
      colnames(sdr)=c("Voltage_sensor", "sdr")
      return(sdr)})
    
    sdr_soc=reactive({sdr_soc=as.data.frame(abs(time_row_select_file1()$soc-time_row_select_file2()$soc)/input$day_num)
    sdr_soc=data.frame(cbind(time_row_select_file1()$Voltage_sensor, sdr_soc))
    colnames(sdr_soc)=c("Voltage_sensor", "sdr_soc")
    return(sdr_soc)})
    
    output$sdr_plot =renderPlotly({
      validate(
        need(input$csvs_file1 != "", "Please upload a data file") )
      p=qplot( sdr()$Voltage_sensor, sdr()$sdr,  xlab= "Voltage sensors", ylab = "SDR(mV/day)")+
        geom_point(aes(x = sdr()$Voltage_sensor, y= sdr()$sdr, colour=sdr()$sdr>0.108 ))+
        scale_colour_manual(values = c("black", "red"))+
        theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0))+
        theme(legend.position = "none",axis.text.x=element_blank())
      
      # convert the graph to plotly graph and put it in session store
      session_store_v$plt <- ggplotly(p)
      ggplotly(p)%>% config(toImageButtonOptions = list( format = "svg", filename = "instant_voltage_dist", width =1000,  height = 600))})
      
    output$voltage_plot_inst_file1 =renderPlotly({
      validate(
        need(input$csvs_file1 != "", "Please upload a data file") )
    p=qplot( time_row_select_file1()$Voltage_sensor, time_row_select_file1()$Voltage,  xlab= "Voltage sensors", ylab = "Voltage(V)")+
      geom_point(aes(x = time_row_select_file1()$Voltage_sensor, y= time_row_select_file1()$Voltage, colour=time_row_select_file1()$Voltage<3.066 ))+
      scale_colour_manual(values = c("black", "red"))+
      theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0))+
      theme(legend.position = "none",axis.text.x=element_blank())
      
    
    ggplotly(p)%>% config(toImageButtonOptions = list( format = "svg", filename = "instant_voltage_dist", width =1000,  height = 600))})
    
    output$voltage_plot_inst_file2 =renderPlotly({
      validate(
        need(input$csvs_file2 != "", "Please upload a data file") )
      p=qplot( time_row_select_file2()$Voltage_sensor, time_row_select_file2()$Voltage,  xlab= "Voltage sensors", ylab = "Voltage(V)")+
        geom_point(aes(x = time_row_select_file2()$Voltage_sensor, y= time_row_select_file2()$Voltage, colour=time_row_select_file2()$Voltage<3.066 ))+
        scale_colour_manual(values = c("black", "red"))+
        theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0))+
        theme(legend.position = "none",axis.text.x=element_blank())
      
      
      ggplotly(p)%>% config(toImageButtonOptions = list( format = "svg", filename = "instant_voltage_dist", width =1000,  height = 600))})
    
    # data_V_pulse= reactive({voltage=pulse()[ , grepl( "Voltage_" , names( pulse() ) )] 
    # voltage$std=apply(voltage[,1:108],1,sd)
    # voltage$avg=rowMeans(voltage[,1:108])
    # for (i in 1:nrow(voltage)){
    #   voltage$min_V[i]=min(voltage[i,1:108])
    #   
    #   voltage$max_V[i]=max(voltage[i,1:108])
    # }
    # voltage$min_V_colname  <- apply(voltage[,1:108], 1, function(x) colnames(voltage[,1:108])[which.min(x)])
    # voltage$max_V_colname  <- apply(voltage[,1:108], 1, function(x) colnames(voltage[,1:108])[which.max(x)])
    # voltage_all=cbind(voltage,pulse()$Time)
    # colnames(voltage_all)[ncol(voltage_all)] <- "Time"
    # return(voltage_all)
    # })
    # NOTE: downloadHandler must be assigned to the ID of your downloadButton
    output$downloadreport <- downloadHandler(
      filename = function() paste0("SDR Report", ".html"),
      content = function(file) {
        tempreport <- file.path(tempdir(), "sdr_analysis_report.rmd")
        file.copy("sdr_analysis_report.rmd", tempreport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        #  these can be any time of R objects
        #  that you want your .Rmd to have access to
        params <- list(sdr_v= sdr(), 
                       sdr_s= sdr_soc(),
                       time1= time_row_select_file1(),
                       time2= time_row_select_file2())
                       
        # Knit the document, passing in the `params` list, and eval it in a
        #  child of the global environment (this isolates the code in the document
        #  from the code in this app).
        rmarkdown::render(tempreport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }     
    )
    # output$downloadReport <- downloadHandler(
    #   filename = function() {
    #     paste("SDR_Report", Sys.Date(), ".html", sep = "")
    #   },
    #   content = function(file) {
    #     # export plotly html widget as a temp file to download.
    #     saveWidget(as_widget(session_store_v$plt), file, selfcontained = TRUE)
    #   }
    #)
    
    output$download_plotly_v <- downloadHandler(
      filename = function() {
        paste("sdr_v", Sys.Date(), ".html", sep = "")
      },
      content = function(file) {
        # export plotly html widget as a temp file to download.
        saveWidget(as_widget(session_store_v$plt), file, selfcontained = TRUE)
      }
    )
    output$download_plotly_soc <- downloadHandler(
      filename = function() {
        paste("sdr_soc", Sys.Date(), ".html", sep = "")
      },
      content = function(file) {
        # export plotly html widget as a temp file to download.
        saveWidget(as_widget(session_store_soc$plt), file, selfcontained = TRUE)
      }
    )
    
     data_T= reactive({temp=train1()[ , grepl( "CELL_Temp" , names( train1() ) )]
       temp <- temp[,-which_are_constant(temp, verbose=FALSE)]
     temp$std=apply(temp[,1:ncol(temp)],1,sd)
   #  temp$avg=rowMeans(temp[,1:40])
     for (i in 1:nrow(temp)){
       temp$min_T[i]=min(temp[i,1:(ncol(temp)-3)])
       
       temp$max_T[i]=max(temp[i,1:(ncol(temp)-3)])
     }
     temp$min_T_colname  <- apply(temp[,1:(ncol(temp)-3)], 1, function(x) colnames(temp[,1:(ncol(temp)-3)])[which.min(x)])
     temp$max_T_colname  <- apply(temp[,1:(ncol(temp)-3)], 1, function(x) colnames(temp[,1:(ncol(temp)-3)])[which.max(x)])
     temp_all=cbind(temp, train1()$Time)
     colnames(temp_all)[ncol(temp_all)] <- "Time"
     return(temp_all)
     })
     
   #  
     data_T_cold= reactive({temp_cold=data_T()[ ,c(1,4,5,8,9,12,13,16,17,20,21,24,25,28,29,32,33,36,37,40)] 
     temp_cold <- temp_cold[,-which_are_constant(temp_cold, verbose=FALSE)]
     temp_cold$std=apply(temp_cold[,1:ncol(temp_cold)],1,sd)
     #temp_cold$avg=rowMeans(temp_cold[,1:ncol(temp_cold)])
     for (i in 1:nrow(temp_cold)){
       temp_cold$min_T[i]=min(temp_cold[i,1:(ncol(temp_cold)-3)])
       
       temp_cold$max_T[i]=max(temp_cold[i,1:(ncol(temp_cold)-3)]) }
     
     
      temp_cold$min_T_colname  <- apply(temp_cold[,1:(ncol(temp_cold)-3)], 1, function(x) colnames(temp_cold[,1:(ncol(temp_cold)-3)])[which.min(x)])
     temp_cold$max_T_colname  <- apply(temp_cold[,1:(ncol(temp_cold)-3)], 1, function(x) colnames(temp_cold[,1:(ncol(temp_cold)-3)])[which.max(x)])
     temp_all_cold=cbind(temp_cold, train1()$Time)
     colnames(temp_all_cold)[ncol(temp_all_cold)] <- "Time"
     return(temp_all_cold)
     })
   #  
     data_T_hot= reactive({temp_hot=data_T()[ ,c(2,3,6,7,10,11,14,15,18,19,22,23,26,27,30,31,34,35,38,39)] 
     temp_hot$std=apply(temp_hot[,1:ncol(temp_hot)],1,sd)
     #temp_hot$avg=rowMeans(temp_hot[,1:ncol(temp_cold)])
     for (i in 1:nrow(temp_hot)){
       temp_hot$min_T[i]=min(temp_hot[i,1:(ncol(temp_hot)-3)])
       
       temp_hot$max_T[i]=max(temp_hot[i,1:(ncol(temp_hot)-3)])
     }
     
     temp_hot$min_T_colname  <- apply(temp_hot[,1:(ncol(temp_hot)-3)], 1, function(x) colnames(temp_hot[,1:(ncol(temp_hot)-3)])[which.min(x)])
    temp_hot$max_T_colname  <- apply(temp_hot[,1:(ncol(temp_hot)-3)], 1, function(x) colnames(temp_hot[,1:(ncol(temp_hot)-3)])[which.max(x)])
     temp_all_hot=cbind(temp_hot, train1()$Time)
     colnames(temp_all_hot)[ncol(temp_all_hot)] <- "Time"
     return(temp_all_hot)
     })
   #  
   #  
   #  q_transfer=reactive({((15*1.113*1000*2.433)/60)*(pulse()$BMS_CoolantTemperatureOutlet-25)})
   #  
   #  
   #  
   #  output$condition = renderText(if(pulse_end()$BMS_Current< 0) paste( "Discharge (-)")  else if(pulse_end()$BMS_Current > 0) paste("Charge (+)") )
   # # 
  output$min_max_V <- renderPlotly({  p= qplot( data_V()$Time, data_V()$min_V,  xlab= "Time (s)", ylab = "Voltage(V)", showlegend = TRUE)+
      geom_point(aes(x = data_V()$Time, y= data_V()$min_V, z=data_V()$min_V_colname , color="blue" ))+
      theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0))+
      theme(legend.position = "none")
      p= p%>% add_trace(y=data_V()$max_V)+
        geom_point(aes(x = data_V()$Time, y= data_V()$max_V, z=data_V()$max_V_colname  , color="orange"))+
        theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0))+
        theme(legend.position = "none")
    
    ggplotly(p)%>% config(toImageButtonOptions = list( format = "svg", filename = "min_max_V", width =1000,  height = 600))
    
    
  })
  
  output$min_max_V_diff <- renderPlotly({  p= qplot( data_V()$Time, (data_V()$max_V-data_V()$min_V),  xlab= "Time (s)", ylab = "Voltage(V)", showlegend = TRUE)+
    geom_point(aes(x = data_V()$Time, y= (data_V()$max_V-data_V()$min_V), z=data_V()$Time , color="blue" ))+
    theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0))+
    theme(legend.position = "none")
  p= p%>% add_trace(y=data_V()$max_V)+
    geom_point(aes(x = data_V()$Time, y= (data_V()$max_V-data_V()$min_V), z=data_V()$Time  , color="orange"))+
    theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0))+
    theme(legend.position = "none")
  
  ggplotly(p)%>% config(toImageButtonOptions = list( format = "svg", filename = "min_max_V", width =1000,  height = 600))
  
  
  })
  
  output$min_max_T <- renderPlotly({  p= qplot( data_T()$Time, data_T()$min_T,  xlab= "Time (s)", ylab = "Temperature (C)")+
    geom_point(aes(x = data_T()$Time, y= data_T()$min_T, z=data_T()$min_T_colname , color="blue"))+
    theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0))+
    theme(legend.position = "none")
  p= p%>% add_trace(y=data_T()$max_T)+
    geom_point(aes(x = data_T()$Time, y= data_T()$max_T, z=data_T()$max_T_colname, color="orange" ))+
    theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0))+
    theme(legend.position = "none")
  
  ggplotly(p)%>% config(toImageButtonOptions = list( format = "svg", filename = "time_T_plot_cycle", width =1000,  height = 600))
  
  
  })
  # 
  # 
  # 
   output$min_max_T_cold <- renderPlotly({  p= qplot( data_T_cold()$Time, data_T_cold()$min_T,  xlab= "Time (s)", ylab = "Temperature (C)")+
     geom_point(aes(x = data_T_cold()$Time, y= data_T_cold()$min_T, z=data_T_cold()$min_T_colname , color="blue"))+
     theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0))+
     theme(legend.position = "none")
   p= p%>% add_trace(y=data_T_cold()$max_T)+
     geom_point(aes(x = data_T_cold()$Time, y= data_T_cold()$max_T, z=data_T_cold()$max_T_colname, color="orange" ))+
     theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0))+
     theme(legend.position = "none")
   
   ggplotly(p)%>% config(toImageButtonOptions = list( format = "svg", filename = "time_P_plot_cycle", width =1000,  height = 600))
   
   
   })
  # 
   output$min_max_T_hot <- renderPlotly({  p= qplot( data_T_hot()$Time, data_T_hot()$min_T,  xlab= "Time (s)", ylab = "Temperature (C)")+
     geom_point(aes(x = data_T_hot()$Time, y= data_T_hot()$min_T, z=data_T_hot()$min_T_colname ), color="blue")+
     theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0))+
     theme(legend.position = "none")
   p= p%>% add_trace(y=data_T_hot()$max_T)+
     geom_point(aes(x = data_T_hot()$Time, y= data_T_hot()$max_T, z=data_T_hot()$max_T_colname ), color="orange")+
     theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0))+
     theme(legend.position = "none")
   
   ggplotly(p)%>% config(toImageButtonOptions = list( format = "svg", filename = "time_P_plot_cycle", width =1000,  height = 600))
   
   
   })
 
   output$min_max_T_diff <- renderPlotly({  p= qplot( data_T()$Time, (data_T()$max_T-data_T()$min_T),  xlab= "Time (s)", ylab = "Temperature difference (C)")+
     geom_point(aes(x = data_T()$Time, y= (data_T()$max_T-data_T()$min_T), color="blue"))+
     theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0))+
     theme(legend.position = "none")
   
   
   ggplotly(p)%>% config(toImageButtonOptions = list( format = "svg", filename = "time_T_diff", width =1000,  height = 600))
   
   })  
   
   output$min_max_T_hot_diff <- renderPlotly({  p= qplot( data_T_hot()$Time, (data_T_hot()$max_T-data_T_hot()$min_T),  xlab= "Time (s)", ylab = "Temperature difference (C)")+
     geom_point(aes(x = data_T_hot()$Time, y= (data_T_hot()$max_T-data_T_hot()$min_T), color="blue"))+
     theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0))+
     theme(legend.position = "none")

   
   ggplotly(p)%>% config(toImageButtonOptions = list( format = "svg", filename = "time_T_diff_hot", width =1000,  height = 600))
   
   })
   
   
   output$min_max_T_cold_diff <- renderPlotly({  p= qplot( data_T_cold()$Time, (data_T_cold()$max_T-data_T_cold()$min_T),  xlab= "Time (s)", ylab = "Temperature difference (C)")+
     geom_point(aes(x = data_T_cold()$Time, y= (data_T_cold()$max_T-data_T_cold()$min_T), color="blue"))+
     theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0))+
     theme(legend.position = "none")
   
   
   ggplotly(p)%>% config(toImageButtonOptions = list( format = "svg", filename = "time_T_diff_cold", width =1000,  height = 600))
   
   })
  # 
  # 
  # 
  
  output$voltage_std <- renderPlotly({  p= qplot( data_V()$Time, data_V()$std,  xlab= "Time (s)", ylab = "Voltage standard deviation")+
    geom_point(aes(x = data_V()$Time, y= data_V()$std ))+
    theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0))+
    theme(legend.position = "none")
 
  
  ggplotly()%>% config(toImageButtonOptions = list( format = "svg", filename = "voltage_std", width =1000,  height = 600))
  
  
  })
  
  
  
   output$temp_std <- renderPlotly({  p= qplot( data_T()$Time, data_T()$std,  xlab= "Time (s)", ylab = "Temperature standard deviation")+
     geom_point(aes(x = data_T()$Time, y= data_T()$std ))+
     theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0))+
     theme(legend.position = "none")
   
   
   ggplotly()%>% config(toImageButtonOptions = list( format = "svg", filename = "time_P_plot_cycle", width =1000,  height = 600))
   
   
   })
   
  # 
  # 
   output$temp_std_cold <- renderPlotly({  p= qplot( data_T_cold()$Time, data_T_cold()$std,  xlab= "Time (s)", ylab = "Temperature standard deviation")+
     geom_point(aes(x = data_T_cold()$Time, y= data_T_cold()$std ))+
     theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0))+
     theme(legend.position = "none")
   
   
   ggplotly()%>% config(toImageButtonOptions = list( format = "svg", filename = "time_P_plot_cycle", width =1000,  height = 600))
  # 
  # 
   })
  # 
  # 
   output$temp_std_hot <- renderPlotly({  p= qplot( data_T_hot()$Time, data_T_hot()$std,  xlab= "Time (s)", ylab = "Temperature standard deviation")+
     geom_point(aes(x = data_T_hot()$Time, y= data_T_hot()$std ))+
     theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0))+
     theme(legend.position = "none")
   
  # 
   ggplotly()%>% config(toImageButtonOptions = list( format = "svg", filename = "time_P_plot_cycle", width =1000,  height = 600))
  # 
  # 
   })
  # 
  # output$coolant_outlet <- renderPlotly({  p= qplot(pulse()$Time, pulse()$BMS_CoolantTemperatureOutlet,  xlab= "Time (s)", ylab = "Coolant Temperature Outlet (C)")+
  #   geom_point(aes(x = pulse()$Time, y= pulse()$BMS_CoolantTemperatureOutlet ))+
  #   theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0))+
  #   theme(legend.position = "none")
  # 
  # 
  # ggplotly(p)%>% config(toImageButtonOptions = list( format = "svg", filename = "time_P_plot_cycle", width =1000,  height = 600))
  # 
  # 
  # })
  # 
  # output$q <- renderPlotly({  p= qplot(pulse()$Time, q_transfer(),  xlab= "Time (s)", ylab = "Transferred Heat (W)")+
  #   geom_point(aes(x = pulse()$Time, y= q_transfer() ))+
  #   theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0))+
  #   theme(legend.position = "none")
  # 
  # 
  # ggplotly(p)%>% config(toImageButtonOptions = list( format = "svg", filename = "time_P_plot_cycle", width =1000,  height = 600))
  # 
  # 
  # })
  
  
  
    }
  


shinyApp(ui = ui, server = server)