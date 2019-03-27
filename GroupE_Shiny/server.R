#Install libraries
library(shiny);
library(data.table);
library(ggplot2);
library(leaflet);



# Define server logic required to make data table and draw plots
shinyServer(function(input, output) {

  
  
  #Filter data table
  compute_data <- reactive({
    
    dt <- readRDS("/Users/vickylin/Desktop/MBD/Term 1/R/workgroup_project/solar_dataset.RData");
    dt <- as.data.table(dt);
    dt$Date <- as.Date(dt$Date, format = "%Y%m%d");
    dt <- dt[, 1:99];
    dt <- as.data.frame(dt);
    
    start <- which(dt$Date==input$dat_dates[1]);
    end <- which(dt$Date==input$dat_dates[2]);
    
    if (length(input$dat_dates) > 0 & length(input$dat_ws) > 0){
      dat <- dt[start:end, c("Date",input$dat_ws)];
    } else{
      dat <- data.table();
    }
    
    return(dat);
  })
  

  
  
  # Print 'data' table
  output$data = DT::renderDataTable(
    compute_data(), filter = 'top', rownames=FALSE)
  
  
  
  #Generate station map
  output$stationMap <- renderLeaflet({

    folder_path <- "/Users/vickylin/Desktop/MBD/Term 1/R/workgroup_project"
    station_info <- read.table(file.path(folder_path, "station_info.csv"), sep = ",", header = TRUE);
    station_info <- as.data.frame(station_info);
    
    mean_station <- data.frame(sapply(dt[1:5113,2:99], mean, na.rm=TRUE));
    station_info <- cbind(station_info, mean_station[,1])
    
    
    #Get the year based on input$station from ui.R
    st1 <- which(station_info[,1] == input$selected_t_ws);
    st2 <- which(station_info[,1] == input$selected_c_ws);
    
    map <- leaflet();
    map <- addTiles(map);
    map <- addCircleMarkers(map, lng = as.numeric(station_info[st1,3]), lat = as.numeric(station_info[st1,2]),
                            label=paste0(input$selected_t_ws," with production mean ",round(station_info[st1,5]/1000000, 1),"m"), 
                            labelOptions = labelOptions(noHide = T), radius = 7,
                            color = if(as.numeric(round(station_info[st1,5]/1000000, 1)) < 16) {"red"}
                            else if (as.numeric(round(station_info[st1,5]/1000000, 1)) >= 16 & as.numeric(round(station_info[st1,5]/1000000, 1)) < 17) {"green"}
                            else {"blue"});
    
    map <- addCircleMarkers(map, lng = as.numeric(station_info[st2,3]), lat = as.numeric(station_info[st2,2]),
                            label=paste0(input$selected_c_ws," with production mean ",round(station_info[st2,5]/1000000, 1),"m"), 
                            labelOptions = labelOptions(noHide = T), radius = 7,
                            color = if(as.numeric(round(station_info[st2,5]/1000000, 1)) < 16) {"red"}
                            else if (as.numeric(round(station_info[st2,5]/1000000, 1)) >= 16 & as.numeric(round(station_info[st2,5]/1000000, 1)) < 17) {"green"}
                            else {"blue"});

    map;
    
    
  })
  
  #generate daily production plot 
  output$prod_plot <- renderPlot({
    
    dt <- readRDS("/Users/vickylin/Desktop/MBD/Term 1/R/workgroup_project/solar_dataset.RData");
    dt <- as.data.table(dt);
    dt$Date <- as.Date(dt$Date, format = "%Y%m%d");
    dt <- dt[1:5113, 1:99];
    dt <- as.data.frame(dt);
    
    st1 <- which(colnames(dt)==input$selected_t_ws);
    st2 <- which(colnames(dt)==input$selected_c_ws);
    start <- which(dt$Date==input$dates[1]);
    end <- which(dt$Date==input$dates[2]);
    
    
    prod_plot <- ggplot(dt[start:end, ], aes(x = dt[start:end, "Date"], y = round(dt[start:end, st1]/1000000, 1))) + geom_line();
    prod_plot <- prod_plot + geom_line(aes(x = dt[start:end, "Date"], y = round(dt[start:end, st2]/1000000, 1)), color = "lightgreen");
    prod_plot <- prod_plot + labs(title = sprintf("Daily Production from %s to %s of Station %s (Black) vs. %s (Green)", 
                                                  as.character(input$dates[1]), as.character(input$dates[2]), as.character(input$selected_t_ws), as.character(input$selected_c_ws)),
                                  x="Date", y="Production (millions)");
    prod_plot <- prod_plot + theme(plot.title = element_text(face = "bold", size = 16, hjust = -0.45));
    prod_plot <- prod_plot + theme(axis.title = element_text(face = "bold"));
    prod_plot;

  })
  
})
