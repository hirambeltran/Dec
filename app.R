
# instructions to deploy the app in GitHub
# https://docs.posit.co/connect-cloud/how-to/r/shiny-r.html


# after checking that the app runs fine locally, then run the code below to create the file "manifest.json"
# the manifest file also goes in the same folder in GitHub with the app file
# VERY IMPORTANT: the file with the app must be named "app.R", otherwise the file is not published
  # rsconnect::writeManifest(
  #  appDir = getwd(),
  #   appFiles = c("app.R", "Decomp_res_e0_edagger_sd10_long_format.RData"), # Add "data.csv" here if you have one!
  #   appPrimaryDoc = "app.R"
  # )

# then  Push to GitHub

# Deploy to Posit Connect Cloud


library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

# Load data
# setwd("C:\\Users\\HBS\\Box\\Beltran-Sanchez\\Book Mortality in LAC\\Chapter_causes_of_death\\Comparing decomp with and without unknown")
load("Decomp_res_e0_edagger_sd10_long_format.RData")
df <- e0.cause2
df2 <- edagger.cause2
df3 <- sd10.cause2

# Readable cause labels
cause_cols   <- c("Tb_IPB", "dia_Oinf", "Oinfec", "maternal", "cvd", "cancer", "Acc_all", "Degen.dis", "Malaria", "Sen_Ounk")
cause_labels <- c("TB/IPB", "Diab/Other Inf", "Other Infec", "Maternal", "CVD", "Cancer", "All Accidents", "Degenerative dis", "Malaria", "Senility/Unknown")
 
# Apply readable labels and factor order
df$cause_label <- cause_labels[match(df$Cause, cause_cols)]
df$cause_label <- factor(df$cause_label, levels = cause_labels)
df$sex_label   <- ifelse(df$sex == 1, "Male", "Female")
 
df2$cause_label <- cause_labels[match(df2$Cause, cause_cols)]
df2$cause_label <- factor(df2$cause_label, levels = cause_labels)
df2$sex_label   <- ifelse(df2$sex == 1, "Male", "Female")

df3$cause_label <- cause_labels[match(df3$Cause, cause_cols)]
df3$cause_label <- factor(df3$cause_label, levels = cause_labels)
df3$sex_label   <- ifelse(df3$sex == 1, "Male", "Female")



# costa rica, Cuba, Dom Rep, first period, with



ui <- fluidPage(
  titlePanel("Decompositions by Cause of death, Country, Period, and Sex"),
  
  tabsetPanel(
    
    # --- First Tab: e(0) ---
    tabPanel("Decomposing: e(0)",
             fluidRow(
               column(width = 12,
                      wellPanel(
                        selectInput("country", "Select Country:",
                                    choices  = sort(unique(df$ctry.lab)),
                                    selected = sort(unique(df$ctry.lab)))
                      )
               )
             ),
             fluidRow(
               column(width = 12,
                      plotOutput("e0", height = "600px")
               )
             )
    ),
    
    # --- Second Tab: sd(10) ---
    tabPanel("Decomposing: sd(10)",
             fluidRow(
               column(width = 12,
                      wellPanel(
                        selectInput("country_sd", "Select Country:", # Note: unique ID suggested
                                    choices  = sort(unique(df2$ctry.lab)),
                                    selected = sort(unique(df2$ctry.lab)))
                      )
               )
             ),
             fluidRow(
               column(width = 12,
                      plotOutput("sd10", height = "600px")
               )
             )
    ),
    
    # --- Third Tab: e.dagger(0) ---
    tabPanel("Decomposing: e.dagger(0)",
             fluidRow(
               column(width = 12,
                      wellPanel(
                        selectInput("country_ed", "Select Country:", # Note: unique ID suggested
                                    choices  = sort(unique(df3$ctry.lab)),
                                    selected = sort(unique(df3$ctry.lab)))
                      )
               )
             ),
             fluidRow(
               column(width = 12,
                      plotOutput("edagger", height = "600px")
               )
             )
    )
  ) # end tabsetPanel
)

server <- function(input, output) {
  output$e0 <- renderPlot({
    sub <- df[df$ctry.lab == input$country, ]
    validate(need(nrow(sub) > 0, "No data for selected country."))

    ggplot(sub, aes(x = cause_label, y = Contr, group = case)) +
      geom_col(aes(alpha = case ), position = position_dodge(width = 0.8), width = 0.8) +
      # Manually define the alpha levels here
      # 1.0 is fully opaque, lower numbers are more transparent
      scale_alpha_manual(values = c("Excludes Unkn" = 0.3, "Includes Unkn" = 1.0)) +
      facet_grid(rows = vars(sex_label), cols = vars(period), switch="y") +
      scale_fill_brewer(palette = "Set2") +
      labs(title = paste0("Country: ", input$country, "; Decomposition of e(0) by Period"), x = "Cause of death", y = "Contribution (years of life)") +
      theme_bw(base_size = 13) +
      theme(
        strip.background   = element_rect(fill = "#2c3e50"),
        strip.text         = element_text(color = "white", face = "bold", size = 20),
        # Increase font size for axis titles (labels)
        axis.title = element_text(size = 20, face = "bold"),        
        axis.text.y        = element_text(angle = 0,  size = 20),
        axis.text.x        = element_text(angle = 40, hjust = 1, size = 20),
        plot.title         = element_text(face = "bold", size = 15),
        legend.position=c(0.12,0.9),
        legend.background=element_rect(fill="white"),legend.key.size = unit(1,"line"),
        legend.text=element_text(size=18),legend.title=element_text(size=18), 
        panel.grid.major.x = element_blank()
        
      )
    
  })
  
  
  output$sd10 <- renderPlot({
    sub <- df3[df3$ctry.lab == input$country, ]
    validate(need(nrow(sub) > 0, "No data for selected country."))
  
    ggplot(sub, aes(x = cause_label, y = Contr, group = case)) +
      geom_col(aes(alpha = case ), position = position_dodge(width = 0.8), width = 0.8) +
    # Manually define the alpha levels here
      # 1.0 is fully opaque, lower numbers are more transparent
      scale_alpha_manual(values = c("Excludes Unkn" = 0.3, "Includes Unkn" = 1.0)) +
      facet_grid(rows = vars(sex_label), cols = vars(period), switch="y") +
      scale_fill_brewer(palette = "Set2") +
      labs(title = paste0("Country: ", input$country, "; Decomposition of the Standard Deviation at age 10 by Period"), x = "Cause of death", y = "Contribution (years of life)") +
      theme_bw(base_size = 13) +
      theme(
        strip.background   = element_rect(fill = "#2c3e50"),
        strip.text         = element_text(color = "white", face = "bold", size = 20),
        # Increase font size for axis titles (labels)
        axis.title = element_text(size = 20, face = "bold"),        
        axis.text.y        = element_text(angle = 0,  size = 20),        
        axis.text.x        = element_text(angle = 40, hjust = 1, size = 20),
        plot.title         = element_text(face = "bold", size = 15),
        legend.position=c(0.12,0.9),
        legend.background=element_rect(fill="white"),legend.key.size = unit(1,"line"),
        legend.text=element_text(size=18),legend.title=element_text(size=18),        
        panel.grid.major.x = element_blank()
        
      )
    
  })  
  
  
  # write.csv(sub, file = "test.csv")    
  output$edagger <- renderPlot({
    sub <- df2[df2$ctry.lab == input$country, ]
    validate(need(nrow(sub) > 0, "No data for selected country."))
    
    ggplot(sub, aes(x = cause_label, y = Contr, group = case)) +
      geom_col(aes(alpha = case ), position = position_dodge(width = 0.8), width = 0.8) +
      # Manually define the alpha levels here
      # 1.0 is fully opaque, lower numbers are more transparent
      scale_alpha_manual(values = c("Excludes Unkn" = 0.3, "Includes Unkn" = 1.0)) +
      facet_grid(rows = vars(sex_label), cols = vars(period), switch="y") +
      scale_fill_brewer(palette = "Set2") +
      labs(title = paste0("Country: ", input$country, "; Decomposition of e.dagger at age 0 by Period"), x = "Cause of death", y = "Contribution (years of life)") +
      theme_bw(base_size = 13) +
      theme(
        strip.background   = element_rect(fill = "#2c3e50"),
        strip.text         = element_text(color = "white", face = "bold", size = 20),        
        # Increase font size for axis titles (labels)
        axis.title = element_text(size = 20, face = "bold"),        
        axis.text.y        = element_text(angle = 0,  size = 20),        
        axis.text.x        = element_text(angle = 40, hjust = 1, size = 20),
        plot.title         = element_text(face = "bold", size = 15),
        legend.position=c(0.12,0.9),
        legend.background=element_rect(fill="white"),legend.key.size = unit(1,"line"),
        legend.text=element_text(size=18),legend.title=element_text(size=18),        
        panel.grid.major.x = element_blank()
      )
  })  
}

shinyApp(ui, server)