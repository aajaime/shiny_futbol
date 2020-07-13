#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sjmisc)
library(dplyr)
library(plotly)
library(ggplot2)

# Lectura de base
df_torneos <- readxl::read_xlsx('base_torneos.xlsx') %>% 
    janitor::clean_names()

# Limpieza torneo 1997
df_97 <- df_torneos %>% 
    filter(year %in% c('1997a','1997c')) %>%
    group_by(equipo) %>% 
    mutate(pts = sum(pts),
           pj = sum(pj),
           pg = sum(pg),
           pe = sum(pe),
           pp = sum(pp),
           gf = sum(gf),
           gc = sum(gc),
           dif = gf-gc,
           year = 1997) %>% 
    arrange(desc(pts)) %>% 
    add_id() %>% 
    filter(ID == 1) %>% 
    remove_var(ID) %>% 
    ungroup() %>% 
    add_id() %>% 
    mutate(pos = ifelse((year != 1997),pos,ID)) %>% 
    remove_var(ID)

# Limpieza torneo 1999
df_99a <- df_torneos %>% 
    mutate(ind_pos = ifelse((year %in% c('1999-1','1999-2')& (pos >= 9)),1,0)) %>% 
    filter(year %in% c('1999-1','1999-2') & ind_pos == 0) %>% 
    group_by(equipo) %>% 
    mutate(pts = sum(pts),
           pj = sum(pj),
           pg = sum(pg),
           pe = sum(pe),
           pp = sum(pp),
           gf = sum(gf),
           gc = sum(gc),
           dif = gf-gc) %>% 
    arrange(desc(pts)) %>% 
    add_id() %>% 
    filter(ID == 1) %>% 
    remove_var(ID) %>% 
    ungroup()

df_99b <- df_torneos %>% 
    mutate(ind_pos = ifelse((year %in% c('1999-1','1999-2')& (pos >= 9)),1,0)) %>% 
    filter(year == '1999-2' & ind_pos == 1)

df_99 <- rbind(df_99a,
               df_99b) %>% 
    mutate(year = 1999) %>% 
    select(-ind_pos) %>% 
    arrange(desc(pts)) %>% 
    add_id() %>% 
    mutate(pos = ID) %>% 
    select(-ID)
    
# Selección de todos los torneos, menos 97 y 99
df_torneos2 <- df_torneos %>% 
    filter(year != '1997a',
           year != '1997c',
           year != '1999-1',
           year != '1999-2')

df_torneos2 <- rbind(df_torneos2,
                     df_97,
                     df_99) %>% 
    arrange(year,pos)

# Cálculo de porcentaje de rendimiento y puntos con 3 pts
df_torneos2 <- df_torneos2 %>% 
    mutate(pts_nuevo = ifelse((year >= 1995),pts,((pg*3)+pe)),
           porc = ifelse((year > 1994), 100*(pts/(pj*3)),100*(pts/(pj*2))),
           year = as.factor(year))

# Inicio shiny
ui <- navbarPage("Fútbol chileno. 1990-2002.",
                 theme = shinythemes::shinytheme('cyborg'),

    # Pestaña 1
    tabPanel("Tablas de posiciones",

    # Barra lateral
        sidebarPanel(
            selectInput("year",
                        "Escoja un año",
                        choices = c(1990:2002),
                        selected = "1990"),
            width = 2
        ),

        # Tabla del año escogido
        mainPanel(
            h3("Tabla del año seleccionado"),
            tableOutput("tabla_year"),
            h2("Tabla de posiciones del periodo"),
            h6("Ordenada según porcentaje de rendimiento calculado sobre la base de 3 puntos por ganador"),
            tableOutput("tabla_decada")
        )),
    tabPanel("Gráficos de rendimiento",
    # Barra lateral
    sidebarPanel(
        selectInput("equipo",
                           "Seleccione el equipo que desea ver:",
                           choices = c("Audax Italiano", "Cobreloa", "Cobresal", "Colo-Colo", "Coquimbo Unido", "Deportes Antofagasta", "Deportes Concepción", "Deportes Iquique", "Deportes La Serena", "Deportes Melipilla", "Deportes Puerto Montt", "Deportes Temuco", "Everton", "Fernández Vial", "Huachipato", "Naval", "O'Higgins", "Palestino", "Provincial Osorno", "Rangers", "Regional Atacama", "Santiago Morning", "Santiago Wanderers", "Universidad Católica", "Universidad de Chile", "Unión Española","Unión San Felipe"),
                           selected = "Audax Italiano"),
        width = 2
    ),
    
    # Gráfico escogido
    mainPanel(
        h2("Gráfico de rendimientos y posiciones de la década"),
        plotOutput("graf_eq")
    )))

# Cálculos shiny
server <- function(input, output) {

    output$tabla_decada <- renderTable({
        df_torneos2 %>% 
            group_by(equipo) %>% 
            mutate(pts_total = sum(pts_nuevo),
                   pj_total = sum(pj),
                   pg_total = sum(pg),
                   pe_total = sum(pe),
                   pp_total = sum(pp),
                   gf_total = sum(gf),
                   gc_total = sum(gc),
                   dif_total = gf_total-gc_total,
                   porc = 100*(pts_total/(pj_total*3))) %>% 
            add_id() %>% 
            filter(ID == 1) %>% 
            ungroup() %>% 
            arrange(desc(porc)) %>% 
            select(-ID,-pos) %>% 
            add_id() %>% 
            rename("pos" = ID) %>% 
            select(pos,equipo,ends_with('total'),porc)
        

    },
    digits = 0,
    caption = "Esta tabla considera bonificaciones según rendimiento en copas nacionales y descuentos por sanciones.")
    
    output$tabla_year <- renderTable({
        df_torneos2 %>% 
            filter(year == input$year) %>% 
            select(-pts_nuevo,-year)
    },
    digits = 0,
    caption = "Desde 1995, se computan 3 pts. al ganador.<br> Para 1997 y 1999, se consideran las tablas anuales.<br> En 1998, Deportes Temuco recibió una sanción de 15 pts.")
        
   output$graf_eq <- renderPlot({
       df_torneos2 %>% 
           filter(equipo == input$equipo) %>% 
           ggplot(aes(x=year,y=porc))+
           geom_point(aes(color = equipo))+
           geom_text(aes(label = pos),size=3,nudge_y = 1)+
           scale_color_manual(values = c("green", "orange", "dark orange", "black", "yellow", "blue", "purple", "light blue", "red", "blue", "green", "green", "blue", "yellow", "dark blue", "dark blue", "light blue", "black", "black", "red", "red", "black", "green", "light blue", "blue", "red","red"), breaks = c("Audax Italiano", "Cobreloa", "Cobresal", "Colo-Colo", "Coquimbo Unido", "Deportes Antofagasta", "Deportes Concepción", "Deportes Iquique", "Deportes La Serena", "Deportes Melipilla", "Deportes Puerto Montt", "Deportes Temuco", "Everton", "Fernández Vial", "Huachipato", "Naval", "O'Higgins", "Palestino", "Provincial Osorno", "Rangers", "Regional Atacama", "Santiago Morning", "Santiago Wanderers", "Universidad Católica", "Universidad de Chile", "Unión Española","Unión San Felipe"))+
           theme_dark()+
           theme(legend.position = 'none')+
           labs(x="Año",
                y="% de rendimiento")
   })     
}

# Run the application 
shinyApp(ui = ui, server = server)
