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
    
# Selección de todos los torneos, menos 99
df_torneos2 <- df_torneos %>% 
    filter(year != '1999-1',
           year != '1999-2')

df_torneos2 <- rbind(df_torneos2,
                     df_99) %>% 
    arrange(year,pos)

# Cálculo para torneos cortos
df_torneos2 <- df_torneos2 %>% 
    group_by(equipo,
             year) %>% 
    mutate(pts = sum(pts),
           pj = sum(pj),
           pg = sum(pg),
           pe = sum(pe),
           pp = sum(pp),
           gf = sum(gf),
           gc = sum(gc),
           dif = (gf-gc)) %>% 
    arrange(desc(pts),
            desc(dif),
            year) %>% 
    add_id() %>% 
    filter(ID == 1) %>% 
    remove_var(ID) %>% 
    ungroup() %>%
    select(-pos) %>% 
    group_by(year) %>% 
    add_id() %>% 
    rename('pos' = ID) %>% 
    ungroup()

# Cálculo de porcentaje de rendimiento y puntos con 3 pts
df_torneos2 <- df_torneos2 %>% 
    mutate(pts_nuevo = ifelse((year >= 1995),pts,((pg*3)+pe)),
           porc = ifelse((year > 1994), 100*(pts/(pj*3)),100*(pts/(pj*2))),
           year = factor(year,levels = c(1966:2013,'2013-14','2014-15','2015-16','2016-17','2017','2018')),
           camp = case_when((year %in% c(1966:1967,1971:1983,1985:2001,2010,2013,2017,2018))& (pos == 1)~'C',
                            (year == 1968) & (equipo == 'Santiago Wanderers')~'C',
                            (year == 1969) & (equipo == 'Universidad de Chile')~'C',
                            (year == 1970) & (equipo == 'Colo-Colo')~'C',
                            (year == 1984) & (equipo == 'Universidad Católica')~'C',
                            (year == 2002) & (equipo == 'Colo-Colo')~'C',
                            (year == 2002) & (equipo == 'Universidad Católica')~'C',
                            (year == 2003) & (equipo == 'Cobreloa')~'C,C',
                            (year == 2004) & (equipo == 'Universidad de Chile')~'C',
                            (year == 2004) & (equipo == 'Cobreloa')~'C',
                            (year == 2005) & (equipo == 'Unión Española')~'C',
                            (year == 2005) & (equipo == 'Universidad Católica')~'C',
                            (year == 2006) & (equipo == 'Colo-Colo')~'C,C',
                            (year == 2007) & (equipo == 'Colo-Colo')~'C,C',
                            (year == 2008) & (equipo == 'Colo-Colo')~'C',
                            (year == 2008) & (equipo == 'Everton')~'C',
                            (year == 2009) & (equipo == 'Universidad de Chile')~'C',
                            (year == 2009) & (equipo == 'Colo-Colo')~'C',
                            (year == 2011) & (equipo == 'Universidad de Chile')~'C,C',
                            (year == 2012) & (equipo == 'Universidad de Chile')~'C',
                            (year == 2012) & (equipo == 'Huachipato')~'C',
                            (year == '2013-14') & (equipo == "O'Higgins")~'C',
                            (year == '2013-14') & (equipo == 'Colo-Colo')~'C',
                            (year == '2014-15') & (equipo == "Universidad de Chile")~'C',
                            (year == '2014-15') & (equipo == 'Cobresal')~'C',
                            (year == '2015-16') & (equipo == "Colo-Colo")~'C',
                            (year == '2015-16') & (equipo == 'Universidad Católica')~'C',
                            (year == '2016-17') & (equipo == "Universidad de Chile")~'C',
                            (year == '2016-17') & (equipo == 'Universidad Católica')~'C',
                            TRUE ~ ''))

# Inicio shiny
ui <- navbarPage("Fútbol chileno. 1966-2018.",
                 theme = shinythemes::shinytheme('cyborg'),

    # Pestaña 1
    tabPanel("Tablas de posiciones",

    # Barra lateral
        sidebarPanel(
            selectInput("year",
                        "Escoja un año",
                        choices = c(1966:2013,'2013-14','2014-15','2015-16','2016-17','2017','2018'),
                        selected = "1966"),
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
                           choices = c("Audax Italiano", "Barnechea", "Cobreloa", "Cobresal", "Colo-Colo", "Coquimbo Unido", 
                                       "Curicó Unido","Deportes Antofagasta", "Deportes Arica","Deportes Concepción", "Deportes Aviación","Deportes Iquique", "Deportes La Serena", 
                                       "Deportes Melipilla", "Deportes Ovalle", "Deportes Puerto Montt", "Deportes Temuco", "Deportes Valdivia","Everton", 
                                       "Fernández Vial", "Ferrobádminton","Huachipato", "Lota Schwager","Magallanes",
                                       "Naval", "Ñublense","O'Higgins", "Palestino", "Provincial Osorno", "Rangers",
                                       "Regional Atacama", "San Luis","Santiago Morning", "Santiago Wanderers","Trasandino", 
                                       "Universidad Católica", "Universidad de Chile","Universidad de Concepción", "Unión Española",
                                       "Unión La Calera","Unión San Felipe"),
                           selected = "Audax Italiano"),
        width = 2
    ),
    
    # Gráfico escogido
    mainPanel(
        h2("Gráfico de rendimientos y posiciones del periodo"),
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
    caption = "Desde 1995, se computan 3 pts. al ganador.<br> Para 1997 y desde 2002 hasta 2013, se consideran las tablas anuales.<br> Desde la temporada 2013-14 hasta la 2016-17, se consideran tablas de la temporada. <br> Tablas consideran eventuales bonificaciones y sanciones.")
        
   output$graf_eq <- renderPlot({
       df_torneos2 %>% 
           filter(equipo == input$equipo) %>% 
           ggplot(aes(x=year,y=porc))+
           geom_point(aes(color = equipo))+
           geom_text(aes(label = pos),size=3,nudge_y = 1)+
           scale_color_manual(values = c("green", "orange", "dark orange", "black", "yellow", "blue", 
                                         "purple", "light blue", "red", "blue", "green", "green", "blue", "yellow", 
                                         "dark blue", "dark blue", "light blue", "black", "black", "red", "red", "black", 
                                         "green", "light blue", "blue", "red","red","yellow","black","red","yellow","red",
                                         "black","light blue","white","red","light blue","green","light blue","green","yellow"), 
                              breaks = c("Audax Italiano", "Cobreloa", "Cobresal", "Colo-Colo", "Coquimbo Unido", 
                                         "Deportes Antofagasta", "Deportes Concepción", "Deportes Iquique", 
                                         "Deportes La Serena", "Deportes Melipilla", "Deportes Puerto Montt", 
                                         "Deportes Temuco", "Everton", "Fernández Vial", "Huachipato", "Naval", 
                                         "O'Higgins", "Palestino", "Provincial Osorno", "Rangers", "Regional Atacama", 
                                         "Santiago Morning", "Santiago Wanderers", "Universidad Católica", 
                                         "Universidad de Chile", "Unión Española","Unión San Felipe",
                                         "Universidad de Concepción","Lota Schwager","Ñublense","San Luis",
                                         "Unión La Calera","Curicó Unido","Deportes Arica","Barnechea",
                                         "Deportes Valdivia","Magallanes","Trasandino","Deportes Aviación","Deportes Ovalle",
                                         "Ferrobádminton"))+
           theme_dark()+
           theme(legend.position = 'none')+
           labs(x="Año",
                y="% de rendimiento")
   })     
}

# Run the application 
shinyApp(ui = ui, server = server)
