# Useful Functions

create.bar.chart.by.race <- function(data=equity.data.by.race, geo, con, w.x="ACS_Race", w.y="share", w.fill="ACS_Race", lbl=scales::percent, c.facet=2, c.scales="fixed", w.var="NAME", cat, race=race.list.charts) {
  
  if (w.y == "share") {
    fact <- 100
    dec <- 1
    suff <- "%"
  } else {
    fact <- 1
    dec <- 0
    suff <- ""
  }
  
  tbl <- data %>% filter((ACS_Geography == geo & ACS_Concept == con & ACS_Race %in% race & ACS_Category %in% cat))
  
  # Create Plot
  g <-  ggplotly(ggplot(data = tbl,
                        
                        aes(x = get(eval(w.x)), 
                            y = get(eval(w.y)), 
                            fill = get(eval(w.fill)),
                            text = paste0("<b>", get(eval(w.x)), ": </b>", prettyNum(round(get(eval(w.y))*fact, dec), big.mark = ","), suff,"<br>"))) +
                   geom_col(
                     color = "black",
                     alpha = 1.0,
                     position = "dodge") +
                   labs(x = NULL, y = NULL) +
                   scale_y_continuous(labels = lbl) +
                   scale_fill_manual(values= psrc.colors) +
                   theme(plot.title = element_text(size = 10, face = 'bold'),
                         axis.text.x = element_blank(),
                         axis.ticks.x = element_blank(),
                         axis.line = element_blank(),
                         panel.background = element_blank(),
                         panel.grid.major.y = element_line(colour="#BBBDC0",size = 0.25),
                         panel.grid.minor.y = element_line(colour="#BBBDC0",size = 0.25),
                         panel.grid.major.x = element_blank(),
                         panel.grid.minor.x = element_blank(),
                         text = element_text(family = "Segoe UI"),
                         legend.position = "bottom",
                         legend.title = element_blank())+
                   facet_wrap(vars(get(eval(w.var))), scales = c.scales, ncol=c.facet) +
                   theme(panel.spacing.y = unit(4, "lines")),
                 tooltip = c("text")) %>% layout(legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.25))
  
  return(g)
  
}

create.msa.bar.chart <- function(data=equity.data.by.race, con, race, w.y="share", lbl=scales::percent, cat) {
  
  if (w.y == "share") {
    fact <- 100
    dec <- 1
    suff <- "%"
  } else {
    fact <- 1
    dec <- 0
    suff <- ""
  }
  
  tbl <- data %>% filter((ACS_Geography == "MSA" & ACS_Race == race & ACS_Concept == con & ACS_Category %in% cat)) %>%
    filter(!grepl('PR Metro Area', NAME)) %>%
    mutate(plot_id=0) %>%
    mutate(plot_id = case_when(
      NAME %in% sister.msa ~ 3,
      NAME == "Seattle-Tacoma-Bellevue, WA Metro Area" ~ 1,
      NAME == "Bremerton-Silverdale-Port Orchard, WA Metro Area" ~ 2)) %>%
    mutate(plot_id = replace_na(plot_id,0))
  
  g <-  ggplotly(ggplot(data= tbl,
                        aes(x = reorder(NAME,-get(eval(w.y))), 
                            y = get(eval(w.y)), 
                            fill = as.factor(plot_id),
                            text = paste0("<b>", NAME, ": </b>", prettyNum(round(get(eval(w.y))*fact, dec), big.mark = ","), suff, "<br>"))) +
                   geom_col(
                     color = "black",
                     alpha = 1.0,
                     position = "dodge") +
                   labs(x = NULL, y = NULL) +
                   scale_y_continuous(labels = lbl) +
                   scale_fill_manual(values= psrc.colors) +
                   theme(plot.title = element_text(size = 10, face = 'bold'),
                         axis.text.x = element_blank(),
                         axis.ticks.x = element_blank(),
                         axis.line = element_blank(),
                         panel.background = element_blank(),
                         panel.grid.major.y = element_line(colour="#BBBDC0",size = 0.25),
                         panel.grid.minor.y = element_line(colour="#BBBDC0",size = 0.25),
                         panel.grid.major.x = element_blank(),
                         panel.grid.minor.x = element_blank(),
                         text = element_text(family = "Segoe UI"),
                         legend.position = "none",
                         legend.title = element_blank()),
                 tooltip = c("text"))  
  return(g)
}

create.city.bar.charts <- function(data=equity.data.by.race, race, con, reg, w.x="NAME", w.y="share", lbl=scales::percent, c.scales="free", c.facet=2) {
  
  if (w.y == "share") {
    fact <- 100
    dec <- 1
    suff <- "%"
  } else {
    fact <- 1
    dec <- 0
    suff <- ""
  }
  
  tbl <- data %>% filter((ACS_Geography == "Place" & ACS_Race == race & ACS_Concept == con))
  
  regeo <- city.lyr %>% st_drop_geometry() %>% select(city_name,class_desc)
  
  tbl <- left_join(tbl, regeo, by=c("NAME"= "city_name"))
  tbl$class_desc <- factor(tbl$class_desc, levels=city.class.order)
  
  # Create Plot
  g <-  ggplotly(ggplot(data = tbl,
                        aes(x = reorder(get(eval(w.x)),-get(eval(w.y))), 
                            y = get(eval(w.y)), 
                            fill = get(eval(w.x)),
                            text = paste0("<b>", get(eval(w.x)), ": </b>", prettyNum(round(get(eval(w.y))*fact, dec), big.mark = ","), suff, "<br>"))) +
                   geom_col(
                     color = "black",
                     alpha = 1.0,
                     position = "dodge") +
                   geom_hline(yintercept=reg,
                              size = 0.5,
                              color = "black") +
                   labs(x = NULL, y = NULL) +
                   scale_y_continuous(labels = lbl) +
                   #scale_fill_manual(values= psrc.colors) +
                   theme(plot.title = element_text(size = 10, face = 'bold'),
                         axis.text.x = element_blank(),
                         axis.ticks.x = element_blank(),
                         axis.line = element_blank(),
                         panel.background = element_blank(),
                         panel.grid.major.y = element_line(colour="#BBBDC0",size = 0.25),
                         panel.grid.minor.y = element_line(colour="#BBBDC0",size = 0.25),
                         panel.grid.major.x = element_blank(),
                         panel.grid.minor.x = element_blank(),
                         text = element_text(family = "Segoe UI"),
                         legend.position = "none",
                         legend.title = element_blank())+
                   facet_wrap(vars(class_desc), scales = c.scales, ncol=c.facet) +
                   theme(panel.spacing.y = unit(4, "lines")),
                 tooltip = c("text"))
  
  return(g)
  
}

create.grouped.charts <- function(data=equity.data.by.race, con, geo, cat, w.x="ACS_Category", w.y="share", lbl=scales::percent, ord, race=race.list.charts) {
  
  if (w.y == "share") {
    fact <- 100
    dec <- 1
    suff <- "%"
  } else {
    fact <- 1
    dec <- 0
    suff <- ""
  }
  
  tbl <- data %>% filter((ACS_Geography == geo & ACS_Concept == con & ACS_Category %in% cat & ACS_Race %in% race))
  tbl$ACS_Category <- factor(tbl$ACS_Category, levels=ord)
  
  g <- ggplotly(ggplot(data = tbl, 
                       aes(fill=ACS_Race, 
                           y=get(eval(w.y)), 
                           x=get(eval(w.x)),
                           text=paste0("<b>", ACS_Race, ": </b>", prettyNum(round(get(eval(w.y))*fact, dec), big.mark = ","), suff,"<br>"))) + 
                  geom_bar(position="dodge", stat="identity") +
                  labs(x = NULL, y = NULL) +
                  scale_y_continuous(labels = lbl) +
                  scale_fill_manual(values= psrc.colors) +
                  theme(plot.title = element_text(size = 10, face = 'bold'),
                        #axis.text.x = element_blank(),
                        axis.ticks.x = element_blank(),
                        axis.line = element_blank(),
                        panel.background = element_blank(),
                        panel.grid.major.y = element_line(colour="#BBBDC0",size = 0.25),
                        panel.grid.minor.y = element_line(colour="#BBBDC0",size = 0.25),
                        panel.grid.major.x = element_blank(),
                        panel.grid.minor.x = element_blank(),
                        legend.position = "none",
                        legend.title = element_blank()),
                tooltip = c("text")) %>% layout(legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.25))
  
  return(g)
}

create.tract.map <- function(data = equity.data.by.race, c.layer = tract.lyr, con, val, race) {

  tbl <- data %>%
    filter((ACS_Geography == "Tract" & ACS_Race == race & ACS_Concept == con)) %>%
    select(GEOID,.data[[val]]) %>%
    mutate(across(everything(), .fns = ~replace_na(.,0))) %>%
    mutate(across(c('GEOID'), as.character))
  
  c.layer <- left_join(c.layer,tbl, by = c("geoid10"="GEOID")) %>%
    select(geoid10, .data[[val]]) %>%
    mutate(people_of_color=0) %>%
    mutate(people_of_color = case_when(
      share >= region.non.white ~ "Greater than Region")) %>%
    mutate(people_of_color = replace_na(people_of_color,"Less than Region")) %>%
    st_transform(wgs84)
  
  m.pal <- colorFactor(
    palette = c("white", "#C388C2"),
    levels = c("Less than Region", "Greater than Region"))
  
  m <- leaflet() %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    
    addEasyButton(easyButton(
      icon="fa-globe", title="Region",
      onClick=JS("function(btn, map){map.setView([47.615,-122.257],8.5); }"))) %>%
    
    addPolygons(data=c.layer,
                fillOpacity = 0.5,
                fillColor = ~m.pal(people_of_color),
                opacity = 0.5,
                weight = 0.5,
                color = "#BCBEC0",
                dashArray = "") %>%
    
    addLegend(pal = m.pal,
              values = c.layer$people_of_color,
              group = "People of Color",
              position = "bottomright",
              title = "People of Color") %>%
    
    setView(lng=-122.257, lat=47.615, zoom=8.5)
  
  return(m)
  
}

create.tract.map.by.category <- function(data=equity.data.by.race, con, cat, val="share", race, c.layer=tract.lyr) {
  
  tbl <- data %>%
    filter((ACS_Geography == "Tract" & ACS_Race == race & ACS_Concept == con & ACS_Category == cat & estimate >tract.population.threshold)) %>%
    select(GEOID,.data[[val]]) %>%
    mutate(across(everything(), .fns = ~replace_na(.,0))) %>%
    mutate(across(c('GEOID'), as.character)) %>%
    rename(value=val)
  
  c.layer <- left_join(c.layer,tbl, by = c("geoid10"="GEOID")) %>%
    select(geoid10, value) %>%
    #mutate(value = replace_na(value,0)) %>%
    st_transform(wgs84)
  
  breaks <- (seq(from=0,to=1,by=0.125))
  m.pal <- colorBin("Blues", domain = c.layer$value, bins = breaks)
  
  labels <- paste0(prettyNum(round(c.layer$value*100, 1), big.mark = ","),"%") %>% lapply(htmltools::HTML)
  
  m <- leaflet() %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    
    addEasyButton(easyButton(
      icon="fa-globe", title="Region",
      onClick=JS("function(btn, map){map.setView([47.615,-122.257],8.5); }"))) %>%
    
    addPolygons(data=people.of.color.lyr,
                fillOpacity = 0.0,
                opacity = 1,
                weight = 2,
                color = "#F05A28",
                dashArray = "4") %>%
    
    addPolygons(data=c.layer,
                fillOpacity = 0.5,
                fillColor = ~m.pal(value),
                opacity = 0.5,
                weight = 0.5,
                color = "#BCBEC0",
                dashArray = "",
                highlight = highlightOptions(
                  weight =5,
                  color = "76787A",
                  dashArray ="",
                  fillOpacity = 0.7,
                  bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")) %>%

    addLegend(pal = m.pal,
              values = c.layer$value,
              position = "bottomright",
              labFormat = labelFormat(suffix="%", between = "% - ", transform = function(x) 100 * x)) %>%
    
    setView(lng=-122.257, lat=47.615, zoom=8.5)
  
  return(m)
  
}
