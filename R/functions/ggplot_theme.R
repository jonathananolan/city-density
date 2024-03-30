jn_colours <- list(
  complementary =  c("#0bb4ff",
                     "#e60049",
                     "#50e991",
                     "#FFE019",
                     #"#e6d800",
                     "#9b19f5",
                     "#ffa300",
                     "#076C99"), 
  
  divergent = c(
    "#48b6fa",
    "#91c9f7",
    "#c4ddf4",
    "#f1f1f1",
    "#f3babc",
    "#ec838a",
    "#de425b"),
  
  descending = c("#0080ff",
                 "#0094ff",
                 "#00a6ff",
                 "#00b6ff",
                 "#00c6ff",
                 "#4ed4ff",
                 "#78e2fe",
                 "#9ceefd",
                 "#bffaff"),
  
  #  c(
  #"#0010d9",
  #"#632cdb",
  #"#8c48df",
  #"#ac64e3",
  #"#c682e8",
  #"#dda0ee",
  #"#efc0f6",
  #"#ffe0ff"),
  
  text = c("#000000",
           "#2e2e2e",
           "#666666",
           "#ffffff"),
  
  descending_two <- c(
    "#0000b3",
    "#0010d9" ,
    "#0020ff",
    "#0040ff",
    "#0060ff",
    "#0080ff",
    "#009fff",
    "#00bfff",
    "#00ffff"),
  
  
  
  didnt_use_one = 
    c("#006fb0",
      "#0083c2",
      "#0097d2",
      "#00ace0",
      "#00c1eb",
      "#00d5f4",
      "#00eafb",
      "#00feff"),
  
  
  
  didnt_use_two = 
    c("#15C3EB",
      "#634EED",
      "#de425b",
      "#ffa600",
      "#FFE019",
      "#34EB56"),
  
  didnt_use_3 = c("#973AFC",
                  "#FA6971",
                  "#48B6FA",
                  "#FAF92F",
                  "#6BFA8F",
                  "#ffa600")
)


theme_jn_caption <- function(caption_text = " ",
                             plot_type = "bar",
                             colour_scale = "complimentary"){
  library(ggtext)
  
  
  update_geom_defaults("bar",  list(fill = jn_colours$descending[3], colour = "white"))
  update_geom_defaults("col",  list(fill = jn_colours$descending[3], colour = "white"))
  update_geom_defaults("point",list(fill = jn_colours$descending[3], colour = "white"))
  update_geom_defaults("line", list(fill = jn_colours$descending[3],  colour = "white"))
  update_geom_defaults("sf",   list(fill = jn_colours$descending[3],  colour = "white"))
  
  
  #Pull in fill types
  fill_colours <- function(){
    if(substr(tolower(colour_scale),1,1) == "c")  {   op <-   scale_fill_manual(values = jn_colours$complementary)
    } else if(substr(tolower(colour_scale),1,4) == "div") {   op <-   scale_fill_manual(values = jn_colours$divergent)
    } else if(substr(tolower(colour_scale),1,1) == "des") {   op <-   scale_fill_manual(values = jn_colours$descending)
    } else {                                                  op <- list()}
    return(op) 
  }
  
  #Pull in colour types
  
  colour_colours <- function(){
    if(substr(tolower(colour_scale),1,1) == "c")  {   op <-   scale_colour_manual(values = jn_colours$complementary)
    } else if(substr(tolower(colour_scale),1,4) == "div") {   op <-   scale_colour_manual(values = jn_colours$divergent)
    } else if(substr(tolower(colour_scale),1,1) == "des") {   op <-   scale_colour_manual(values = jn_colours$descending)
    } else {                                                  op <- list()}
    return(op) 
  }
  
  x <- glue::glue("<span style='color:{jn_colours$text[3]}'>{caption_text}</span><br><span style='color:{jn_colours$text[5]}'>JonathanNolan.substack.com</span>")
  
  output <- list(theme(plot.background = element_rect(fill = "white", 
                                                      colour = "white"),
                       plot.title  = element_text(size=16, 
                                                  color=jn_colours$text[1], 
                                                  face="bold",
                                                  angle=0),
                       plot.subtitle  = element_text(color=jn_colours$text[3]),
                       axis.title  = element_text(color=jn_colours$text[3]),
                       legend.title =    element_text(color=jn_colours$text[3])
  )
  )
  #,
  #labs(caption = x)

  
  non_map_elements <- list(theme_minimal()+
                             theme(panel.grid.major = element_blank(), 
                                   panel.grid.minor = element_blank()))
  
  if(plot_type %in%  c("bar","column")) { 
    
    output <- list(non_map_elements,
                   fill_colours(),
                   output)
  }
  
  if(plot_type %in%  c("point","line")){ 
    
    output <- list(non_map_elements,
                   colour_colours(),
                   output)
  }
  
  if(plot_type == "map"){ 
    output <- list(ggthemes::theme_map(),
                   fill_colours(),
                   output)}
  
  return(output)
}

