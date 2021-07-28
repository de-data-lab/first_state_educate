fse_blue <- function() "#27829e"
fse_red <- function() "#d23742"

fse_pal <- function() {
  c("fse_blue" = fse_blue(), 
       "fse_red" = fse_red())
  }

font_add("Futura PT",
         regular = "Futura PT Book.otf",
         bold = "Futura PT Bold.otf",
         italic = "Futura PT Book Oblique.otf",
         bolditalic = "Futura PT Bold Oblique.otf")

font_add("Poppins",
         regular = "Poppins Regular.otf",
         bold = "Poppins Bold.otf",
         italic = "Poppins Italic.otf",
         bolditalic = "Poppins Bold Italic.otf")


get_acs_with_year <- function(geo_var = "tract",
                              year_var = 2018,
                              var_nums = c("B19013_001"),
                              state_abbr = "DE",
                              survey_var = "acs5") {
  
  get_acs(geography = geo_var,
          variables = var_nums,
          state = state_abbr,
          geometry = FALSE,
          survey = survey_var,
          year = year_var,
          show_call = TRUE) %>% 
    mutate(year = year_var)
  
}


percent_or_not <- function(x) {
  ifelse(x <= 1,
         scales::percent(x), x)
}


create_student_proficiency_plot <- function(data, content, grade, districts, title_copy) {
  
  student_assessment_selected <- 
    data %>% 
    filter(District %in% districts,
           `School Code` == 0,
           Gender == "All Students",
           Race == "All Students",
           SpecialDemo == "All Students",
           ContentArea == content,
           Grade == grade,
           `Assessment Name` == "Smarter Balanced Summative Assessment") %>% 
    mutate(PctProficient = PctProficient / 100,
           highlight = District == "State of Delaware")
  
  student_assessment_selected_last <-
    student_assessment_selected %>% 
    filter(`School Year` == max(`School Year`)) %>% 
    mutate(highlight = District == "State of Delaware",
           face = if_else(District == "State of Delaware", "bold", "plain"))
  
  ggplot() +
    geom_rect(aes(xmin = 2019.1,
                  xmax = Inf,
                  ymin = -Inf,
                  ymax = Inf),
              color = NA,
              fill = "white") +
    geom_line(data = student_assessment_selected,
              aes(x = `School Year`,
                  y = PctProficient,
                  color = highlight,
                  alpha = highlight,
                  size = highlight,
                  group = District)) +
    geom_text_repel(data = student_assessment_selected_last,
                    aes(x = `School Year`,
                        y = PctProficient,
                        label = District,
                        color = highlight,
                        fontface = face),
                    # hjust = 0,
                    direction = "y",
                    nudge_x = .1,
                    hjust = 0,
                    seed = 1231,
                    family = "Poppins",
                    size = 3.5) +
    
    scale_color_manual(values = c(fse_blue(), fse_red())) +
    scale_size_manual(values = c(.5, 1)) +
    scale_alpha_manual(values = c(0.5, 1)) +
    scale_x_continuous(breaks = seq(2015, 2019, 1),
                       limits = c(2015, 2022)) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0, 1)) +
    labs(title = title_copy) +
    theme(plot.title = element_text(family = "Futura PT",
                                    face = "bold"),
          plot.title.position = "plot",
          axis.title = element_blank(),
          axis.text = element_text(family = "Poppins",
                                   size = 8),
          legend.position = "none",
          panel.grid.minor = element_blank())
  
}


americas_health_rankings_plot <- function(data, measure, title_text, subtitle_text) {
  
  data %>% 
    filter(`State Name` == "Delaware",
           `Measure Name` == measure,
           ! is.na(Rank)) %>% 
    ggplot(aes(x = Edition,
               y = Value)) +
    geom_line(size = 2,
              color = fse_red(),
              linejoin = "round",
              lineend = "round") +
    labs(title = title_text,
         subtitle = subtitle_text,
         caption = "Source: America's Health Rankings") +
    theme(plot.title = element_text(family = "Futura PT",
                                    face = "bold"),
          plot.title.position = "plot",
          axis.title = element_blank(),
          axis.text = element_text(family = "Poppins",
                                   size = 8),
          legend.position = "none",
          panel.grid.minor = element_blank())
  
}
