# title: clean
# date: august 2023
# by: jan oledan
# desc: load, clean data of LGBT index (Velasco), combine with income data
#       output plots

##### install and load packages #####
rm(list=ls())
# Package names
packages <- c("sf","readr","tidyr","dplyr","ggplot2",
              "stringr","magrittr", "ggrepel",
              "sysfonts","extrafont", "readxl", "cowplot",
              "patchwork", "Cairo", "lubridate", "ragg",
              "camcorder", "devtools")

# install packages not yet installed
install_package <- packages %in% rownames(installed.packages())
if (any(install_package == FALSE)) {
  install.packages(packages[!install_package])
}
invisible(lapply(packages, library, character.only = TRUE))

##### set up directories ######
### define main directory here ###
main <- "lgbtq-policy"
dataraw <- paste(main, "dataraw", sep = "/")
plots <- paste(main, "plots", sep = "/")
code <- paste(main, "code", sep = "/")

##### load raw data #####
# rename
df <- read_csv(paste(dataraw,
                     "lgbt-rights-index.csv",
                     sep = "/")) %>%
  rename(iso3c = Code,
         year = Year,
         lgbt_index = "LGBT+ Policy Index")

##### visualize data #####
# robinson projection not recognized, have to define
robinson = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
world_dir <- paste(main, 
                   "world",
                   sep="/")

# data for 2019 
df_2019 <- df %>%
  filter(!is.na(iso3c), 
         year == 2019) %>%
  rename(year_2019 = year) %>%
  mutate(norm2019 = (lgbt_index + 5)/(18), # value - min/(max-min)
         norm2019_cat = case_when( # define quantiles manually for plot
           is.na(norm2019) ~ "No data",
           norm2019 >= 0.8 ~ "1",
           norm2019 >= 0.6 ~ "0.8",
           norm2019 >= 0.4 ~ "0.6",
           norm2019 >= 0.2 ~ "0.4",
           norm2019 >= 0 ~ "0.2"),
         iso3c = if_else(Entity=="Kosovo", "KOS", iso3c)) # adjust iso3c 


# get those below the world average in 2019
# store world average as value
world_avg <- df_2019 %>%
  filter(Entity=="World") %>%
  select(norm2019) %>%
  as.numeric()

# get total number of countries in data set
countries <- df_2019 %>%
  filter(Entity!="World") %>%
  n_distinct() 

# count number of countries below world average 2019
below_world2019 <- df_2019 %>%
  filter(norm2019 < world_avg) %>%
  n_distinct()

# 61% or 118/193 below world average in 2019
below_world2019/countries


##### figure 1 - map of 2019 normalized values #####
# load raw shapefile
world <- st_read(paste(dataraw,
                       "world",
                       "world_fix.shp",
                       sep = "/")) %>%
  select(starts_with("ADMIN"), 
         starts_with("ISO_A3_EH"), 
         geometry) %>% # cleaning up shapefile after processing in qgis
  mutate(ADMIN = case_when(
    is.na(ADMIN) & !is.na(ADMIN_5) ~ ADMIN_5,
    is.na(ADMIN) & !is.na(ADMIN_4) ~ ADMIN_4,
    T ~ ADMIN
  ), 
  ISO_A3_EH = case_when( # minor changes
    ADMIN == "Somalia" ~ "SOM",
    ADMIN == "Kosovo" ~ "KOS",
    ISO_A3_EH_ == "ESH" ~ "ESH",
    T ~ ISO_A3_EH
  )) %>%
  rename(name = ADMIN,
         iso3c = ISO_A3_EH) %>%
  filter(iso3c != "ATA") %>%
  left_join(df_2019) %>%
  mutate(norm2019_cat = ifelse(is.na(norm2019_cat), "No data", norm2019_cat)) %>%
  select(name, iso3c, geometry, norm2019, norm2019_cat)

# disputed boundaries
boundaries <- st_read(paste(dataraw,
                            "world",
                            "disputes_boundary.shp",
                            sep = "/"), crs = 4326)
bound_robin <- st_transform(boundaries, robinson)

# camcorder to set up and output plots
camcorder::gg_record(
  dir = plots,
  device = "png",
  scale = 1,
  width = 6,
  height = 3.5,
  units = "in",
  dpi = 300,
  bg = "white"
)

# plotting normalized index for 2019
# transform to robinson
col <- c("#DFDFDF", "#d0d1e6", "#a6bddb", "#74a9cf", "#3690c0", "#0570b0")

# plot
f1 <- ggplot() + 
  geom_sf(data = world, 
          aes(fill=norm2019_cat, color=col),
          color="#FFFFFF",
          linewidth=0.1) +
  geom_sf(data=bound_robin,
          color="#FFFFFF",
          linewidth=0.2,
          linetype="11") +
  scale_fill_manual(values = col,
                    breaks = c("No data", "0.2","0.4", "0.6", "0.8", "1")) +
  coord_sf(datum = NA,
           xlim = c(-14000000, 15000000), 
           ylim = c(-6000000, 8000000)) +
  theme_minimal(base_family = "Noto Sans") +
  theme(plot.title = element_text(face="bold",
                                  size=12,
                                  margin=margin(t=0, r=0, b=3, l=0, "pt")),
        plot.subtitle = element_text(size=10,
                                     margin=margin(t=0,r=0,b=0,l=0, "pt")),
        plot.margin = unit(c(t=13.5,r=13.5,b=13.5,l=13.5), "pt"),
        plot.caption = element_text(hjust = 0,
                                    size = 6,
                                    color="grey",
                                    margin=margin(t=0,r=0,b=0,l=0, "pt")),
        plot.caption.position = "plot") +
  theme(legend.position = c(1,1),
        legend.margin=margin(t=0,r=0,b=5,l=0, "pt"),
        legend.justification = "right",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.spacing.y= unit(1, "pt"),
        legend.spacing.x= unit(0, "pt"),
        legend.key.width = unit(25, "pt"),
        legend.key.height = unit(10, "pt"),
        legend.text = element_text(size = 8)) +  
  guides(color=guide_legend(nrow = 1),
         fill=guide_legend(nrow=1,
                           label.position = "bottom",
                           label.hjust=1)) +
  labs(title = "Pride divide",
       subtitle = "LGBTQ policy index, 2019, 1=best",
       caption = bquote(paste(bold("Source:")~"Our World in Data, Velasco (2020) • ",
                              ~bold("By:")~"Jan Oledan",
                              sep="\n"))) 

f1

##### figure 2 - dumbbell plot with arrows, select countries #####
# arrow plot from 1991 to 2019
df_1991_2019 <- df %>%
  filter(year %in% c(1991, 2019), !is.na(iso3c)) %>%
  pivot_wider(id_cols = c(Entity, iso3c),
              names_from = year,
              names_glue = "norm{year}",
              values_from = lgbt_index) %>%
  mutate(norm1991 = (norm1991 + 5)/(18),
         norm2019 = (norm2019 + 5)/(18),
         iso3c = if_else(Entity=="Kosovo", "KOS", iso3c),
         Entity = ifelse(Entity=="World", "World average", Entity),
         change = norm2019-norm1991) %>%
  filter(Entity %in% c("Nigeria", 
                       "Japan", 
                       "Italy", 
                       "World average",
                       "Mexico",
                       "Qatar",
                       "Paraguay",
                       "South Africa",
                       "Cambodia")) %>%
  pivot_longer(cols = c(norm1991, norm2019))

# refactor to order in plot
df_1991_2019$Entity <- factor(df_1991_2019$Entity,
                              levels=c("Nigeria",
                                       "Qatar", 
                                       "Paraguay",
                                       "Cambodia",
                                       "World average",
                                       "Italy",
                                       "Japan", 
                                       "Mexico",
                                       "South Africa"))

# fake data for legend
fake_dat <- tibble(
  country = c(1),
  year_1991 = c(1),
  year_2019 = c(1.1)
) 

fake_dat_longer <- fake_dat |> 
  pivot_longer(
    cols = -country,
    names_to = 'label',
    values_to = 'life_exp',
    names_prefix = 'year_'
  ) 


custom_legend <- ggplot() +
  geom_rect(
    aes(xmin = 1, xmax = 1,
        ymin = 1, ymax = 1),
    fill = 'white',
    col = 'white'
  ) + geom_segment(
    data = fake_dat,
    mapping = aes(
      x = year_1991, xend = year_2019, 
      y = country, yend = country,
    ),
    arrow = arrow(angle = 20, 
                  length = unit(5, "pt"),
                  type = "closed"),
    linewidth = 1,
    color = "grey"
  ) +
  geom_text(
    data = fake_dat_longer,
    mapping = aes(x = life_exp, y = country, label = label),
    hjust = c(1, -0.1),
    family = 'Noto Sans',
    size = 3,
    color = "grey"
  ) +
  theme_void() +
  coord_cartesian(
    ylim = c(0, 1.3),
    xlim = c(0.75, 2.25), 
    expand = F
  ) 

#custom_legend

# arrow plot!
f2 <- ggplot(df_1991_2019, 
             aes(x = value, y = Entity)) +
  #geom_point(aes(fill=name), size = 1) +
  geom_path(aes(x = value, 
                y = Entity, 
                color = I(ifelse(change < 0, "#D55E00", "#0072B2")),
                group = Entity), 
            arrow = arrow(angle = 20, 
                          length = unit(5, "pt"),
                          type = "closed"),
            linewidth = 1,
            show.legend=T) +
  theme_minimal(base_family = "Noto Sans") +
  scale_x_continuous(position = "top", # move the x axis labels up top
                     breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                     limits = c(0, 1),
                     expand = c(0, 0))  +
  theme(panel.border = element_blank(),  # Remove the panel border
        panel.grid.minor.x = element_blank(), # remove minor x, y lines
        panel.grid.minor.y = element_blank()) + 
  theme(axis.ticks.y = element_blank(), # remove y axis ticks
        axis.line.x = element_line(), # remove x line
        axis.title.x = element_blank(), # remove x, y axis title
        axis.title.y = element_blank(),
        axis.text.y = element_text(hjust=0)) + # adjust country names, left align
  theme(plot.title = element_text(face="bold",
                                  size=12,
                                  margin=margin(t=0,r=0,b=3,l=0, unit="pt")),
        plot.subtitle = element_text(size=10),
        #plot.margin = unit(c(5,5,5,5), "pt"),
        plot.margin = unit(c(t=10,r=10,b=10,l=10), "pt"),
        #plot.background=element_rect(fill='#e3fbff'),
        plot.caption = element_text(hjust = 0,
                                    size=6,
                                    color = "grey"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8)) +
  labs(title = "Forward and back",
       subtitle = "Change in LGBTQ policy index, 1991-2019, 1=best",
       caption = bquote(paste(bold("Source:")~"Our World in Data, Velasco (2020) • ",
                              ~bold("Visual:")~"Jan Oledan",
                              sep="\n"))) +
  inset_element(custom_legend, left = 0.5, right = 2.4, top = 1.19, bottom = 1)

f2

#### end of code #####
