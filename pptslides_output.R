
# -- block for required packages
library(tibble)
library(gtsummary)
library(tidyverse)
library(arsenal)
library(haven)
library(survey)
library(labelled)
library(table1)
library(officer)
library(here)
library(modelsummary)
library(flextable)
library(tinytable)
library(ggeffects)
library(effects)
require(MASS)
library(marginaleffects)
library(here)
library(ftExtra)
library(ghibli)
library(lme4)

# -- end of block



# May introduce here() for workstation

df <- read_dta("/Users/AntonieHsu/Library/CloudStorage/Box-Box/Teaching Materials/R workshop/2025/2025workshop_rdata.dta")

# Short review of tidyverse (maybe introduce tidylog)

df<- df%>%
  mutate(genrelns = to_factor(genrelns),
         fem = ifelse(fem==1, "Female", "Male"), 
         pfem= ifelse(pfem==1, "Female", "Male"),
         postgrad = to_factor(postgrad),
         fem = to_factor(fem), 
         pfem = to_factor(pfem), 
         race = to_factor(race), 
         hhincome_max = to_factor(hhincome_max), 
         kidage = to_factor(kidage))

df<- df%>%
  set_variable_labels(genrelns = "Gender-Relational Contexts", 
                      depsym = "Depressive Symptoms",
                      scoping = "Spousal Support",
                      yrsliv = "Years Lived Together",
                      rage = "Respndent's Age",
                      postgrad = "College and Above", 
                      fem = "Respodent's Gender", 
                      pfem = "Partner's Gender", 
                      race = "Race", 
                      hhincome_max = "Household Income",
                      kidage = "Having Kids under 18")


df$genrelns <- droplevels(df$genrelns)

### Modelings
mod2 <- lmer(depsym ~ scoping * genrelns + rage + yrsliv + race + postgrad + (1 | cid), data = df, REML = FALSE)



pred_ds <-  ggpredict(mod2, terms = c("scoping", "genrelns"))

data.frame(pred_ds)

format <-  theme_minimal() + 
  theme(axis.text = element_text(size = 15, face = "bold"),
        axis.title = element_text(size=16, face = "bold"), 
        legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 16),
        strip.text = element_text(size=14, face = "bold"),
        text = element_text('Comic Sans MS')) # 'serif', 'sans' 
pd <- position_dodge(width = 0.4)

plot1<-  pred_ds%>%
  filter(group == c("Wom w/Man", "Man w/Wom"))%>%
  ggplot( aes(x = x, y = predicted, 
                    color = group)) +
  geom_line(position = pd) +  
  geom_point(aes(shape = group), size = 3, position = pd) +
  geom_errorbar(aes(ymin = conf.low,
                    ymax = conf.high),
                position = pd, 
                width = 0.3 ,linewidth = 0.3)+
  scale_color_grey(start = 0, end = .7)+
  labs(
    x = "Spousal Support",
    y = "Pred. Dep. Sym.",
    color = "Gender-Relational", shape = "Gender-Relational"
  ) + format

plot2<-  pred_ds%>%
  ggplot( aes(x = x, y = predicted, 
              color = group)) +
  geom_line(position = pd) +  
  geom_point(aes(shape = group), size = 3, position = pd) +
  geom_errorbar(aes(ymin = conf.low,
                    ymax = conf.high),
                position = pd, 
                width = 0.3 ,linewidth = 0.3)+
  scale_color_grey(start = 0, end = .7)+
  labs(
    x = "Spousal Support",
    y = "Pred. Dep. Sym.",
    color = "Gender-Relational", shape = "Gender-Relational"
  ) + format


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ...looping the figures in a slideshow
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pptplot_list<- list(plot1, plot2)

output_folder <- c("/Users/AntonieHsu/Library/CloudStorage/Box-Box/Teaching Materials/R workshop/UT_R_2025/")
timestamp <- Sys.Date()

pptfile <- tempfile(fileext = ".pptx", tmpdir = output_folder)

pptx <- read_pptx()

for(i in 1:length(pptplot_list)){
  pptx <- add_slide(pptx)
  pptx <- ph_with(pptx,
                  value = pptplot_list[[i]],
                  location = ph_location_fullsize() )
  
}

print(pptx, pptfile)

destination_file <- file.path(paste0(output_folder, timestamp, "R_workshop.pptx"))
file.rename(pptfile, destination_file)
