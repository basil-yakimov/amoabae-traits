library(readxl)
library(tidyverse)
library(ggplot2)


#--------------# Loading trait data
td <- read_xlsx("species-level_trait.xlsx", sheet = "Traits")


# Drawing format
th <- theme(plot.title=element_text(size=20, color="black", family  = "sans", face= "bold",vjust=0.5,hjust=0.5),
            axis.line=element_line(size=.5, colour="black"),
            axis.ticks=element_line(color="black"),
            axis.text= element_text(size = 13, color="black", family  = "sans", face= "bold", vjust=0.5, hjust=0.5),
            axis.title = element_text(size=20, color="black", family  = "sans",face= "bold", vjust=0.5, hjust=0.5),
            legend.position="none",
            legend.background=element_blank(),
            legend.key=element_blank(),
            legend.text = element_text(colour = 'black', size = 20,  family  = "sans",face = 'bold'),
            legend.title=element_blank(),
            panel.background=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank()  ) 


# Figure 9. Number of species in each genus for trait data table
spe.sm <- td |> group_by(Genus) |> summarise(n = n())

spe.sm %>%
  ggplot(aes(x = reorder(Genus, -n), y = n)) +
  geom_bar(stat="identity", fill="#f68060", alpha=0.7, width=0.8) +
  coord_flip() +
  labs(x = "", y= "Number of species") +
  th



# Abbreviation of trait name
names(td)[3:20] <- c("Shell_length","Shell_width","Shell_thickness","R1","R2","Shell_shape",
                     "Aperture_length","Aperture_width","R3","R4","Aperture_position","Aperture_invagination",
                     "Aperture_rim","Collar","Shell_cover","Partition","Spine","Feeding_type"
                     )

td$Shell_shape <- factor(td$Shell_shape, levels = c("sphere", "hemisphere", "cylinder", "patelliform", "rectangular cuboid", "ovoid", "pyriform", "spiral"))
td$Aperture_position <- factor(td$Aperture_position, levels = c("straight terminal", "sub terminal", "central ventral", "shifted ventral", "amphistomic"))
td$Aperture_invagination <- factor(td$Aperture_invagination, levels = c("absent", "slightly", "strongly"))
td$Aperture_rim <- factor(td$Aperture_rim, levels = c("straight", "curved", "lobbed", "denticular"))
td$Shell_cover <- factor(td$Shell_cover, levels = c("organic", "xenosomes", "idiosomes", "cleptostomes"))
td$Feeding_type <- factor(td$Feeding_type, levels = c("mixotrophy", "bacterivory", "predatory"))



# Figure 10. Histogram of distribution for the numerical traits
ggplot(data = td, aes(x = Shell_length)) +
  geom_histogram(color="darkblue", fill="lightblue")+ 
  labs(x = "Shell length", y= "Count") +
  th


ggplot(data = td, aes(x = Shell_width)) +
  geom_histogram(color="darkblue", fill="lightblue")+ 
  labs(x = "Shell width", y= "Count") +
  th


ggplot(data = td, aes(x = Shell_thickness)) +
  geom_histogram(color="darkblue", fill="lightblue")+ 
  labs(x = "Shell thickness", y= "Count") +
  th


ggplot(data = td, aes(x = Aperture_length)) +
  geom_histogram(color="darkblue", fill="lightblue")+ 
  labs(x = "Aperture length", y= "Count") +
  th


ggplot(data = td, aes(x = Aperture_width)) +
  geom_histogram(color="darkblue", fill="lightblue")+ 
  labs(x = "Aperture width", y= "Count") +
  th


ggplot(data = td, aes(x = R1)) +
  geom_histogram(color="darkblue", fill="lightblue")+ 
  labs(x = "R1", y= "Count") +
  th


ggplot(data = td, aes(x = R2)) +
  geom_histogram(color="darkblue", fill="lightblue")+ 
  labs(x = "R2", y= "Count") +
  th


ggplot(data = td, aes(x = R3)) +
  geom_histogram(color="darkblue", fill="lightblue")+ 
  labs(x = "R3", y= "Count") +
  th


ggplot(data = td, aes(x = R4)) +
  geom_histogram(color="darkblue", fill="lightblue")+ 
  labs(x = "R4", y= "Count") +
  th



# Figure 11. Barplot of distribution for the categorical and binary traits
ggplot(data = td, aes(x = Shell_shape)) +
  geom_bar(fill="#E69F00", alpha=0.7, width=0.4) +
  ylim(0, 150) +
  coord_flip() +
  stat_count(geom = "text", aes(label = stat(count)),colour="black", fontface = "bold", size = 7, hjust = -.01) +
  labs(x = "", y= "Frequency") +
  th


ggplot(data = td, aes(x = Aperture_position)) +
  geom_bar(fill="#E69F00", alpha=0.7, width=0.2) +
  ylim(0, 250) +
  coord_flip() +
  stat_count(geom = "text", aes(label = stat(count)),colour="black", fontface = "bold", size = 7, hjust = -.01) +
  labs(x = "", y= "Frequency") +
  th


ggplot(data = td, aes(x = Aperture_invagination)) +
  geom_bar(fill="#E69F00", alpha=0.7, width=0.13) +
  ylim(0, 300) +
  coord_flip() +
  stat_count(geom = "text", aes(label = stat(count)),colour="black", fontface = "bold", size = 7, hjust = -.01) +
  labs(x = "", y= "Frequency") +
  th



ggplot(data = td, aes(x = Aperture_rim)) +
  geom_bar(fill="#E69F00", alpha=0.7, width=0.15) +
  ylim(0, 150) +
  coord_flip() +
  stat_count(geom = "text", aes(label = stat(count)),colour="black", fontface = "bold", size = 7, hjust = -.01) +
  labs(x = "", y= "Frequency") +
  th



ggplot(data = td, aes(x = Collar)) +
  geom_bar(fill="#E69F00", alpha=0.7, width=0.1) +
  ylim(0, 310) +
  coord_flip() +
  stat_count(geom = "text", aes(label = stat(count)),colour="black", fontface = "bold", size = 7, hjust = -.01) +
  labs(x = "", y= "Frequency") +
  th



ggplot(data = td, aes(x = Shell_cover)) +
  geom_bar(fill="#E69F00", alpha=0.7, width=0.15) +
  ylim(0, 200) +
  coord_flip() +
  stat_count(geom = "text", aes(label = stat(count)),colour="black", fontface = "bold", size = 7, hjust = -.01) +
  labs(x = "", y= "Frequency") +
  th



ggplot(data = td, aes(x = Partition)) +
  geom_bar(fill="#E69F00", alpha=0.7, width=0.1) +
  ylim(0, 350) +
  coord_flip() +
  stat_count(geom = "text", aes(label = stat(count)),colour="black", fontface = "bold", size = 7, hjust = -.01) +
  labs(x = "", y= "Frequency") +
  th



ggplot(data = td, aes(x = Spine)) +
  geom_bar(fill="#E69F00", alpha=0.7, width=0.1) +
  ylim(0, 310) +
  coord_flip() +
  stat_count(geom = "text", aes(label = stat(count)),colour="black", fontface = "bold", size = 7, hjust = -.01) +
  labs(x = "", y= "Frequency") +
  th


ggplot(data = td, aes(x = Feeding_type)) +
  geom_bar(fill="#E69F00", alpha=0.7, width=0.13) +
  ylim(0, 302) +
  coord_flip() +
  stat_count(geom = "text", aes(label = stat(count)),colour="black", fontface = "bold", size = 7, hjust = -.01) +
  labs(x = "", y= "Frequency") +
  th




