
# 1.1 Objekte erstellen ---------------------------------------------------
  # Führt die Operation aus und lasst euch x ausgeben.

x <- 10
x

# 1.2 Numerische Variablen ------------------------------------------------
  # Erstellt nun ein numerisches Objekt namens x_num 💾, das die Werte 1 bis 5 enthält.

x_num <- c(1:5)


# 1.3 Kategorische Variablen ----------------------------------------------
  #  Generiert die Variable x_char! Überprüft anschließend, ob die neue Variable als numeric oder als character vorliegt.

x_char <- c("eins", "zwei", "drei", "vier", "fünf")
class(x_char)


# 3 Dataframes ------------------------------------------------------------
  # Erstellt ein Dataframe, mithilfe von data.frame(x_num, x_char) und weist es einem neuen Objekt namens df 💾 zu!

df <- data.frame(x_num, x_char)

  # oder über cbind

df <- cbind(x_num, x_char)
df <- data.frame(df)


# 4 Umfragedaten ----------------------------------------------------------

# 4.1 Daten einlesen ------------------------------------------------------
  # Lest die Daten ein und weist sie einem Objekt namens survey 💾 zu!

survey <- readRDS("Data/survey_red.rds")

# 4.2 Daten erkunden ------------------------------------------------------
  # Wie viele Studierende haben an der Umfrage teilgenommen? Wie viele Variablen enthält der Datensatz?

View(survey)

library(tidyverse)
library(magrittr)

count(survey)
names(survey)
ncol(survey)
nrow(survey)
dim(survey)
# 27 Studierende haben an der Umfrage teilgenommen
# Der Datensatz enthält 15 Variablen

# 5 Deskriptive Analysen --------------------------------------------------

# 5.1 Studiengang ---------------------------------------------------------
# Wie sieht die absolute und relative Verteilung der Studiengänge in unserem Seminarkurs aus?

table(survey$studiengang)

table(survey$studiengang) %>% proportions * 100

# Schaut euch die Verteilung mit Hilfe des data_tabulate()-Befehls an

install.packages("easystats")
library(easystats)

data_tabulate(survey$studiengang)

# 5.2 Interesse -----------------------------------------------------------
# Untersucht die Verteilung des Interesses an ländervergleichender Forschung im Seminar.

summary(survey$int_vergl)

# Das Interesse an ländervergl. Forschung im Seminar weist ein arithmetische Mittel von 4,63 auf
# Der Median liegt bei 5
# Die Spannweite beträgt 2 (Min = 3, Max = 5)
# Der Interquartilsabstand liegt bei 1 (Q3(4) - Q1(5))

sd(survey$int_vergl)

# Die Standardabweichung liegt bei 0,565
# Ausgehend von der 68-95-94 Faustregel liegen unter Annahme einer Normalverteilung etwa 68% der Werte zwischen 4,065 und 5 (mean(4,63) +/- sd*1(0,565) 
# -> Spannweite, IQR und SD weisen auf wenig Variation hin

# 5.3 Vorwissen in Statistik ----------------------------------------------
# ✏️ Generiert die Variable wissen und schaut euch die Verteilung der Vorkenntnisse an.
# 🔨 Tipp: Verwendet hierzu die %>%-Pipe.

stat_score <- c(survey$wis_stat_desk + survey$wis_stat_zuhm + survey$wis_stat_reglin + survey$wis_stat_reglog)

# Beginner, wenn Studierende einen Score von kleiner oder gleich 20 haben,
# Intermediate, wenn Studierende einen Score zwischen 21 und 30 haben,
# Advanced, wenn Studierende einen Score zwischen 31 und 40 haben und
# Expert, wenn Studierende einen Score größer als 40 haben.

survey <- survey %>% 
  mutate(wissen = case_when(
    stat_score <= 20 ~ "Beginner",
    stat_score >= 21 & stat_score <= 30 ~ "Intermediate",
    stat_score >= 31 & stat_score <= 40 ~ "Advanced",
    stat_score > 40 ~ "Expert")
)

survey %>% data_tabulate(wissen) 

#Erstellt eine neue Variable wissen_f, die die wissen Variable zu einer factor Variable macht. Achtet darauf, dass die alte Variable wissen nicht überschrieben wird.

wissen_f <- as.factor(survey$wissen)                                   # Wissen in Faktor wandeln und in Wissen_F speichern
wissen_f_levels <- c("Beginner", "Intermediate", "Advanced", "Expert") # Vektor mit Levelordnung erstellen
wissen_f <- factor(wissen_f, levels = wissen_f_levels)                 # Faktorvektor wissen_f Levelordnung zuweisen und in wissen_f speichern 
survey$wissen_f <- wissen_f

# 5.4 Erfahrung mit Statistiksoftware nach statistischen Vorkenntn --------
# ✏️ Mit wie vielen Statistikprogrammen haben die unterschiedlichen Typen (wissen_f) im Durchschnitt bereits gearbeitet? Nutzt hierfür die aggregate() Funktion. Lässt sich ein Muster erkennen?
#🔨 Tipp: schaut euch die Beispiele und Argumente auf der Hilfsseite der Funktion an (?aggregate).

aggregate(software_anz ~ wissen_f, data = survey, FUN = mean)

# Eine alternativer Weg wäre mit group_by() und summarise()

#survey %>% 
#  group_by(wissen_f) %>% # für jede Ausprägugn von wissen_f wird gruppiert
#  summarise(software_anz_mean =
#              mean(software_anz) # für jede Gruppe wird ein Mittelwert berechnet
#  )

#wissen_f        software_anz
#Beginner        1.0
#Intermediate    1.5
#Advanced        2.1
#Expert          3.5
#Personen mit höherem Wissenslevel, haben im Durchschnitt Erfahrung mit mehr Statistikprogrammen

# 5.5 Erstellen eines Sub-Datensatzes -------------------------------------
# Erstellt einen neuen Datensatz survey_soz 💾, der nur noch Studierende der soziologischen Masterstudiengänge beinhaltet.

survey_soz <- filter(survey, studiengang == "Wirtschaftssoziologie" | studiengang == "Soziologie")

#Alternative 
#survey_soz <- survey %>% 
#  filter(studiengang == "Soziologie" | studiengang == "Wirtschaftssoziologie")

#✏️ Wie steht es um das Vorwissen zu Mehrebenenanalysen (wis_ml) unter den Studierenden aus den soziologischen MA-Studiengängen, differenziert nach den Erfahrungstypen in Statistik (wissen_f)?
# Reduziert zunächst den Datensatz auf die relevanten Variablen. Schaut euch danach die Häufigkeitsverteilungen von Vorwissen zu Mehrebenenanalysen nach Erfahrungstyp an.

survey_soz <- survey_soz %>% select(wis_ml, wissen_f)

table(survey_soz$wis_ml, survey_soz$wissen_f)

#survey_soz %>% 
#  group_by(wissen_f) %>% 
#  data_tabulate(wis_ml)

