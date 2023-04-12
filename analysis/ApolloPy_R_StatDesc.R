## ----setup, include=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(dpi=300,fig.width=7)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# clear workspace
rm(list = ls())

# load data relating to Baleroc 25 hc for each class and spec
files <- list.files("./data_csv/", pattern = "Baleroc_25hc*")

# load data
data          <- lapply(paste0("./data_csv/", files), read.csv)
data_dps      <- lapply(data, function(x) {x$dps})
data_ilvl     <- lapply(data, function(x) {x$avg_item_lvl})
data_fightlen <- lapply(data, function(x) {x$length})

# formatting class / spec
class_spec <- lapply(files, function(x) {strsplit(x, "_")[[1]][3:4]})
class      <- lapply(class_spec, function(x) {x[1]})
spec       <- lapply(class_spec, function(x) {sub(" ", "_", x[2])})


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# append NAs to each vector if shorter than max_len
max_len <- max(unlist(lapply(data_dps, function(x) {length(x)})))

data_dps_na <- list()
for (i in seq_along(data_dps)) {
 if (length(data_dps[[i]])<max_len) {
   data_dps_na[[i]] <- c(data_dps[[i]], rep(NA_character_, max_len-length(data_dps[[i]])))
 } else {
   data_dps_na[[i]] <- data_dps[[i]]
 }
}

data_df <- data.frame(x = rep(NA_character_, max_len))

# append columns
for (i in seq_along(data_dps_na)) {
 data_df[, i] <- data_dps_na[[i]]
}


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# rename columns
class_spec_2 <- list()
for (i in seq_along(class)) {
  class_spec_2[i] <- paste0(class[[i]], ".", spec[[i]])
}
colnames(data_df) <- class_spec_2


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
suppressPackageStartupMessages(library(tidyverse))

data_df <- plyr::colwise(as.numeric)(data_df)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# convert wide to long format (ignore NAs)
data_df_long <- data_df %>%
  gather(key = "class_spec", value = "DPS") %>%
  na.omit() %>%
  mutate()


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data_df_long$class <- lapply(data_df_long$class_spec, function(x) {strsplit(x, ".", fixed = TRUE)[[1]][1]})
data_df_long$class <- lapply(data_df_long$class, toupper)

data_df_long$spec <- lapply(data_df_long$class_spec, function(x) {strsplit(x, ".", fixed = TRUE)[[1]][2]})
data_df_long$spec <- lapply(data_df_long$spec, toupper)

# trim leading white space
data_df_long$class <- as.character(trimws(data_df_long$class))
data_df_long$spec <- as.character(trimws(data_df_long$spec))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Boxplot
# Order: DK, Druid, Hunter, Mage, Paladin, Priest, Rogue, Shaman, Warrior, Warlock
class_colors <- c("#C41E3A", "#FF7C0A", "#AAD372", "#3FC7EB", "#F48CBA", "#FFFFFF", "#FFF468", "#0070DD", "#C69B6D", "#8788EE")

data_df_long %>%
  ggplot(aes(x=class, y=DPS, fill = class, color = class)) +
  geom_boxplot() + 
  geom_jitter(color="black", size=0.4, alpha=0.7) +
  scale_fill_manual(values=class_colors) +
  ggtitle("DPS Comparison: All Classes / All Specs") +
  xlab("Classes") +
  labs(fill="Classes") +
  guides(color=FALSE) +
  theme_classic()


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Violin Plot
data_df_long %>%
  ggplot(aes(x=class, y=DPS, fill = class, color = class)) +
  geom_violin() +
  scale_fill_manual(values=class_colors) +
  ggtitle("DPS Comparison: All Classes / All Specs") +
  xlab("Classes") +
  labs(fill="Classes") +
  guides(color=FALSE) +
  theme_classic()


## ----echo=FALSE, results='asis'------------------------------------------------------------------------------------------------------------------------------------------------------------
suppressPackageStartupMessages(library(pastecs))
suppressPackageStartupMessages(library(knitr))
kable(format(stat.desc(data_df[,1:5]), scientific=FALSE, digits=2))


## ----echo=FALSE, results='asis'------------------------------------------------------------------------------------------------------------------------------------------------------------
kable(format(stat.desc(data_df[,6:10]), scientific=FALSE, digits=2))


## ----echo=FALSE, results='asis'------------------------------------------------------------------------------------------------------------------------------------------------------------
kable(format(stat.desc(data_df[,11:15]), scientific=FALSE, digits=2))


## ----echo=FALSE, results='asis'------------------------------------------------------------------------------------------------------------------------------------------------------------
kable(format(stat.desc(data_df[,16:20]), scientific=FALSE, digits=2))


## ----echo=FALSE, results='asis'------------------------------------------------------------------------------------------------------------------------------------------------------------
kable(format(stat.desc(data_df[,21:25]), scientific=FALSE, digits=2))


## ----echo=FALSE, results='asis'------------------------------------------------------------------------------------------------------------------------------------------------------------
kable(format(stat.desc(data_df[,25:29]), scientific=FALSE, digits=2))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# identifying highest DPS spec for each class
classes_unique <- unique(unlist(class))
max_dps_list <- list()
max_dps_ind <- list()

for (c in classes_unique) {
  max_dps_spec <- which.max(plyr::colwise(max, na.rm=TRUE)(data_df[, grep(pattern=paste0(c, ".*"), names(data_df))]))
  max_dps_ind <- append(max_dps_ind, max_dps_spec)
  
  max_dps_val <- plyr::colwise(max, na.rm=TRUE)(data_df[, grep(pattern=paste0(c, ".*"), names(data_df))])
  max_dps_list <- append(max_dps_list, max_dps_val)
}

max_dps_spec_names <- names(max_dps_ind)

# rename non-unique specs 
data_df_long$spec[data_df_long$spec=="HOLY" & data_df_long$class=="PRI"] <- rep(x="HOLY_PRI", times=length(data_df_long$spec[data_df_long$spec=="HOLY" & data_df_long$class=="PRI"]))
data_df_long$spec[data_df_long$spec=="HOLY" & data_df_long$class=="PAL"] <- rep(x="HOLY_PAL", times=length(data_df_long$spec[data_df_long$spec=="HOLY" & data_df_long$class=="PAL"]))

data_df_long$spec[data_df_long$spec=="FROST" & data_df_long$class=="DK"] <- rep(x="FROST_DK", times=length(data_df_long$spec[data_df_long$spec=="FROST" & data_df_long$class=="DK"]))
data_df_long$spec[data_df_long$spec=="FROST" & data_df_long$class=="MAG"] <- rep(x="FROST_MAG", times=length(data_df_long$spec[data_df_long$spec=="FROST" & data_df_long$class=="MAG"]))

data_df_long$spec[data_df_long$spec=="RESTORATION" & data_df_long$class=="DRU"] <- rep(x="RESTORATION_DRU", times=length(data_df_long$spec[data_df_long$spec=="RESTORATION" & data_df_long$class=="DRU"]))
data_df_long$spec[data_df_long$spec=="RESTORATION" & data_df_long$class=="SHAM"] <- rep(x="RESTORATION_SHAM", times=length(data_df_long$spec[data_df_long$spec=="RESTORATION" & data_df_long$class=="SHAM"]))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# plot max DPS for each Class - Specialization Combination

class_spec_count   <- list()
class_spec_list    <- list()
for (i in unique(data_df_long$class)) {
  class_spec_list    <- append(class_spec_list, unique(data_df_long$spec[data_df_long$class==i]))
  class_spec_count   <- append(class_spec_count, length(unique(data_df_long$spec[data_df_long$class==i])))
}

class_spec_max_dps <- list()
for (i in class_spec_list) {class_spec_max_dps <- append(class_spec_max_dps, max(data_df_long$DPS[data_df_long$spec==i]))}

plot_df_max_dps <- data.frame(class_spec=unique(data_df_long$class_spec),
                              DPS=unlist(class_spec_max_dps),
                              class=rep(unique(data_df_long$class),unlist(class_spec_count)),
                              spec=unique(data_df_long$spec))

#factor classes + specs to keep order
plot_df_max_dps$class <- factor(plot_df_max_dps$class, levels=unique(data_df_long$class))
plot_df_max_dps$spec  <- factor(plot_df_max_dps$spec, levels=class_spec_list)

plot_df_max_dps %>% 
  ggplot( aes(x=spec, y=DPS, fill=class, color=spec)) +
  geom_bar(color="black", stat="identity", position="dodge") +
  scale_fill_manual(values=class_colors) +
  xlab("Talent Specialization") +
  labs(fill="Class") +
  ggtitle("Highest DPS values per Talent Specialization") +
  theme_linedraw() +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# max DPS specs
spec_number_cases <- colSums(! is.na(data_df))
max_dps_spec_number_cases <- spec_number_cases[max_dps_spec_names]
data_df_max_dps <- data_df[max_dps_spec_names]

# convert to long format
data_df_max_dps_long <- data_df_max_dps %>%
  gather(key = "class_spec", value = "DPS") %>%
  na.omit() %>%
  mutate()

data_df_max_dps_long$class <- lapply(data_df_max_dps_long$class_spec, function(x) {strsplit(x, ".", fixed = TRUE)[[1]][1]})
data_df_max_dps_long$class <- lapply(data_df_max_dps_long$class, toupper)

data_df_max_dps_long$spec <- lapply(data_df_max_dps_long$class_spec, function(x) {strsplit(x, ".", fixed = TRUE)[[1]][2]})
data_df_max_dps_long$spec <- lapply(data_df_max_dps_long$spec, toupper)

# trim leading white space
data_df_max_dps_long$class <- as.character(trimws(data_df_max_dps_long$class))
data_df_max_dps_long$spec <- as.character(trimws(data_df_max_dps_long$spec))

# relabel axes
data_df_max_dps_long_classes <- unique(data_df_max_dps_long$class)
data_df_max_dps_long_specs <- unique(data_df_max_dps_long$spec)

new_x_axis <- lapply(max_dps_spec_number_cases, function(x) paste0("n=", x))
new_legend <- mapply(function(x, y) {paste(x, y, sep=": ")},
                     x=data_df_max_dps_long_classes,
                     y=data_df_max_dps_long_specs)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# barplots comparing best DPS specializations
new_x_axis <- unname(unlist(new_x_axis))
data_df_max_dps_long %>% 
  ggplot( aes(x=class, y=DPS, fill=class_spec, color=class_spec)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.7) +
  scale_fill_manual(values=class_colors) +
  ggtitle("DPS Comparison: Best DPS Specialisation of each Class") +
  xlab("Classes") +
  scale_x_discrete(labels=new_x_axis) +
  labs(fill="Class & Specialisation") +
  guides(color=FALSE) +
  theme_classic()


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# violinplots comparing best DPS specializations
data_df_max_dps_long %>% 
  ggplot( aes(x=class, y=DPS, fill=class_spec, color=class_spec)) +
  geom_violin() +
  scale_fill_manual(values=class_colors) +
  ggtitle("DPS Comparison: Best DPS Specialisation of each Class") +
  xlab("Classes") +
  scale_x_discrete(labels=new_x_axis) +
  labs(fill="Class & Specialisation") +
  guides(color=FALSE) +
  theme_classic()


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# create a full dataframe (including item-level, fight length, and guild membership)
data_guild <- lapply(data, function(x) {x$guild})

df_full_long          <- data_df_long[,1:2]
df_full_long$ilvl     <- unlist(data_ilvl)
df_full_long$fightlen <- unlist(data_fightlen)
df_full_long$guild    <- unlist(data_guild)

df_full_long$guild <- ifelse(df_full_long$guild=="", NA, df_full_long$guild)

# remove non-dps specs
non_dps <- c("Dk.Blood", "Dru.Restoration", "Pal.Holy", "Pal.Protection", "Pri.Discipline", "Pri.Holy", "Sham.Restoration")
df_dps_full_long <- df_full_long %>% dplyr::filter(!(class_spec %in% non_dps))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# compare damage of characters in a guild vs. characters in no guild
guild_dps <- df_dps_full_long$DPS[!is.na(df_dps_full_long$guild)]
noguild_dps <- df_dps_full_long$DPS[is.na(df_dps_full_long$guild)]

mean_dps_guild <- mean(guild_dps)
mean_dps_noguild <- mean(noguild_dps)

sd_dps_guild <- sd(guild_dps)
sd_dps_noguild <- sd(noguild_dps)

max_dps_guild <- max(guild_dps)
max_dps_noguild <- max(noguild_dps)

min_dps_guild <- min(guild_dps)
min_dps_noguild <- min(noguild_dps)

cat(paste0("Member of a guild:\nN = ",length(guild_dps), 
             "\nMean DPS = ", round(mean_dps_guild, 2),
             "\nSD = ", round(sd_dps_guild, 2),
             "\nRange = ", round(min_dps_guild, 2), " - ", round(max_dps_guild, 2)
             ))

cat(paste0("\n\nNon-Members:\nN = ",length(noguild_dps), 
             "\nMean DPS = ", round(mean_dps_noguild, 2),
             "\nSD = ", round(sd_dps_noguild, 2),
             "\nRange = ", round(min_dps_noguild, 2), " - ", round(max_dps_noguild, 2)
             ))

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# boxplot comparing average DPS for guild vs. not-guild members

guild_bxplt_df <- data.frame(
  guild=guild_dps,
  noguild=append(noguild_dps, rep(NA, length(guild_dps)-length(noguild_dps)))
)
guild_bxplt_df <- guild_bxplt_df %>% 
  gather(key="guild_factor", value="DPS") %>% 
  mutate()

guild_bxplt_df <- guild_bxplt_df %>% dplyr::filter(DPS>1000)

library(viridis)
guild_bxplt_df %>% 
  ggplot( aes(x=guild_factor, y=DPS, fill=guild_factor, color=guild_factor)) +
  geom_boxplot() +
  stat_boxplot(geom="errorbar", width=0.4) +
  geom_jitter(color="black", size=0.4, alpha=0.3) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, labels=c("Guild Member", "Not Guild Member")) +
  ggtitle("Comparison: DPS depending on Guild Membership") +
  xlab("") +
  scale_x_discrete(labels=c(paste0("Guild Members (n=", length(guild_dps), ")"), 
                            paste0("Not Guild Members (n=", length(noguild_dps), ")"))) + 
  labs(fill="") +
  guides(color=FALSE) +
  theme_classic()


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# histogram with normal distribution laid over

guild_bxplt_df_guild <- data.frame(x=guild_bxplt_df$DPS[guild_bxplt_df=="guild"])
guild_bxplt_df_guild %>% 
  ggplot( aes(x=x)) +
  geom_histogram( aes(y=..density..),
    color="black", fill="#3399FF") +
  stat_function(fun = dnorm, args = list(mean=mean(guild_bxplt_df_guild$x), sd=sd(guild_bxplt_df_guild$x))) +
  ggtitle("Boxplot with Normal Curve: Guild Members") +
  ylab("") +
  xlab("DPS") +
  theme_classic() 



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# histogram with normal distribution laid over

guild_bxplt_df_guild <- data.frame(x=guild_bxplt_df$DPS[guild_bxplt_df=="noguild"])
guild_bxplt_df_guild %>% 
  ggplot( aes(x=x)) +
  geom_histogram( aes(y=..density..),
    color="black", fill="#3399FF") +
  stat_function(fun = dnorm, args = list(mean=mean(guild_bxplt_df_guild$x), sd=sd(guild_bxplt_df_guild$x))) +
  ggtitle("Boxplot with Normal Curve: Not Guild Members") +
  ylab("") +
  xlab("DPS") +
  theme_classic() 


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# test equal variances (Levene's Test)
suppressPackageStartupMessages(library(car))
leveneTest(DPS ~ factor(guild_factor), guild_bxplt_df)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
t.test(DPS ~ factor(guild_factor), guild_bxplt_df, var.equal=TRUE)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Bayesian t-test: do both samples have similar means
suppressPackageStartupMessages(library(BayesFactor))
ttestBF(x=guild_bxplt_df$DPS[guild_bxplt_df$guild_factor=="guild"], y=guild_bxplt_df$DPS[guild_bxplt_df$guild_factor=="noguild"])

