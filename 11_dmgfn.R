## Create income differentiated vulnerability curves 

library(readr)
library(ggplot2)


# read global fn
df_glob <- read_csv("D:/Jeanette/00_Data/Damage-depth function/global_dmgfn_res.csv")
df_shanty <- read_csv("D:/Jeanette/00_Data/Damage-depth function/shanty_srilanka_dmgfn.csv")
df_c3a <- read_csv("D:/Jeanette/00_Data/Damage-depth function/englhardt_class1_dmg_fn.csv")
df_c2 <- read_csv("D:/Jeanette/00_Data/Damage-depth function/englhardt_class2_dmg_fn.csv")
df_c3a <- read_csv("D:/Jeanette/00_Data/Damage-depth function/englhardt_class3a_dmg_fn.csv")
df_c3b <- read_csv("D:/Jeanette/00_Data/Damage-depth function/englhardt_class3b_dmg_fn.csv")


df_c3a
# interpolate between values
df_interp_glob <-  approx(unlist(df_glob[1]), unlist(df_glob[2]), seq(0, 10, by = 0.01)) # df[1]=wd, #df[2]=damagefrac
df_interp_glob <- as.data.frame(df_interp_glob)

df_interp_shanty <- approx(unlist(df_shanty[1]), unlist(df_shanty[2]), seq(0,10,by=0.01))
df_interp_shanty <- as.data.frame(df_shanty_interp)

df_interp_c1 <-  approx(unlist(df_c1[1]), unlist(df_c1[2]), seq(0, 10, by = 0.01)) # df[1]=wd, #df[2]=damagefrac
df_interp_c1 <- as.data.frame(df_interp_c1)

df_interp_c2 <-  approx(unlist(df_c2[1]), unlist(df_c2[2]), seq(0, 10, by = 0.01)) # df[1]=wd, #df[2]=damagefrac
df_interp_c2 <- as.data.frame(df_interp_c2)

df_interp_c3a <-  approx(unlist(df_c3a[1]), unlist(df_c3a[2]), seq(0, 10, by = 0.01)) # df[1]=wd, #df[2]=damagefrac
df_interp_c3a <- as.data.frame(df_interp_c3a)

df_interp_c3b <-  approx(unlist(df_c3b[1]), unlist(df_c3b[2]), seq(0, 10, by = 0.01)) # df[1]=wd, #df[2]=damagefrac
df_interp_c3b <- as.data.frame(df_interp_c3b)


# rename columns
colnames(df_interp_glob) <- c("depth", "df")
colnames(df_interp_shanty) <- c("depth", "df")
colnames(df_interp_c1) <- c("depth", "df")
colnames(df_interp_c2) <- c("depth", "df")
colnames(df_interp_c3a) <- c("depth", "df")
colnames(df_interp_c3b) <- c("depth", "df")

df_interp_glob$cat <- rep("Huizinga et al. (2017)",nrow(df_interp_glob))
df_interp_shanty$cat <- rep("Wagenaar et al. (2019)",nrow(df_interp_shanty))
df_interp_c1$cat <- rep("Englhardt et al. (2019): I",nrow(df_interp_c1))
df_interp_c2$cat <- rep("Englhardt et al. (2019): II",nrow(df_interp_c2))
df_interp_c3a$cat <- rep("Englhardt et al. (2019): IIIa",nrow(df_interp_c3a))
df_interp_c3b$cat <- rep("Englhardt et al. (2019): IIIb",nrow(df_interp_c3b))



combined_df <- rbind(df_interp_shanty, df_interp_glob, df_interp_c1, df_interp_c2, df_interp_c3b, df_interp_c3a)
combined_df$cat <- factor(combined_df$cat, levels= c("Huizinga et al. (2017)",
                                                     "Wagenaar et al. (2019)",
                                                     "Englhardt et al. (2019): I",
                                                     "Englhardt et al. (2019): II",
                                                     "Englhardt et al. (2019): IIIa",
                                                     "Englhardt et al. (2019): IIIb"))

head(combined_df)
# plot function
ggplot(combined_df, aes(x = depth, y = df, linetype = cat)) +
  # geom_smooth(se=F, color="black", linewidth=0.8) +
  # geom_line(size=1)+
  geom_smooth(method="loess",se=F, span=0.555)+
  labs(x = "Depth (m)", y = "Damage Fraction", linetype = "Category") +
  scale_linetype_manual(values=c("Englhardt et al. (2019): I" = "dotted", "Englhardt et al. (2019): II"="dotdash", "Englhardt et al. (2019): IIIa"="dashed", "Huizinga et al. (2017)"="solid", "Englhardt et al. (2019): IIIb" = "twodash", "Wagenaar et al. (2019)"="longdash"))+
  theme_minimal() +
  xlim(0,6)+
  ylim(0,1)

