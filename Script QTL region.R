setwd(r"(D:/output_PHIPSII)")
input_file <- "out_20.assoc.txt"

data1 <- read.table(input_file, header = TRUE)

N <- length(data1$p_wald)
expected_p <- (1:N) / N

reduce <- data1[data1$p_wald < 0.005, ]

reduce$chr[reduce$chr == "NC_056623.1"] <- 1
reduce$chr[reduce$chr == "NC_056624.1"] <- 2
reduce$chr[reduce$chr == "NC_056625.1"] <- 3
reduce$chr[reduce$chr == "NC_056626.1"] <- 4
reduce$chr[reduce$chr == "NC_056627.1"] <- 5
reduce$chr[reduce$chr == "NC_056628.1"] <- 6
reduce$chr[reduce$chr == "NC_056629.1"] <- 7
reduce$chr[reduce$chr == "NC_056630.1"] <- 8
reduce$chr[reduce$chr == "NC_056631.1"] <- 9

chr_of_interest <- 1
start_pos <- 206604115
end_pos <- 209686575


# Filter SNPs in de regio
region_df <- subset(reduce,
                    chr == chr_of_interest &
                      ps >= start_pos &
                      ps <= end_pos)


region_df$log10_p <- -log10(region_df$p_wald)


filtered_df <- region_df[region_df$log10_p >= 5, ]

ggplot(filtered_df, aes(x = ps, y = -log10(p_wald))) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_vline(xintercept = c(start_pos, end_pos), color = "red", linetype = "dashed") +
  annotate("rect", xmin = start_pos, xmax = end_pos, ymin = -Inf, ymax = Inf,
           alpha = 0.1, fill = "red") +
  labs(title = paste("Zoomed-in Manhattan Plot: chr", chr_of_interest, sep = ""),
       x = paste("Position on Chromosome", chr_of_interest),
       y = expression(-log10)) +
  theme_minimal ()                  

chr_of_interest <- 3
start_pos <- 229900000
end_pos <- 231100000

# Filter SNPs in de regio
region_df_chr3 <- subset(reduce,
                    chr == chr_of_interest &
                      ps >= start_pos &
                      ps <= end_pos)


region_df_chr3$log10_p <- -log10(region_df_chr3$p_wald)


filtered_df_chr3 <- region_df_chr3[region_df_chr3$log10_p >= 5, ]

ggplot(filtered_df_chr3, aes(x = ps, y = -log10(p_wald))) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_vline(xintercept = c(start_pos, end_pos), color = "red", linetype = "dashed") +
  annotate("rect", xmin = start_pos, xmax = end_pos, ymin = -Inf, ymax = Inf,
           alpha = 0.1, fill = "red") +
  labs(title = paste("Zoomed-in Manhattan Plot: chr", chr_of_interest, sep = ""),
       x = paste("Position on Chromosome", chr_of_interest),
       y = expression(-log10)) +

  theme_minimal () 
