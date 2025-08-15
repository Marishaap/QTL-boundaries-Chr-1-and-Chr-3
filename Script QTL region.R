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