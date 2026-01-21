#
# library(cgam)
#
# # Sample data
# # sample_data <- readRDS("./src/sample_data.rds") %>%
# #   arrange(antibody_mfi)
#
# sample_data <- readRDS("./src/sample_data_error.rds") %>%
#   arrange(antibody_mfi)
#
# # Create new columns in the sample variable
# sample_data$antibody_au <- NA
# sample_data$antibody_au_se <- NA
# sample_data$reference_dilution <- NA
# sample_data$gate_class_dil <- NA
#
#
# all_dilutions <- unique(sample_data$dilution)
# reference_dilution <- "150"
# sample_data$reference_dilution <- reference_dilution
#
# remaining_dilutions <- setdiff(all_dilutions, reference_dilution)
#
# for(dilution_pair in remaining_dilutions){
#
#   print(dilution_pair)
#
#   # Filter Between_limits points
#   df <- sample_data %>%
#     filter(dilution %in% c(reference_dilution, dilution_pair)) %>%
#     group_by(patientid) %>%
#     # Filter all patients where gate class is between limits
#     filter(all(gate_class == "Between_Limits")) %>%
#     ungroup()
#
#   # Filter Between_limit points for reference dilution
#   df %>%
#     filter(dilution == reference_dilution) -> in_between_reference
#
#   # Filter Between_limit points for corresponding pair
#   df %>%
#     filter(dilution == dilution_pair) -> in_between_dilution_pair
#
#   # Filter all points for reference variable
#   sample_data %>%
#     filter(dilution == reference_dilution) -> sampdat_reference
#
#   # Filter all points for dilution pair
#   sample_data %>%
#     filter(dilution == dilution_pair) -> sampdat_dilution_pair
#
#   # Setting up x and y
#   y <- in_between_reference$antibody_mfi
#   summary(y)[c(1,6)]
#   x <- in_between_dilution_pair$antibody_mfi
#   summary(x)[c(1,6)]
#   x_new <- sampdat_dilution_pair[sampdat_dilution_pair$antibody_mfi>summary(x)[1] & sampdat_dilution_pair$antibody_mfi<=summary(x)[6], c("antibody_mfi")]
#
#   raw_dat <<- data.frame(x,y)
#
#   # n <- ggplot(raw_dat, aes(x = x, y = y)) +
#   #   geom_point()
#   # print(n)
#
#   new <- data.frame(x = x_new)
#
#   mod_fit <- cgam(y ~ s.incr(x,
#                              numknots = 3,
#                              knots = 0,
#                              var.knots = 0,
#                              space = "Q",
#                              db.exp = TRUE),
#                   data=raw_dat,
#                   cic = FALSE,
#                   nsim = 100)
#   fit_dat <- data.frame(raw_dat)
#   fit_pred <- predict(mod_fit, fit_dat, interval = "confidence", level = 0.95)
#   fit_dat$fit <- fit_pred$fit
#   fit_dat$lcl <- fit_pred$lower
#   fit_dat$ucl <- fit_pred$upper
#   predicted <- predict(mod_fit, new, interval = "confidence", level = 0.95)
#   pred_dat <- new
#   pred_dat$fit <- predicted$fit
#   pred_dat$lcl <- predicted$lower
#   pred_dat$ucl <- predicted$upper
#
#
#   q <- ggplot(fit_dat, aes(x = x, y = y)) +
#     geom_point() +
#     # geom_ribbon(aes(ymin = lcl, ymax = ucl),
#     #             data = pred_dat, fill = "green",
#     #             color = "black", linetype = "dotted")+
#     geom_line(aes(y = fit), data = pred_dat)
#   print(q)
#
#
#   r <- ggplot(fit_dat, aes(x = y, y = fit)) +
#     geom_point() +
#     stat_smooth(method = "lm",
#                 formula = y ~ x,
#                 geom = "smooth")
#   print(r)
#
#   y_pred <-  predict(mod_fit, new, interval = "confidence")
#
#   df_pred <- data.frame(dilution = dilution_pair,
#                     patientid = sampdat_dilution_pair[sampdat_dilution_pair$antibody_mfi>summary(x)[1] & sampdat_dilution_pair$antibody_mfi<=summary(x)[6], ]$patientid,
#                     antibody_au_dilution_pair = y_pred$fit,
#                     antibody_au_se_dilution_pair = paste0(y_pred$lower,"|",y_pred$upper)
#                     )
#
#   # Join the new dataframe with predicted values to the original
#   sample_data <- merge(sample_data,df_pred, by = c("dilution", "patientid"), all.x = TRUE)
#   sample_data$antibody_au <- ifelse(sample_data$dilution==reference_dilution, sample_data$antibody_mfi, sample_data$antibody_au_dilution_pair)
#   sample_data$antibody_au_se <- sample_data$antibody_au_se_dilution_pair
#   sample_data <- sample_data[ , c("dilution","patientid", "antibody_mfi", "gate_class", "antibody_au", "antibody_au_se", "reference_dilution","gate_class_dil", "x_min", "x_max")]
# }
#
# # Example code for plotting difference of original plot vs standardized
# sample_data %>%
#   mutate(dilution_class = as.numeric(dilution)) %>%
#   arrange(dilution_class) %>%
#   mutate(dilution_class = dilution_class %>% as.factor() %>% fct_inorder()) -> sample_data
#
# sample_data %>%
#   pivot_longer(cols = c("antibody_mfi", "antibody_au"),
#                names_to = "type",
#                values_to = "mfi") %>%
#   arrange(desc(type)) %>%
#   mutate(type = type %>% as.factor() %>% fct_inorder())-> tsample_data
#
#
# p <- ggplot(tsample_data, aes(x = dilution_class, y = mfi, color = gate_class)) +
#   geom_point() +
#   geom_line(aes(group = patientid), show.legend = FALSE) +
#   theme_minimal() +
#   labs(x = "Dilution", y = "MFI") +
#   theme(legend.position = "bottom") +
#   facet_wrap(~type)
# print(p)
#
