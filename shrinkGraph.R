
campioni <- modello3

n_iter <- length(campioni$rho)
n_grp <- ncol(campioni$phi)

# Trasformare i parametri
s <- scaling_factor2
rho <- campioni$rho
phi_tr <- matrix(NA, nrow = n_iter, ncol = n_grp)
theta_tr <- matrix(NA, nrow = n_iter, ncol = n_grp)

for (i in 1:n_iter) {
  phi_tr[i, ] <- sqrt(rho[i] / s) * campioni$phi[i, ]
  theta_tr[i, ] <- sqrt(1 - rho[i]) * campioni$theta[i, ]
}

nomi_gruppi <- paste("Gruppo", 1:n_grp)

df_phi <- data.frame(
  valore = as.vector(phi_tr),
  parametro = rep(nomi_gruppi, each = n_iter),
  tipo = 'Effetto Spaziale'
)

df_theta <- data.frame(
  valore = as.vector(theta_tr),
  parametro = rep(nomi_gruppi, each = n_iter),
  tipo = 'Effetto Non Strutturato'
)

df_combinato <- rbind(df_phi, df_theta)

media_phi <- aggregate(valore ~ parametro, data = df_phi, mean)
media_theta <- aggregate(valore ~ parametro, data = df_theta, mean)

media <- rbind(
  transform(media_phi, tipo = 'Effetto Spaziale'),
  transform(media_theta, tipo = 'Effetto Non Strutturato')
)

library(ggplot2)
ggplot(df_combinato, aes(x = parametro, y = valore, fill = tipo)) +
  geom_boxplot() +
  geom_point(data = media, aes(x = parametro, y = valore, color = tipo), size = 2) +
  facet_wrap(~tipo, scales = "free") +
  labs(title = ,
       x = "",
       y = "Valore Effetto") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))









# Calculate posterior means for parameters
beta0_mean <- mean(posterior_samples$beta0)
betas_mean <- apply(posterior_samples$betas, 2, mean)
logit_rho_mean <- mean(posterior_samples$logit_rho)
phi_mean <- apply(posterior_samples$phi, 2, mean)
theta_mean <- apply(posterior_samples$theta, 2, mean)
sigma_mean <- mean(posterior_samples$sigma)
rho_mean <- mean(posterior_samples$rho)


shrinkage_phi <- sqrt(rho_mean / sigma_mean) * phi_mean
shrinkage_theta <- sqrt(1 - rho_mean) * theta_mean



# Create a dataframe for easier comparison
shrinkage_df <- data.frame(
  Parameter = rep(c("phi", "theta"), each = length(phi_mean)),
  Effect = c(shrinkage_phi, shrinkage_theta)
)

# Plot the shrinkage effects for visualization
library(ggplot2)

ggplot(shrinkage_df, aes(x = Parameter, y = Effect), color = "blue") +
  geom_boxplot() +
  labs(title = "",
       x = "Effetto",
       y = "Valore Effetto") +
  theme_minimal()

# Summary of shrinkage effects
summary(shrinkage_df)
