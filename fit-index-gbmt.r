calculate_OCC <- function(model) {
  num_groups <- length(model$appa)

  # Si el modelo tiene solo un grupo, devolver 999.0
  if (num_groups == 1) {
    return(Inf)
  }

  # Encontrar el grupo asignado para cada observación según la máxima probabilidad posterior
  assigned_groups <- apply(model$posterior, 1, which.max)

  # Calcular la proporción de población estimada para cada grupo
  population_proportion <- table(assigned_groups) / length(assigned_groups)

  # Calcular el OCC para cada grupo
  OCC <- sapply(1:num_groups, function(j) {
    # Si APPA es igual a 1, devolver 999.0
    if (model$appa[j] == 1) {
      return(999.0)
    }

    odds_correct <- model$appa[j] / (1 - model$appa[j])
    odds_random <- population_proportion[j] / (1 - population_proportion[j])
    return(odds_correct / odds_random)
  })

  # Devolver el promedio de OCC
  return(mean(OCC))
}

calculate_mismatch <- function(model) {
  # Calcular las proporciones de cada grupo estimadas por el modelo
  group_proportions <- model$prior

  # Calcular las proporciones reales de cada grupo en la muestra
  real_proportions <- table(factor(model$assign, levels = 1:length(group_proportions))) / length(model$assign)

  # Calcular el Mismatch para cada grupo
  mismatch <- group_proportions - real_proportions

  # Devolver el promedio de los valores absolutos de Mismatch de todos los grupos
  return(mean(abs(mismatch)))
}

calculate_SD_GMP <- function(model) {
  # Inicializar un vector para almacenar el SD-GMP para cada grupo
  sd_gmp <- numeric(length(model$prior))

  # Calcular el SD-GMP para cada grupo
  for (j in 1:length(model$prior)) {
    assigned_indices <- which(model$assign == j)
    group_probs <- model$posterior[assigned_indices, j]
    sd_gmp[j] <- sd(group_probs)
  }

  # Devolver la media de los SD-GMP de todos los grupos
  return(mean(sd_gmp))
}