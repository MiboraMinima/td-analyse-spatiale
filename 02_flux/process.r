################################################################################
# Script Name: process.r
# Description: Analyse descriptive des mobilités résidentielles en finistère
#              entre 2020 et l'années de résidence prédédent. Données de l'INSEE.
#              Ce script est dédié au chargement, au pré-traitement et au
#              calcul d'indices de flux.
# Author: Antoine Le Doeuff
################################################################################

library(readr)
library(happign)
library(sf)
library(dplyr)
library(stringi)
library(Matrix)

# //////////////////////////////////////////////////////////////////////////////
# Constante --------------------------------------------------------------------
# Où sont enregistré les données (vous, ce sera juste `data`)
DATA_DIR <- "02_flux/data"

# Numéro département
NUM_DEP <- 56

# //////////////////////////////////////////////////////////////////////////////
# Charger les données ----------------------------------------------------------
# Migrations résidentielles ....................................................
dt_mobi <- read_delim(
  glue::glue("{DATA_DIR}/src/base_flux_mobilite_residentielle_2020.csv")
) |> janitor::clean_names()

# Communes .....................................................................
com_path <- "02_flux/data/src/com_56_bdtopov3_2026.gpkg"
if (file.exists(com_path)) {
  cli_alert_info("Le fichier existe déjà, chargement...")
  coms <- read_sf(com_path)
} else {
  cli_alert_info("Téléchargement des données pour le {NUM_DEP}")
  coms <- get_wfs(
    x = NULL,
    layer = "BDTOPO_V3:commune",
    query = glue::glue("code_insee LIKE '{NUM_DEP}%'")
  ) |>
    st_transform(2154)
  cli_alert_info("Écriture...")
  write_sf(coms, com_path)
}

# //////////////////////////////////////////////////////////////////////////////
# Prétraitements ---------------------------------------------------------------
# Filtrer les communes .........................................................
dt_mobi_pb <- filter(
  dt_mobi,
  startsWith(codgeo, as.character(NUM_DEP)),
  startsWith(dcran, as.character(NUM_DEP)),
  codgeo != dcran
)

# Conversion en matrice OD .....................................................
# Récupérer tous les code insee
all_ids <- sort(unique(c(dt_mobi_pb$codgeo, dt_mobi_pb$dcran)))

# Créer des facteurs avec les mêmes niveaux pour chaque O/D
dt_mobi_pb$i <- factor(dt_mobi_pb$dcran, levels = all_ids)
dt_mobi_pb$j <- factor(dt_mobi_pb$codgeo, levels = all_ids)

# Créer la sparse matrix (origin × destination)
M <- sparseMatrix(
  i = as.integer(dt_mobi_pb$i),
  j = as.integer(dt_mobi_pb$j),
  x = dt_mobi_pb$nbflux_c20_pop01p,
  dims = c(length(all_ids), length(all_ids)),
  dimnames = list(all_ids, all_ids)
)
print(dim(M))

# //////////////////////////////////////////////////////////////////////////////
# Analyse des marges -----------------------------------------------------------
# Volume, arrival, departure ...................................................
# Flux total émis
sum_dep <- rowSums(M)

# Flux total reçus
sum_arr <- colSums(M)

# Volumes
volumes <- sum_dep + sum_arr

# Soldes
soldes <- sum_arr - sum_dep

# Ratio d'attractivité
attrac_ratio <- soldes / volumes

# Indices ......................................................................
tot <- sum(M)

# Indice d'attractivité
ia <- sum_arr / tot

# Indice d'emmissivité
ie <- sum_dep / tot

# Relations préférentielles ....................................................
# Flux totaux
tot <- sum(M)

# Calcul du flux attendu
expected <- (sum_dep %o% sum_arr) / tot
# Comparaison des flux observés au flux attendus
m_pref <- M / expected

# Remplacer la diagonale avec 0
diag(m_pref) <- 0
m_pref[!is.finite(m_pref)] <- 0

# //////////////////////////////////////////////////////////////////////////////
# Mise en forme de la donnée ---------------------------------------------------
# Créez un dataframe avec les indices
indices <- data.frame(
  code_insee = names(sum_dep),
  departure = sum_dep,
  arrival = sum_arr,
  volumes = volumes,
  soldes = soldes,
  attrac_ratio = attrac_ratio,
  ia = ia,
  ie = ie
)

# Ajouter la géométries des communes
indices_spa <- left_join(select(coms, code_insee), indices, by = "code_insee")

# //////////////////////////////////////////////////////////////////////////////
# Statistiques -----------------------------------------------------------------
# On calcul les statiques élémentaires des indices
indices_long <-
  tidyr::pivot_longer(
    indices,
    cols = !code_insee,
    names_to = "indices",
    values_to = "value"
  )

stat <- group_by(indices_long, indices) |>
  summarise(
    mean = mean(value),
    median = median(value),
    sd = sd(value),
    min = min(value),
    max = max(value)
  )

# //////////////////////////////////////////////////////////////////////////////
# Écriture des données ---------------------------------------------------------
# Le tableau des mobilités résidentielles filtées
write_csv(dt_mobi_pb, glue::glue("{DATA_DIR}/res/mobi_res_{NUM_DEP}.csv"))

# Les indices avec la géométrie
write_sf(indices_spa, glue::glue("{DATA_DIR}/res/indices_spa_{NUM_DEP}.gpkg"))

# La matrice des relations préférentielles
# Le format RDS permet de sauvegarder n'importe quel objet R qui est dans
# l'environnement. C'est un format exclusif à R.
saveRDS(m_pref, glue::glue("{DATA_DIR}/res/rel_pref_{NUM_DEP}.rds"))
