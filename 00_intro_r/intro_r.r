##############################################################
# SIMULATION D’UNE PARTIE DE ROULETTE FRANÇAISE
# Avec les joueurs : Mozart, Bach, Wagner
# Objectif : Montrer une simulation simple avec mise à jour des gains
##############################################################

# Initialisation des joueurs
gaziers <- list(
  "Mozart" = list("argent" = 20),
  "Bach"   = list("argent" = 20),
  "Wagner" = list("argent" = 20)
)

##############################################################
# PARAMÈTRES DE LA PARTIE
##############################################################

nb_tours <- 10 # nombre de tours à jouer
mise_min <- 1  # mise minimale
mise_max <- 10 # mise maximale
set.seed(42)   # pour des résultats reproductibles

##############################################################
# FONCTION : simulation d’un tour de roulette
##############################################################
# - Chaque joueur mise (s’il a encore de l’argent)
# - On tire un numéro entre 0 et 36
# - On regarde pair/impair :
#     - si le joueur a parié sur le bon type, il double sa mise
#     - sinon, il perd sa mise
##############################################################
mise_pari <- function(argent, mise_min, mise_max, nom) {
    # Sauter le joueur s’il n’a plus d’argent
    if (argent < mise_min) {
        cat(nom, "n’a plus d’argent et ne peut pas jouer.\n")
        return(NULL)
    } else {
        # Le joueur choisit un pari et une mise
        pari <- sample(c("pair", "impair"), 1)
        mise <- min(sample(mise_min:mise_max, 1), argent)

        # On print le pari et la mise pour le joueur courant
        cat(nom, "parie", mise, "€ sur", pari, "\n")
    }
    return(list(pari = pari, mise = mise))
}

roulette_tour <- function(gaziers) {
  # Tirage du numéro gagnant (0 à 36)
  cat("\n🎡 Faites vos jeux !\n")

  # Chaque joueur joue
  pari_mise <- list()
  for (nom in names(gaziers)) {
    argent <- gaziers[[nom]][["argent"]]

    # Effectuer la mise et le parie
    mise_pari_vec <- mise_pari(argent, MISE_MIN, MISE_MAX, nom)

    # Sauter le joueur s’il n’a plus d’argent
    if (is.null(mise_pari_vec)) next

    pari_mise[[nom]][["pari"]] <- mise_pari_vec[["pari"]]
    pari_mise[[nom]][["mise"]] <- mise_pari_vec[["mise"]]
  }

  # Tirage du numéro
  cat("\n🎡 Rien ne vas plus !")

  numero <- sample(0:36, 1)
  cat("\n🎡 Le numéro tiré est :", numero, "\n")

  # Déterminer la couleur du résultat
  if (numero == 0) {
    resultat <- "zero"
  } else if (numero %% 2 == 0) {
    resultat <- "pair"
  } else {
    resultat <- "impair"
  }

  # Résultat du tour
  for (nom in names(pari_mise)) {
    pari <- pari_mise[[nom]][["pari"]]
    mise <- pari_mise[[nom]][["mise"]]
    if (resultat == pari) {
      gain <- mise
      gaziers[[nom]]$argent <- argent + gain
      cat("✅", nom, "gagne", gain, "€ ! Nouveau solde :", gaziers[[nom]]$argent, "€\n")
    } else if (resultat == "zero") {
      # si le zéro sort, tout le monde perd sa mise
      gaziers[[nom]]$argent <- argent - mise
      cat("💀 Zéro sorti !", nom, "perd", mise, "€ (reste :", gaziers[[nom]]$argent, "€)\n")
    } else {
      gaziers[[nom]]$argent <- argent - mise
      cat("❌", nom, "perd", mise, "€ (reste :", gaziers[[nom]]$argent, "€)\n")
    }
  }

  return(gaziers)
}

##############################################################
# SIMULATION COMPLÈTE
##############################################################

for (tour in 1:nb_tours) {
  cat("\n================== TOUR", tour, "==================\n")
  gaziers <- roulette_tour(gaziers)
}

##############################################################
# RÉSULTATS FINAUX
##############################################################
cat("\n💰 Résultats finaux :\n")
for (nom in names(gaziers)) {
  cat(nom, ":", gaziers[[nom]]$argent, "€\n")
}

##############################################################
# FIN DE LA SIMULATION
##############################################################
