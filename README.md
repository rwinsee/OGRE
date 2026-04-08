# OGRE - Outil de Gestion du ROME pour EDEP

## Objet
OGRE est une application Shiny de gestion de familles ROME pour EDEP.

Son but est de permettre a plusieurs acteurs de travailler sur une meme proposition de famille metier, en separant clairement :

- l'edition de la proposition
- la supervision de la proposition
- la validation MOA
- la publication dans le stock
- l'alimentation du referentiel maison EDEP

L'application met l'accent sur la tracabilite :

- qui a fait l'action
- a quelle date et heure
- a partir de quelle proposition source
- dans quel etat de workflow se trouve le fichier

## Parcours fonctionnels
L'application est organisee autour de 3 espaces de travail :

- `Edition`
- `Supervision`
- `Validation`

Le cycle cible est le suivant :

1. un editeur compose une famille et cree une proposition
2. la proposition entre en supervision
3. le superviseur prend en charge, valide, modifie ou rejette
4. la MOA prend en charge, met en attente, rejette ou publie
5. la publication alimente le stock et le referentiel EDEP

## Documents disponibles

- [Guide operatoire](GUIDE_OPERATOIRE.md)
- [Logigramme du workflow](LOGIGRAMME_WORKFLOW_EDITION_VALIDATION.md)

## Lancer l'application

Depuis RStudio :

1. ouvrir le projet `Test_family_apet.Rproj`
2. ouvrir `app.R`
3. lancer l'application avec `Run App`

Depuis une console R :

```r
shiny::runApp()
```

## Reperes de stockage

- `data/referentiel/ref_ogr.csv` : referentiel OGR source
- `data/referentiel/cr_gd_dp_v4_utf8.csv` : enrichissement ROME / domaine
- `data/referentiel/referentiel_familles_edep.csv` : referentiel maison publie
- `data/workflow/supervision/...` : files de supervision
- `data/workflow/validation/...` : files de validation
- `data/familles/stock` : familles publiees

## Contenu versionne dans GitHub
Le depot GitHub embarque le code source et la documentation.

Les donnees de travail generees par l'application ne sont pas poussees :

- files de workflow
- stock courant
- historiques de travail
- fichiers temporaires locaux

Au demarrage, l'application recree automatiquement l'arborescence utile si elle n'existe pas.

## Logique generale

- une proposition creee en edition n'est pas publiee directement
- la supervision travaille sur le fichier avant envoi a la MOA
- la validation MOA est la seule etape qui publie dans le stock
- un rejet reste visible et peut servir de base a une reprise

## Etat actuel
Le projet est maintenant structure en modules :

- `edition/`
- `supervision/`
- `validation/`

Chaque espace dispose de son `ui`, de son `server` et de son `global`.
