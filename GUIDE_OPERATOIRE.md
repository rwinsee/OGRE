# Guide operatoire utilisateur

## But de l'application
OGRE sert a construire, relire et publier des familles ROME pour EDEP.

Une famille repose sur un principe simple :

- un `parent`
- un ou plusieurs `enfants`

L'application permet de faire vivre cette famille dans un workflow en 3 etapes :

1. `Edition`
2. `Supervision`
3. `Validation`

Le but utilisateur n'est pas de manipuler des fichiers, mais de traiter des propositions de familles de maniere tracee et partagee.

## Avant de commencer
Pour travailler dans de bonnes conditions :

- verifier que le referentiel OGR est bien charge
- verifier son `IDEP`
- utiliser le bouton `Rafraichir les files` si plusieurs utilisateurs travaillent en meme temps

## Ecran d'accueil
L'ecran d'accueil propose 3 espaces :

- `Edition`
- `Supervision`
- `Validation`

Chaque espace correspond a un role ou a une etape du workflow.

## Partie 1 - Edition

### Objectif
L'edition sert a preparer une proposition de famille.

### Ce que fait l'utilisateur

1. filtrer le referentiel
2. selectionner les lignes utiles
3. choisir un parent
4. verifier les enfants proposes
5. ajuster les enfants si besoin
6. simuler la filiation
7. creer la proposition

### Ce que fait l'application

- propose une aide au choix du parent
- empeche ou signale certains doublons
- alerte si un enfant est deja rattache ailleurs
- cree une proposition tracee avec `IDEP` et horodatage
- depose le fichier dans la file de supervision

### Point d'attention
Creer une proposition en edition ne publie rien dans le stock.

## Partie 2 - Supervision

### Objectif
La supervision relit et arbitre les propositions avant envoi a la MOA.

### Ce que voit le superviseur

- une file `A superviser`
- une file `En cours`
- une file `Rejetes`
- un detail de proposition
- une zone d'ajustement parent / enfants
- des KPI de suivi de file

### Actions possibles

1. `Prendre en charge`
2. `Envoyer en validation MOA` en l'etat
3. `Modifier puis envoyer en validation MOA`
4. `Rejeter`
5. `Creer une reprise d'edition` depuis un rejet

### Ce que fait l'application

- deplace le fichier dans la bonne file
- conserve la trace du superviseur
- enregistre la decision de supervision
- conserve les deltas si la supervision modifie la famille
- permet de repartir d'un rejet sans ecraser l'historique

## Partie 3 - Validation

### Objectif
La validation MOA est l'etape qui tranche et publie.

### Ce que voit le valideur
Le tableau de bord validation est structure en 3 blocs :

- `Couverture referentiel`
- `Vue supervision`
- `Vue validation MOA`

Le valideur dispose aussi de plusieurs files :

- `A valider`
- `En cours`
- `En attente`
- `Rejetes`
- `Validees`
- `Stock familles`
- `Referentiel EDEP`

### Actions possibles

1. `Prendre en charge`
2. `Mettre en attente`
3. `Reprendre la validation`
4. `Rejeter`
5. `Publier dans le stock`

### Ce que fait l'application

- enregistre l'IDEP validation et les horodatages
- garde la decision MOA
- publie une nouvelle famille ou remplace une famille existante
- alimente le referentiel maison EDEP
- deplace la proposition dans `valides` ou `rejetes`

### Effet d'une publication
Quand la MOA publie :

- la famille entre dans le stock
- le lien parent -> enfants entre dans le referentiel EDEP
- la proposition reste tracable

## Comment lire les ecrans

### Referentiel
Le referentiel sert a rechercher des codes OGR et a selectionner les bons libelles.

### Stock familles
Le stock correspond aux familles deja publiees.

### Referentiel EDEP
Le referentiel EDEP montre la version maison publiee des liens parent / enfant.

### Propositions
Une proposition est un fichier de travail en cours de vie dans le workflow.

## Cas d'usage les plus frequents

### Creer une nouvelle famille

1. composer la famille en edition
2. creer la proposition
3. faire relire en supervision
4. faire valider en MOA
5. publier

### Modifier une famille existante

1. recomposer la famille en edition
2. creer une proposition de modification
3. faire arbitrer en supervision
4. faire publier en validation

### Rejeter puis reprendre

1. rejeter en supervision ou validation
2. conserver le fichier rejete pour consultation
3. recreer une reprise
4. relancer un nouveau cycle de traitement

## Tracabilite
Chaque etape conserve des informations de suivi :

- l'agent qui agit
- la date et l'heure
- la decision prise
- le contenu parent / enfants a l'etape concernee
- la reference eventuelle a un fichier source

## Bonnes pratiques utilisateur

- verifier le parent avant de creer une proposition
- verifier les enfants deja rattaches ailleurs
- utiliser les simulations avant validation
- documenter un rejet ou une mise en attente dans le commentaire
- rafraichir les files en cas de travail a plusieurs

## Pour aller plus loin
Pour comprendre la logique de passage d'une etape a l'autre, voir aussi :

- [Logigramme du workflow](LOGIGRAMME_WORKFLOW_EDITION_VALIDATION.md)
