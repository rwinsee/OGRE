# Guide operatoire utilisateur

## But de l'application
OGRE sert a construire, relire et publier des familles ROME pour EDEP.

Une famille repose sur un principe simple :

- un `parent`
- un ou plusieurs `enfants`

L'application fait vivre cette famille dans un workflow en 3 etapes :

1. `Edition`
2. `Supervision`
3. `Validation`

Le but utilisateur n'est pas de manipuler des fichiers, mais de traiter des propositions de familles de maniere tracee et partagee.

## Avant de commencer
Pour travailler dans de bonnes conditions :

- verifier que le referentiel OGR est bien charge
- verifier son `IDEP`
- utiliser le bouton `Rafraichir les files` si plusieurs utilisateurs travaillent en meme temps
- garder en tete que le stock reste vide tant qu'aucune publication MOA n'a eu lieu

## Reperes d'interface
Quelques aides sont maintenant presentes dans l'application :

- les petits `i` donnent une aide courte au survol ou au focus clavier
- certains blocs sont regroupes en `vantaux` internes via des onglets
- les tableaux de supervision et de validation sont compacts par defaut
- le bouton `Choisir les colonnes` permet d'afficher plus ou moins de champs selon le besoin

## Ecran d'accueil
L'ecran d'accueil propose 3 espaces :

- `Edition`
- `Supervision`
- `Validation`

Chaque espace correspond a un role ou a une etape du workflow.

## Partie 1 - Edition

### Objectif
L'edition sert a preparer une proposition de famille avant relecture.

### Ce que voit l'utilisateur
L'ecran Edition est organise autour de :

- une carte `Composer une famille`
- une carte `Analyse de la proposition` avec les onglets `Resume` et `Preconisations`
- un onglet `Selection`
- un onglet `Stock existant`
- un onglet `Propositions`
- un onglet `Detail stock`

### Parcours standard
Le parcours nominal est le suivant :

1. ouvrir l'onglet `Selection`
2. filtrer le referentiel et selectionner les lignes utiles
3. renseigner l'`IDEP agent`
4. choisir un `Parent`
5. choisir les `Enfants`
6. utiliser `Verifier la filiation`
7. relire `Analyse de la proposition`, cote `Resume` puis cote `Preconisations`
8. cliquer sur `Creer la proposition`

### Ce que fait l'application
L'application :

- aide au choix du parent
- affiche des preconisations avant creation
- signale certains doublons ou recouvrements
- cree une proposition tracee avec `IDEP` et horodatage
- depose le fichier dans `supervision / a_superviser`

### Points d'attention

- `Verifier la filiation` controle sans enregistrer
- creer une proposition en edition ne publie rien dans le stock
- l'onglet `Stock existant` peut etre vide, ce qui est normal tant qu'aucune famille n'a ete publiee
- l'onglet `Propositions` permet de relire ou de supprimer une proposition locale
- l'onglet `Stock existant` permet aussi de supprimer une famille deja stockee si l'usage local le permet

## Partie 2 - Supervision

### Objectif
La supervision relit et arbitre les propositions avant envoi a la MOA.

### Ce que voit le superviseur
L'ecran Supervision comprend :

- une zone `Prise en charge`
- une zone `Ajustement supervision`
- des KPI de suivi de file
- une file `A superviser`
- une file `En cours`
- une file `Rejetes`
- un bloc `Detail de la proposition`

### Actions selon l'etat
Les actions ouvertes changent selon la file :

1. `A superviser` : `Prendre en charge`
2. `En cours` : `Envoyer en validation MOA`, `Modifier puis envoyer en validation MOA`, `Rejeter`
3. `Rejetes` : `Creer une reprise d'edition`

### Ce que fait l'application
L'application :

- deplace le fichier dans la bonne file
- conserve la trace du superviseur et des horodatages
- enregistre la decision de supervision
- conserve les deltas si la supervision modifie la famille
- cree une nouvelle reprise d'edition liee au rejet si le superviseur relance le dossier

### Points d'attention

- les tableaux sont compacts par defaut
- le bouton `Choisir les colonnes` permet de reafficher les champs supervision
- une reprise d'edition depuis `Rejetes` cree un nouveau dossier remis dans `supervision / a_superviser`
- le rejet supervision reste consultable meme apres reprise

## Partie 3 - Validation

### Objectif
La validation MOA est l'etape qui tranche et publie.

### Ce que voit le valideur
L'ecran Validation est structure en plusieurs ensembles :

- un bloc KPI a onglets : `Couverture`, `Supervision`, `Validation MOA`
- un bloc a onglets : `Decision MOA`, `Impact de publication`
- une file `A valider`
- une file `En cours`
- une file `En attente`
- une file `Rejetes`
- une file `Validees`
- un onglet `Stock familles`
- un onglet `Referentiel EDEP`
- un bloc `Detail validation`

### Actions selon l'etat
Les actions ouvertes changent selon la file :

1. `A valider` : `Prendre en charge`
2. `En cours` : `Mettre en attente`, `Publier dans le stock`, `Rejeter`
3. `En attente` : `Reprendre la validation`, `Publier dans le stock`, `Rejeter`
4. `Rejetes` : consultation
5. `Validees` : consultation

### Ce que fait l'application
L'application :

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

### Points d'attention

- `Impact de publication` aide a voir ce qui sera cree ou remplace avant publication
- les tableaux sont compacts par defaut
- le bouton `Choisir les colonnes` permet de reafficher les champs de validation
- un dossier rejete en validation reste consultable, mais l'ecran Validation n'ouvre pas de reprise directe
- `Stock familles` et `Referentiel EDEP` montrent l'etat publie, pas les dossiers encore en cours

## Comment lire les principaux ecrans

### Selection
`Selection` sert a filtrer le referentiel OGR et a retenir les lignes qui vont entrer dans la famille.

### Analyse de la proposition
`Resume` confirme ce qui sera envoye.

`Preconisations` aide a reperer les regroupements a confirmer ou a retravailler.

### Stock existant et Stock familles
Ces vues correspondent au stock deja publie.

Si elles sont vides, cela signifie qu'aucune famille n'a encore ete publiee dans l'environnement courant.

### Propositions
Une proposition est un fichier de travail encore engage dans le workflow ou conserve pour trace.

### Detail stock
`Detail stock` permet de comparer une idee de famille avec une famille deja publiee.

### Referentiel EDEP
Le `Referentiel EDEP` montre la version aval parent -> enfant produite par les publications MOA.

## Cas d'usage les plus frequents

### Creer une nouvelle famille

1. composer la famille en edition
2. verifier la filiation
3. creer la proposition
4. faire relire en supervision
5. faire valider en MOA
6. publier

### Modifier une famille existante

1. consulter `Stock existant`
2. recomposer la famille en edition
3. creer une proposition de modification
4. faire arbitrer en supervision
5. faire publier en validation

### Rejeter en supervision puis relancer

1. rejeter en supervision
2. consulter le dossier dans `Rejetes`
3. utiliser `Creer une reprise d'edition`
4. laisser repartir le nouveau dossier dans `A superviser`

### Rejeter en validation

1. rejeter en MOA
2. consulter le dossier dans `Rejetes`
3. repartir d'un nouveau travail si une nouvelle proposition doit etre preparee

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
- lire les `Preconisations` avant creation
- documenter un rejet ou une mise en attente dans le commentaire
- utiliser `Choisir les colonnes` quand un tableau parait trop pauvre ou trop charge
- rafraichir les files en cas de travail a plusieurs

## Pour aller plus loin
Pour comprendre la logique de passage d'une etape a l'autre, voir aussi :

- [Logigramme du workflow](LOGIGRAMME_WORKFLOW_EDITION_VALIDATION.md)
