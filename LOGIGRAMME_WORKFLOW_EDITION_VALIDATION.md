# Logigramme workflow edition -> validation

## Schema image compatible RStudio

![Schema du workflow OGRE](LOGIGRAMME_WORKFLOW_EDITION_VALIDATION.svg)

Note : le bloc Mermaid ci-dessous reste utile sur les viewers qui le rendent, mais RStudio peut ne pas l'afficher. Une version texte compatible RStudio est conservee plus bas.

```mermaid
flowchart TD
    A["Edition<br/>Selectionner des lignes<br/>Choisir 1 parent + des enfants"] --> B["Verifier la filiation<br/>Lire Resume / Preconisations"]
    B --> C["Creer la proposition"]
    C -. "Le stock ne change pas encore" .-> C1["Pas de publication a ce stade"]
    C --> D["Depot dans<br/>supervision / a_superviser"]

    D --> E["Supervision<br/>Prendre en charge"]
    E --> F{"Decision supervision"}

    F -- "Envoyer en l'etat" --> G["validation / a_valider"]
    F -- "Modifier puis envoyer" --> G
    F -- "Rejeter" --> H["supervision / rejetes"]

    H --> I{"Relancer le dossier ?"}
    I -- "Oui" --> J["Creer une reprise d'edition<br/>nouveau dossier lie au rejet"]
    J --> D
    I -- "Non" --> H

    G --> K["Validation MOA<br/>Prendre en charge"]
    K --> L{"Decision MOA"}

    L -- "Mettre en attente" --> M["validation / en_attente"]
    M --> N{"Reprendre le dossier ?"}
    N -- "Oui" --> K
    N -- "Non" --> M

    L -- "Rejeter" --> O["validation / rejetes<br/>consultation"]

    L -- "Publier dans le stock" --> P["validation / valides"]
    P --> Q["Creation ou remplacement<br/>de la famille dans le stock"]
    Q --> R["Mise a jour du referentiel EDEP<br/>parent -> enfants"]
```

## Version texte compatible RStudio

```text
Edition
  -> Selectionner des lignes
  -> Choisir 1 parent + des enfants
  -> Verifier la filiation
  -> Lire Resume / Preconisations
  -> Creer la proposition
  -> Depot dans supervision / a_superviser

Supervision
  -> Prendre en charge
  -> Decision supervision :
     - Envoyer en l'etat -> validation / a_valider
     - Modifier puis envoyer -> validation / a_valider
     - Rejeter -> supervision / rejetes

Supervision / rejetes
  -> Relancer le dossier ?
     - Oui -> creer une reprise d'edition -> retour dans supervision / a_superviser
     - Non -> rester en rejet

Validation MOA
  -> Prendre en charge
  -> Decision MOA :
     - Mettre en attente -> validation / en_attente
     - Reprendre plus tard -> retour en validation / en_cours
     - Rejeter -> validation / rejetes
     - Publier dans le stock -> validation / valides

Publication
  -> Creation ou remplacement de la famille dans le stock
  -> Mise a jour du referentiel EDEP parent -> enfants
```
