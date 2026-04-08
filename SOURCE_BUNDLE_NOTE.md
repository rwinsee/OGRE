# Source Bundle

Le depot contient une archive `ogre_source_bundle.zip` qui embarque l'ensemble du code local de l'application tel qu'il existait au moment de la publication.

Pourquoi cette archive existe : l'environnement courant ne dispose pas de `git` local, et le connecteur GitHub disponible ici est mal adapte a l'injection directe de certains tres gros fichiers R en texte brut.

Si vous voulez reconstituer exactement le projet local :

1. Telechargez `ogre_source_bundle.zip` depuis le repo.
2. Decompressez son contenu a la racine du projet.
3. Lancez ensuite l'application depuis `app.R`.

Les fichiers Markdown (`README.md`, `GUIDE_OPERATOIRE.md`, `LOGIGRAMME_WORKFLOW_EDITION_VALIDATION.md`) et plusieurs modules UI ont ete publies directement dans le depot.
