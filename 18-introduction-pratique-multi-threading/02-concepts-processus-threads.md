🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 18.2 Concepts de processus et threads

## Introduction

Avant de plonger dans le code, nous devons comprendre quelques concepts fondamentaux. Ne vous inquiétez pas, nous allons rester très concrets et utiliser des analogies simples. Cette section est courte mais essentielle pour bien comprendre la suite.

## Qu'est-ce qu'un processus ?

### Définition simple

Un **processus** est un programme en cours d'exécution. Quand vous double-cliquez sur l'icône de votre application Lazarus compilée, le système d'exploitation crée un processus.

### Caractéristiques d'un processus

Chaque processus possède :

- **Son propre espace mémoire** : Les variables d'un processus sont totalement isolées des autres processus
- **Ses propres ressources** : Fichiers ouverts, connexions réseau, fenêtres graphiques
- **Un identifiant unique** : Le PID (Process ID) sous Windows et Linux
- **Au moins un thread** : Le thread principal qui exécute le code

### Analogie : l'entreprise

Imaginez un processus comme une **entreprise** :
- L'entreprise a ses propres locaux (espace mémoire)
- Elle a ses propres employés (threads)
- Elle a son matériel et équipements (ressources)
- Elle est identifiée par son numéro SIRET (PID)

Deux entreprises différentes (deux processus) ne peuvent pas facilement partager leurs ressources. Si l'entreprise A fait faillite, cela n'affecte pas directement l'entreprise B.

### Visualisation dans le système

Vous pouvez voir les processus actifs sur votre système :

**Sous Windows :**
- Ouvrez le Gestionnaire des tâches (Ctrl+Shift+Échap)
- Onglet "Processus" : Chaque ligne est un processus

**Sous Linux/Ubuntu :**
- Ouvrez le Moniteur système
- Ou tapez `ps aux` dans un terminal

Vous verrez par exemple :
- `firefox` (le navigateur)
- `lazarus` (l'EDI)
- `vlc` (le lecteur vidéo)
- `monappli` (votre application)

Chacun est un **processus séparé**.

## Qu'est-ce qu'un thread ?

### Définition simple

Un **thread** (fil d'exécution) est une unité d'exécution à l'intérieur d'un processus. C'est comme un "mini-programme" qui s'exécute dans le cadre du processus parent.

Un processus peut avoir **plusieurs threads** qui s'exécutent en parallèle, partageant les mêmes ressources.

### Caractéristiques d'un thread

Chaque thread possède :

- **Sa propre pile d'exécution** : Ses propres variables locales
- **Son propre compteur de programme** : Il sait où il en est dans le code
- **Accès à la mémoire partagée** : Tous les threads d'un processus voient les mêmes variables globales

### Analogie : les employés de l'entreprise

Reprenons notre analogie de l'entreprise :

- L'**entreprise** = le processus
- Les **employés** = les threads

Dans une entreprise :
- Chaque employé (thread) a sa propre tâche à accomplir
- Ils travaillent en parallèle
- Ils partagent les mêmes locaux (mémoire)
- Ils ont accès aux mêmes dossiers (variables globales)
- Ils peuvent communiquer entre eux

Si un employé (thread) fait une erreur grave, toute l'entreprise (processus) peut en pâtir !

### Le thread principal

Tout processus commence avec **un seul thread** : le **thread principal**.

Dans une application graphique Lazarus, c'est ce thread principal qui :
- Crée et affiche la fenêtre
- Gère les événements (clics, saisies clavier)
- Exécute le code de vos gestionnaires d'événements
- Redessine l'interface

On l'appelle aussi **UI Thread** (User Interface Thread) ou **Main Thread**.

## Processus vs Threads : tableau comparatif

| Aspect | Processus | Thread |
|--------|-----------|--------|
| **Définition** | Programme en cours d'exécution | Unité d'exécution dans un processus |
| **Mémoire** | Espace mémoire isolé | Partagent la mémoire du processus |
| **Création** | Lourde et lente (plusieurs millisecondes) | Légère et rapide (microsecondes) |
| **Communication** | Difficile, nécessite l'IPC | Facile via variables partagées |
| **Crash** | N'affecte que lui-même | Peut faire crasher tout le processus |
| **Sécurité** | Isolement fort | Protection faible |
| **Cas d'usage** | Applications séparées | Tâches parallèles dans une appli |

## Pourquoi plusieurs threads dans une application ?

### Le problème du thread unique

Reprenons notre application graphique. Avec un seul thread (le thread principal), celui-ci doit :

1. Surveiller la souris et le clavier
2. Redessiner la fenêtre quand nécessaire
3. Exécuter votre code métier
4. Tout faire en même temps !

C'est comme avoir un seul employé dans une entreprise qui doit :
- Répondre au téléphone
- Traiter les commandes
- Faire la comptabilité
- Accueillir les clients

**Résultat** : Quand il est occupé à faire la comptabilité (tâche longue), il ne peut plus répondre au téléphone (interface qui gèle) !

### La solution multi-thread

Avec plusieurs threads, on peut **diviser le travail** :

- **Thread principal (UI Thread)** : S'occupe UNIQUEMENT de l'interface
- **Thread de travail (Worker Thread)** : Effectue les tâches longues

```
┌─────────────────────────────────────────┐
│           PROCESSUS (Application)       │
│                                         │
│  ┌─────────────────┐  ┌───────────────┐ │
│  │ Thread Principal│  │Thread Travail │ │
│  │    (UI Thread)  │  │ (Worker)      │ │
│  ├─────────────────┤  ├───────────────┤ │
│  │ • Gère clics    │  │ • Télécharge  │ │
│  │ • Redessine     │  │ • Calcule     │ │
│  │ • Met à jour    │  │ • Lit fichiers│ │
│  │   l'interface   │  │ • Requêtes DB │ │
│  └─────────────────┘  └───────────────┘ │
│           ▲                  │          │
│           │    Communication │          │
│           └──────────────────┘          │
└─────────────────────────────────────────┘
```

### Avantages du multi-threading

1. **Interface réactive** : Le thread UI reste toujours disponible
2. **Exploitation des processeurs multi-cœurs** : Les threads peuvent s'exécuter sur des cœurs différents
3. **Meilleure expérience utilisateur** : Barres de progression, annulations possibles
4. **Applications modernes** : C'est la norme attendue par les utilisateurs

## Le multi-threading dans votre quotidien

Vous utilisez le multi-threading tous les jours sans le savoir :

### Navigateur web
- **Thread 1** : Gère l'interface (onglets, boutons)
- **Thread 2** : Télécharge une page web
- **Thread 3** : Télécharge les images
- **Thread 4** : Exécute du JavaScript
- **Thread 5** : Lit une vidéo

Résultat : Vous pouvez cliquer sur d'autres onglets pendant qu'une page charge !

### Traitement de texte
- **Thread 1** : Gère l'interface et la saisie
- **Thread 2** : Vérification orthographique en arrière-plan
- **Thread 3** : Sauvegarde automatique
- **Thread 4** : Correction grammaticale

Résultat : L'écriture reste fluide pendant que le logiciel analyse votre texte !

### Lecteur de musique
- **Thread 1** : Interface (boutons, playlist)
- **Thread 2** : Décodage du fichier audio
- **Thread 3** : Envoi vers la carte son

Résultat : La musique ne s'arrête pas quand vous changez de chanson !

## Concepts importants à retenir

### 1. Concurrence vs Parallélisme

**Concurrence** : Plusieurs threads qui semblent s'exécuter en même temps, même sur un seul cœur de processeur. Le système alterne rapidement entre eux (comme un jongleur).

**Parallélisme** : Plusieurs threads qui s'exécutent réellement en même temps sur des cœurs différents.

Pour nous, développeurs d'applications graphiques, cette distinction n'est pas très importante. L'OS gère cela automatiquement !

### 2. Le thread principal est spécial

Dans une application graphique, **seul le thread principal peut modifier l'interface**. C'est une règle absolue dans Windows, Linux, macOS.

❌ **INTERDIT** : Modifier un `TLabel` depuis un thread de travail
✅ **PERMIS** : Demander au thread principal de modifier le `TLabel`

Nous verrons comment faire dans les sections suivantes avec `Synchronize`.

### 3. Les threads partagent la mémoire

```pascal
var
  Compteur: Integer = 0;  // Variable globale

// Thread 1 fait :
Compteur := Compteur + 1;

// Thread 2 fait en même temps :
Compteur := Compteur + 1;
```

**Problème potentiel** : Les deux threads accèdent à `Compteur` simultanément. Le résultat peut être incorrect !

C'est le problème des **variables partagées**, que nous traiterons dans la section 18.7.

### 4. Les threads ont un coût

Créer un thread n'est pas gratuit :
- Consommation de mémoire (environ 1 Mo par thread)
- Temps de création
- Surcharge de gestion par l'OS

**Règle d'or** : Ne créez pas 1000 threads ! En général, quelques threads bien utilisés suffisent.

## Les threads dans FreePascal

FreePascal fournit la classe **`TThread`** pour créer et gérer des threads facilement.

Cette classe :
- Encapsule les différences entre Windows et Linux
- Fournit des méthodes sûres pour communiquer avec le thread principal
- Gère automatiquement beaucoup de détails techniques

Dans les sections suivantes, nous allons apprendre à l'utiliser. Mais d'abord, il était important de comprendre ces concepts fondamentaux.

## Récapitulatif

### Processus
- Programme en cours d'exécution
- Mémoire isolée
- Un ou plusieurs threads

### Thread
- Unité d'exécution dans un processus
- Partage la mémoire du processus
- Peut s'exécuter en parallèle des autres threads

### Application graphique mono-thread
- UN seul thread fait tout
- Interface gèle pendant les tâches longues
- Expérience utilisateur médiocre

### Application graphique multi-thread
- Thread principal : gère l'interface
- Threads de travail : tâches longues
- Interface reste fluide
- Expérience utilisateur moderne

### Point clé à retenir
**Seul le thread principal peut modifier l'interface directement.** C'est LA règle à ne jamais oublier !

## Conclusion

Vous comprenez maintenant :
- Ce qu'est un processus
- Ce qu'est un thread
- Pourquoi on a besoin de plusieurs threads
- Les avantages et contraintes du multi-threading

Ces concepts peuvent sembler abstraits, mais ils vont devenir très concrets quand nous allons créer notre premier thread dans la section suivante.

Le multi-threading n'est finalement qu'une façon d'organiser le travail : au lieu d'avoir un seul "employé" qui fait tout, on en a plusieurs qui collaborent. Simple, non ?

Passons maintenant à la pratique avec la classe `TThread` !

⏭️ [La classe TThread : création et utilisation](/18-introduction-pratique-multi-threading/03-classe-tthread-creation-utilisation.md)
