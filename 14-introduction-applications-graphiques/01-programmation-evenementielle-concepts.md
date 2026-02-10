🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 14.1 Programmation événementielle - Concepts

## Introduction

Jusqu'à présent, vous avez créé des programmes qui s'exécutent de manière **séquentielle** : le code commence au début, suit un chemin prédéfini, et se termine. L'utilisateur interagit principalement via la console en répondant à des questions au moment où le programme le demande.

Avec les applications graphiques (fenêtrées), nous entrons dans un nouveau paradigme : la **programmation événementielle**. C'est un changement fondamental dans la façon de concevoir vos programmes.

---

## Qu'est-ce que la programmation événementielle ?

### Définition simple

La programmation événementielle est une approche où **le flux d'exécution du programme est déterminé par des événements** plutôt que par une séquence fixe d'instructions.

Un **événement** est une action qui se produit pendant l'exécution du programme, comme :
- Un clic de souris
- Une frappe au clavier
- Le redimensionnement d'une fenêtre
- Le passage du temps (timer)
- L'arrivée de données réseau

---

## Comparaison : Programmation séquentielle vs événementielle

### Programmation séquentielle (console)

```
Programme démarre
├─ Afficher "Entrez votre nom :"
├─ Lire le nom
├─ Afficher "Entrez votre âge :"
├─ Lire l'âge
├─ Calculer quelque chose
├─ Afficher le résultat
└─ Programme termine
```

**Caractéristiques :**
- Flux linéaire et prévisible
- Le programme contrôle quand l'utilisateur peut interagir
- Ordre fixe des opérations

### Programmation événementielle (GUI)

```
Programme démarre
├─ Afficher la fenêtre
└─ Attendre des événements...
    │
    ├─ Événement : Clic sur bouton "Calculer"
    │   └─ Exécuter la fonction CalculerResultat()
    │
    ├─ Événement : Saisie dans la zone de texte
    │   └─ Exécuter la fonction ValiderEntree()
    │
    ├─ Événement : Clic sur bouton "Quitter"
    │   └─ Exécuter la fonction FermerApplication()
    │
    └─ Événement : Redimensionnement fenêtre
        └─ Exécuter la fonction ReorganiserInterface()
```

**Caractéristiques :**
- Flux non-linéaire et imprévisible
- L'utilisateur contrôle quand et comment interagir
- Le programme réagit aux actions de l'utilisateur

---

## Les concepts clés

### 1. Les événements (Events)

Un événement représente quelque chose qui s'est produit dans le système. Exemples courants :

| Événement | Description |
|-----------|-------------|
| `OnClick` | L'utilisateur a cliqué sur un composant |
| `OnKeyPress` | L'utilisateur a appuyé sur une touche |
| `OnChange` | Le contenu d'un champ a été modifié |
| `OnMouseMove` | La souris s'est déplacée sur un composant |
| `OnClose` | L'utilisateur tente de fermer la fenêtre |
| `OnTimer` | Un intervalle de temps s'est écoulé |

### 2. Les gestionnaires d'événements (Event Handlers)

Un gestionnaire d'événement est une **procédure ou fonction** que vous écrivez pour répondre à un événement spécifique.

Quand un événement se produit, le système appelle automatiquement le gestionnaire correspondant.

**Exemple conceptuel :**
```pascal
// Gestionnaire pour le clic sur un bouton
procedure TForm1.BoutonCalculerClick(Sender: TObject);  
begin  
  // Votre code ici : que faire quand on clique ?
  ShowMessage('Bouton cliqué !');
end;
```

### 3. La boucle d'événements (Event Loop)

Au cœur de toute application événementielle se trouve la **boucle d'événements**. C'est un mécanisme invisible qui :

1. **Attend** qu'un événement se produise
2. **Identifie** le type d'événement et le composant concerné
3. **Appelle** le gestionnaire d'événement approprié
4. **Retourne** en attente du prochain événement

Cette boucle tourne en permanence jusqu'à ce que l'application se termine.

```
┌─────────────────────────────────────┐
│     BOUCLE D'ÉVÉNEMENTS             │
│                                     │
│  ┌────────────────────────────┐     │
│  │ Attendre un événement...   │     │
│  └────────────┬───────────────┘     │
│               │                     │
│               ↓                     │
│  ┌────────────────────────────┐     │
│  │ Événement détecté !        │     │
│  │ (ex: clic sur bouton)      │     │
│  └────────────┬───────────────┘     │
│               │                     │
│               ↓                     │
│  ┌────────────────────────────┐     │
│  │ Appeler le gestionnaire    │     │
│  │ BoutonClick()              │     │
│  └────────────┬───────────────┘     │
│               │                     │
│               ↓                     │
│  ┌────────────────────────────┐     │
│  │ Gestionnaire terminé       │     │
│  └────────────┬───────────────┘     │
│               │                     │
│               └──────┐              │
│                      │              │
└──────────────────────┼──────────────┘
                       │
                       └─→ Retour en attente
```

---

## Avantages de la programmation événementielle

### 1. Interactivité naturelle
L'utilisateur peut effectuer les actions dans l'ordre qu'il souhaite, rendant l'application plus intuitive et flexible.

### 2. Réactivité
Le programme répond immédiatement aux actions sans bloquer l'interface.

### 3. Modularité
Chaque gestionnaire d'événement est indépendant, ce qui facilite l'organisation du code.

### 4. Interface moderne
Correspond au fonctionnement standard des applications modernes (Windows, Linux, macOS).

---

## Comment penser "événementiel" ?

### Changement de mentalité

**Avant (séquentiel) :** "Mon programme fait A, puis B, puis C"

**Maintenant (événementiel) :** "Mon programme attend. Si l'utilisateur fait X, alors je fais Y"

### Questions à se poser

Lors de la conception d'une application graphique :

1. **Quels événements** peuvent se produire ?
   - Quels boutons peuvent être cliqués ?
   - Quelles zones de texte peuvent être modifiées ?

2. **Que doit-il se passer** quand chaque événement survient ?
   - Quelle action exécuter ?
   - Quelles données traiter ?
   - Quelle interface mettre à jour ?

3. **Dans quel ordre** les événements peuvent-ils arriver ?
   - Attention : l'ordre n'est PAS prévisible !

---

## Exemple conceptuel : Une calculatrice

Imaginons une calculatrice simple avec :
- Deux zones de texte (pour les nombres)
- Un bouton "Addition"
- Un label pour afficher le résultat

### Approche séquentielle (impossible en GUI)
```
1. Demander le premier nombre
2. Attendre la saisie
3. Demander le deuxième nombre
4. Attendre la saisie
5. Calculer
6. Afficher
```

### Approche événementielle (correct)
```
État initial :
- Fenêtre affichée
- Zones de texte vides
- En attente...

Événements possibles :
- L'utilisateur tape dans la première zone
  → OnChange : valider que c'est un nombre

- L'utilisateur tape dans la deuxième zone
  → OnChange : valider que c'est un nombre

- L'utilisateur clique sur "Addition"
  → OnClick :
    1. Lire les deux zones
    2. Calculer la somme
    3. Afficher dans le label
```

---

## Les pièges courants pour les débutants

### 1. Attendre qu'un événement se produise
❌ **Erreur :** Essayer de faire une boucle qui "attend" un clic
```pascal
// NE FAITES PAS ÇA !
while not BoutonClique do
  Application.ProcessMessages;
```

✅ **Correct :** Laisser la boucle d'événements gérer l'attente
```pascal
// Créez simplement le gestionnaire d'événement
procedure BoutonClick(Sender: TObject);  
begin  
  // Votre code ici
end;
```

### 2. Bloquer la boucle d'événements
❌ **Erreur :** Effectuer des opérations longues dans un gestionnaire
```pascal
procedure BoutonClick(Sender: TObject);  
var i: integer;  
begin  
  for i := 1 to 1000000000 do
    Calcul; // L'interface se fige !
end;
```

✅ **Solution :** Utiliser des threads ou découper le traitement (voir chapitre 18)

### 3. Oublier que l'ordre est imprévisible
Vous ne pouvez pas supposer qu'un événement A arrivera avant un événement B.

---

## Rôle de Lazarus et de la LCL

La bonne nouvelle : **Lazarus gère automatiquement la boucle d'événements pour vous !**

Vous n'avez pas à :
- Créer la boucle d'événements manuellement
- Détecter les clics de souris au niveau système
- Router les événements vers les bons composants

Vous devez simplement :
- Placer des composants sur votre formulaire
- Associer des gestionnaires d'événements
- Écrire le code de réponse dans ces gestionnaires

---

## En résumé

| Aspect | Programmation séquentielle | Programmation événementielle |
|--------|---------------------------|------------------------------|
| **Flux** | Linéaire, prévisible | Non-linéaire, imprévisible |
| **Contrôle** | Le programme décide | L'utilisateur décide |
| **Interaction** | Questions/Réponses fixes | Réactions à des actions |
| **Structure** | Séquence d'instructions | Gestionnaires indépendants |
| **Attente** | Bloquante (ReadLn) | Non-bloquante (boucle) |

---

## Prochaines étapes

Maintenant que vous comprenez les concepts, les prochaines sections vous montreront :
- Comment créer votre première fenêtre (14.2)
- Comment placer des composants (14.3-14.4)
- Comment écrire des gestionnaires d'événements en pratique (14.5)

La programmation événementielle peut sembler déroutante au début, mais avec la pratique, elle devient une seconde nature. Rappelez-vous : **pensez en termes de "que se passe-t-il si..." plutôt que "faire A puis B"**.

---

**Point clé à retenir :** Dans une application graphique, vous ne contrôlez plus le flux du programme. Vous définissez des **réactions** aux **actions** de l'utilisateur. Le système fait le reste !

⏭️ [Première application fenêtrée](/14-introduction-applications-graphiques/02-premiere-application-fenetree.md)
