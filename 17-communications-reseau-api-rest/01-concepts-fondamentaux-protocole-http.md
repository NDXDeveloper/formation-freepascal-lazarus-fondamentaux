🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 17.1 Concepts fondamentaux : protocole HTTP

## Introduction

Avant de pouvoir communiquer avec des serveurs web ou consommer des API REST dans nos applications FreePascal, il est essentiel de comprendre comment fonctionne le protocole HTTP. Ce protocole est le fondement de toute communication sur le web.

## Qu'est-ce que HTTP ?

**HTTP** signifie **HyperText Transfer Protocol** (Protocole de Transfert HyperTexte). C'est un protocole de communication qui permet l'échange d'informations entre un **client** (votre programme, navigateur web) et un **serveur** (ordinateur distant qui héberge des données).

### Analogie simple

Imaginez HTTP comme un système de commande dans un restaurant :
- **Vous** (le client) passez une commande
- **Le serveur** (le restaurant) reçoit votre commande
- Le serveur prépare ce que vous avez demandé
- Le serveur vous **renvoie** votre plat (ou une réponse)

## Architecture Client-Serveur

HTTP fonctionne selon un modèle **requête-réponse** :

```
   CLIENT                           SERVEUR
     |                                 |
     |  1. Envoie une REQUÊTE         |
     |------------------------------->|
     |                                 |
     |                                 | 2. Traite la requête
     |                                 |
     |  3. Renvoie une RÉPONSE        |
     |<-------------------------------|
     |                                 |
```

### Le client
C'est votre application FreePascal qui **initie** la communication. Le client envoie une **requête** au serveur pour demander quelque chose (une page web, des données, etc.).

### Le serveur
C'est l'ordinateur distant qui **attend** les requêtes des clients. Quand il reçoit une requête, il la traite et renvoie une **réponse** appropriée.

## Structure d'une URL

Avant de faire une requête HTTP, il faut savoir où l'envoyer. On utilise une **URL** (Uniform Resource Locator) :

```
https://www.example.com:443/api/users?page=2#section
│──┬──│ │─────┬────────│ │─│ │───┬───│ │──┬─││──┬──│
   │    │      │         │  │  │   │    │   │  │  │
   │    │      │         │  │  │   │    │   │  │  └─ Fragment (ancre)
   │    │      │         │  │  │   │    │   │  └──── Paramètres de requête
   │    │      │         │  │  │   │    │   └─────── Nom de la ressource
   │    │      │         │  │  │   │    └─────────── Chemin
   │    │      │         │  │  │   └──────────────── Port (optionnel)
   │    │      │         │  │  └──────────────────── Domaine
   │    │      │         │  └─────────────────────── Sous-domaine (optionnel)
   │    │      │         └────────────────────────── TLD (Top Level Domain)
   │    │      └──────────────────────────────────── Nom de domaine
   │    └─────────────────────────────────────────── Protocole (http ou https)
   └──────────────────────────────────────────────── Schéma
```

**Exemple concret :**
```
https://api.openweathermap.org/data/2.5/weather?q=Paris
```

## HTTP vs HTTPS

### HTTP (Port 80 par défaut)
- Communication **non chiffrée**
- Les données transitent en clair sur le réseau
- Adapté pour des données publiques non sensibles

### HTTPS (Port 443 par défaut)
- Communication **chiffrée** avec SSL/TLS
- Les données sont protégées contre l'interception
- **Recommandé** pour toute communication sensible
- Obligatoire pour les mots de passe, données bancaires, etc.

> **Important :** Aujourd'hui, HTTPS est devenu la norme. La plupart des API modernes exigent HTTPS.

## Anatomie d'une Requête HTTP

Une requête HTTP se compose de plusieurs éléments :

### 1. La ligne de requête
```
GET /api/users HTTP/1.1
│   │          │
│   │          └─ Version du protocole
│   └──────────── Ressource demandée (chemin)
└──────────────── Méthode HTTP
```

### 2. Les en-têtes (Headers)
Les en-têtes fournissent des informations supplémentaires sur la requête :

```
Host: api.example.com  
User-Agent: MyFreePascalApp/1.0  
Accept: application/json  
Content-Type: application/json  
Authorization: Bearer token123  
```

**En-têtes courants :**
- **Host** : nom du serveur cible (obligatoire en HTTP/1.1)
- **User-Agent** : identifie le client (navigateur, application)
- **Accept** : type de contenu que le client accepte en réponse
- **Content-Type** : type du contenu envoyé dans le corps de la requête
- **Authorization** : informations d'authentification

### 3. Le corps de la requête (Body)
Optionnel, contient les données envoyées au serveur (pour POST, PUT, etc.)

```json
{
  "nom": "Dupont",
  "prenom": "Jean",
  "email": "jean.dupont@example.com"
}
```

## Anatomie d'une Réponse HTTP

Le serveur renvoie une réponse structurée de façon similaire :

### 1. La ligne de statut
```
HTTP/1.1 200 OK
│        │   │
│        │   └─ Message de statut
│        └───── Code de statut
└────────────── Version du protocole
```

### 2. Les en-têtes de réponse
```
Content-Type: application/json  
Content-Length: 1234  
Date: Mon, 15 Oct 2025 10:30:00 GMT  
Server: Apache/2.4.41  
```

### 3. Le corps de la réponse
Contient les données renvoyées par le serveur :

```json
{
  "id": 42,
  "nom": "Dupont",
  "prenom": "Jean",
  "email": "jean.dupont@example.com"
}
```

## Codes de Statut HTTP

Le serveur indique le résultat de la requête avec un **code de statut** numérique à 3 chiffres :

### 2xx - Succès
- **200 OK** : requête réussie, réponse contient les données demandées
- **201 Created** : ressource créée avec succès
- **204 No Content** : succès, mais pas de contenu à renvoyer

### 3xx - Redirection
- **301 Moved Permanently** : la ressource a été déplacée définitivement
- **302 Found** : redirection temporaire
- **304 Not Modified** : la ressource n'a pas changé (cache)

### 4xx - Erreur Client
- **400 Bad Request** : la requête est mal formée
- **401 Unauthorized** : authentification requise
- **403 Forbidden** : accès interdit (même authentifié)
- **404 Not Found** : ressource introuvable
- **429 Too Many Requests** : trop de requêtes (limitation de débit)

### 5xx - Erreur Serveur
- **500 Internal Server Error** : erreur interne du serveur
- **502 Bad Gateway** : problème de passerelle
- **503 Service Unavailable** : service temporairement indisponible

> **Mnémotechnique :**
> - **2xx** = "C'est bon !"
> - **3xx** = "Allez voir ailleurs"
> - **4xx** = "C'est votre faute"
> - **5xx** = "C'est ma faute" (serveur)

## Sessions et État

**HTTP est un protocole sans état (stateless)** : chaque requête est indépendante. Le serveur ne "se souvient" pas des requêtes précédentes.

### Pourquoi c'est important ?

Si vous faites deux requêtes successives, le serveur ne saura pas que c'est le même client, sauf si vous lui fournissez un moyen de vous identifier.

### Solutions courantes :

**1. Cookies**
Petits fichiers texte stockés côté client, renvoyés automatiquement à chaque requête :
```
Set-Cookie: session_id=abc123; Path=/; HttpOnly
```

**2. Tokens d'authentification**
Chaîne de caractères envoyée dans l'en-tête `Authorization` :
```
Authorization: Bearer eyJhbGciOiJIUzI1NiIs...
```

**3. Sessions côté serveur**
Le serveur stocke les informations de session, le client envoie juste un identifiant.

## Connexions Persistantes

### HTTP/1.0 (ancien)
Une nouvelle connexion TCP pour chaque requête/réponse :
```
Connexion → Requête → Réponse → Fermeture  
Connexion → Requête → Réponse → Fermeture  
```

### HTTP/1.1 (moderne)
Connexions persistantes par défaut (Keep-Alive) :
```
Connexion → Requête → Réponse → Requête → Réponse → Fermeture
```

**Avantages :**
- Moins de latence
- Économie de ressources (pas de reconnexion TCP)
- Meilleure performance globale

## Résumé des Concepts Clés

1. **HTTP** est un protocole de communication client-serveur basé sur le modèle requête-réponse
2. **HTTPS** ajoute une couche de chiffrement pour sécuriser les échanges
3. Une **URL** identifie précisément une ressource sur le web
4. Les **requêtes** contiennent une méthode, des en-têtes et éventuellement un corps
5. Les **réponses** incluent un code de statut, des en-têtes et éventuellement un corps
6. Les **codes de statut** indiquent le résultat de la requête (2xx = succès, 4xx = erreur client, 5xx = erreur serveur)
7. HTTP est **sans état** : chaque requête est indépendante

## Ce Qu'il Faut Retenir pour la Suite

Maintenant que vous comprenez les fondamentaux de HTTP, vous êtes prêt à :
- Comprendre les différentes **méthodes HTTP** (GET, POST, PUT, DELETE)
- Utiliser la bibliothèque **TFPHttpClient** de FreePascal
- Consommer des **API REST**
- Échanger des données au format **JSON**

Dans les prochaines sections, nous mettrons ces concepts en pratique avec du code FreePascal concret !

⏭️ [Méthodes HTTP (GET, POST, PUT, DELETE)](/17-communications-reseau-api-rest/02-methodes-http-get-post-put-delete.md)
