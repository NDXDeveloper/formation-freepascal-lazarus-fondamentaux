🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 17.3 Introduction aux API REST

## Introduction

Maintenant que vous connaissez le protocole HTTP et ses méthodes, nous allons découvrir les **API REST**, qui sont devenues le standard de communication entre applications sur le web. Presque tous les services modernes (réseaux sociaux, météo, paiement en ligne, etc.) exposent une API REST.

## Qu'est-ce qu'une API ?

### Définition simple

**API** signifie **Application Programming Interface** (Interface de Programmation d'Application).

Une API est une "porte d'entrée" qui permet à votre programme de communiquer avec un autre programme ou service, sans avoir besoin de connaître son fonctionnement interne.

### Analogie : Le Restaurant

Imaginez un restaurant :
- **La cuisine** = le serveur (où les données sont stockées et traitées)
- **Vous** = votre application cliente
- **Le menu** = la documentation de l'API (liste des plats disponibles)
- **Le serveur/serveur** = l'API (l'intermédiaire qui prend votre commande et vous ramène le plat)

Vous n'avez pas besoin de savoir comment le chef prépare le plat (implémentation), vous utilisez simplement le menu (API) pour commander.

```
Votre Application  →  API  →  Serveur/Base de données
    (Client)          ↓         (Backend)
                   Réponse
```

### Exemples d'API dans la vie réelle

- **API Météo** : votre application demande "Quel temps fait-il à Paris ?" → l'API renvoie les données météo
- **API Google Maps** : vous demandez "Quelle est la distance entre Paris et Lyon ?" → l'API calcule et renvoie la distance
- **API de Paiement** : votre e-commerce demande "Traiter ce paiement" → l'API traite la transaction
- **API Twitter** : votre application demande "Affiche les derniers tweets" → l'API renvoie les tweets

## Qu'est-ce que REST ?

### Définition

**REST** signifie **REpresentational State Transfer** (Transfert de l'État de Représentation).

C'est un **style architectural** créé par Roy Fielding en 2000, qui définit des conventions pour créer des API web standardisées et prévisibles.

> **Note :** REST n'est pas un protocole ou un standard strict, c'est plutôt un ensemble de bonnes pratiques et de principes.

### Pourquoi REST ?

Avant REST, chaque API avait sa propre manière de fonctionner. REST a apporté une **standardisation** :
- ✅ Facile à comprendre et à utiliser
- ✅ Prévisible (mêmes conventions partout)
- ✅ Scalable (peut gérer beaucoup d'utilisateurs)
- ✅ Sans état (chaque requête est indépendante)

## Les 6 Principes REST

### 1. Architecture Client-Serveur

Le **client** (votre application) et le **serveur** (l'API) sont séparés et indépendants.

```
Client (FreePascal App)  ←→  Serveur (API REST)
    Interface utilisateur      Logique métier
    Présentation              Base de données
```

**Avantage :** On peut modifier le client sans toucher au serveur, et vice-versa.

### 2. Sans État (Stateless)

Chaque requête doit contenir **toutes** les informations nécessaires. Le serveur ne stocke rien entre les requêtes.

```
❌ AVEC ÉTAT (à éviter)              ✅ SANS ÉTAT (REST)
─────────────────────────            ──────────────────────
1. POST /login                       1. GET /users/42
   → Serveur : "OK, je me souviens"     Headers: Authorization: Bearer token123
                                        → Le token contient tout le nécessaire
2. GET /profile
   → Serveur : "Je sais qui tu es"   2. GET /orders
                                        Headers: Authorization: Bearer token123
```

**Avantage :** Meilleure scalabilité, le serveur n'a pas à mémoriser l'état de chaque client.

### 3. Cachable

Les réponses doivent indiquer si elles peuvent être mises en cache ou non.

```
HTTP/1.1 200 OK  
Cache-Control: max-age=3600  
ETag: "abc123"  

→ Le client peut réutiliser cette réponse pendant 1 heure
```

**Avantage :** Réduit la charge serveur et améliore les performances.

### 4. Interface Uniforme

Toutes les API REST suivent les **mêmes conventions** :
- Utilisation des méthodes HTTP standard (GET, POST, PUT, DELETE)
- URLs structurées de manière logique
- Réponses au format standard (JSON, XML)

**Avantage :** Une fois que vous connaissez une API REST, vous pouvez facilement en utiliser d'autres.

### 5. Système en Couches

Le client ne sait pas s'il communique directement avec le serveur final ou avec un intermédiaire.

```
Client → Load Balancer → Cache → API Server → Database
```

**Avantage :** On peut ajouter des serveurs de cache, de sécurité, etc., sans que le client le sache.

### 6. Code à la Demande (Optionnel)

Le serveur peut envoyer du code exécutable au client (JavaScript, etc.). Ce principe est optionnel et rarement utilisé.

## Structure d'une API REST : Les Ressources

### Concept de Ressource

En REST, tout est **ressource**. Une ressource est une entité que vous manipulez via l'API.

**Exemples de ressources :**
- Utilisateurs (`users`)
- Articles (`articles`)
- Commandes (`orders`)
- Produits (`products`)

### URLs RESTful : Bonnes Pratiques

#### Règle 1 : Utiliser des noms au pluriel

```
✅ BIEN                    ❌ À ÉVITER
/api/users                /api/user
/api/products             /api/product
/api/orders               /api/order
```

#### Règle 2 : Hiérarchie des ressources

```
Collection    →  /api/users              (tous les utilisateurs)
Élément       →  /api/users/42           (utilisateur #42)
Sous-ressource → /api/users/42/orders   (commandes de l'utilisateur #42)
```

#### Règle 3 : Utiliser les méthodes HTTP pour les actions

**Ne mettez PAS les actions dans l'URL !**

```
❌ MAUVAIS                        ✅ BON
GET  /api/getUsers               GET    /api/users  
POST /api/createUser             POST   /api/users  
POST /api/updateUser/42          PUT    /api/users/42  
POST /api/deleteUser/42          DELETE /api/users/42  
GET  /api/getUserOrders/42       GET    /api/users/42/orders  
```

#### Règle 4 : Utiliser des noms, pas des verbes

```
❌ MAUVAIS                        ✅ BON
/api/getAllProducts              /api/products
/api/createNewProduct            /api/products (avec POST)
/api/deleteProduct/42            /api/products/42 (avec DELETE)
```

#### Règle 5 : Utiliser des minuscules et des tirets

```
✅ BIEN                          ❌ À ÉVITER
/api/users                       /api/Users
/api/product-categories          /api/productCategories
/api/order-items                 /api/order_items
```

### Paramètres de Requête

Pour filtrer, trier ou paginer, utilisez des **query parameters** :

```
# Filtrage
GET /api/products?category=electronics&price_max=500

# Tri
GET /api/users?sort=name&order=asc

# Pagination
GET /api/articles?page=2&limit=20

# Recherche
GET /api/books?search=pascal&author=Wirth

# Sélection de champs (pour optimiser)
GET /api/users?fields=id,name,email
```

## Opérations CRUD avec REST

Voici comment les opérations CRUD se traduisent en API REST :

### Exemple : Gestion d'Utilisateurs

```
┌────────────────────────────────────────────────────────────────────┐
│ OPÉRATION             │ MÉTHODE  │ URL              │ CORPS        │
├───────────────────────┼──────────┼──────────────────┼──────────────┤
│ Lister tous           │ GET      │ /api/users       │ -            │
│ Obtenir un utilisateur│ GET      │ /api/users/42    │ -            │
│ Créer                 │ POST     │ /api/users       │ { données }  │
│ Modifier (complet)    │ PUT      │ /api/users/42    │ { données }  │
│ Modifier (partiel)    │ PATCH    │ /api/users/42    │ { données }  │
│ Supprimer             │ DELETE   │ /api/users/42    │ -            │
└────────────────────────────────────────────────────────────────────┘
```

### Exemple : Sous-ressources

Pour gérer les commandes d'un utilisateur :

```
GET    /api/users/42/orders           → Lister toutes les commandes de l'utilisateur 42  
GET    /api/users/42/orders/5         → Obtenir la commande #5 de l'utilisateur 42  
POST   /api/users/42/orders           → Créer une commande pour l'utilisateur 42  
DELETE /api/users/42/orders/5         → Supprimer la commande #5 de l'utilisateur 42  
```

## Formats de Données : JSON

Les API REST modernes utilisent principalement **JSON** (JavaScript Object Notation) pour échanger des données.

### Pourquoi JSON ?

- ✅ Facile à lire pour les humains
- ✅ Facile à parser pour les machines
- ✅ Léger (peu de caractères superflus)
- ✅ Supporté par tous les langages
- ✅ Structure flexible

### Exemple de réponse JSON

**Requête :**
```
GET /api/users/42
```

**Réponse :**
```json
{
  "id": 42,
  "nom": "Dupont",
  "prenom": "Jean",
  "email": "jean.dupont@example.com",
  "date_creation": "2025-01-15T10:30:00Z",
  "actif": true,
  "roles": ["user", "admin"],
  "adresse": {
    "rue": "123 rue de la Paix",
    "ville": "Paris",
    "code_postal": "75001"
  }
}
```

### Collection vs Élément

**Un seul élément :**
```json
{
  "id": 42,
  "nom": "Dupont"
}
```

**Une collection (liste) :**
```json
[
  {
    "id": 42,
    "nom": "Dupont"
  },
  {
    "id": 43,
    "nom": "Martin"
  }
]
```

**Collection avec métadonnées (recommandé) :**
```json
{
  "data": [
    { "id": 42, "nom": "Dupont" },
    { "id": 43, "nom": "Martin" }
  ],
  "total": 150,
  "page": 1,
  "per_page": 2
}
```

## Codes de Statut dans les API REST

Les codes HTTP sont essentiels pour communiquer le résultat d'une opération.

### Succès (2xx)

```
200 OK                  → Succès général (GET, PUT, PATCH)
201 Created             → Ressource créée (POST)
204 No Content          → Succès sans contenu (DELETE)
```

### Redirection (3xx)

```
301 Moved Permanently   → Ressource déplacée définitivement
304 Not Modified        → Ressource non modifiée (cache valide)
```

### Erreurs Client (4xx)

```
400 Bad Request         → Requête mal formée
401 Unauthorized        → Authentification requise
403 Forbidden           → Accès interdit
404 Not Found           → Ressource introuvable
405 Method Not Allowed  → Méthode HTTP non autorisée
409 Conflict            → Conflit (ex: ressource déjà existante)
422 Unprocessable Entity → Données invalides
429 Too Many Requests   → Trop de requêtes (rate limiting)
```

### Erreurs Serveur (5xx)

```
500 Internal Server Error → Erreur interne du serveur
502 Bad Gateway           → Erreur de passerelle
503 Service Unavailable   → Service temporairement indisponible
```

## Versioning d'API

Les API évoluent. Il faut pouvoir modifier l'API sans casser les clients existants.

### Méthode 1 : Versioning dans l'URL (recommandé)

```
https://api.example.com/v1/users  
https://api.example.com/v2/users  
https://api.example.com/v3/users  
```

**Avantages :** Simple, clair, visible immédiatement

### Méthode 2 : Versioning dans les headers

```
GET /api/users  
Accept: application/vnd.example.v2+json  
```

### Méthode 3 : Sous-domaine

```
https://v1.api.example.com/users  
https://v2.api.example.com/users  
```

**Bonne pratique :** Maintenir au moins 2 versions en parallèle, annoncer les dépréciations à l'avance.

## Authentification et Sécurité

### Méthodes d'authentification courantes

#### 1. API Key (Clé d'API)

Clé secrète passée dans les headers ou l'URL :

```
GET /api/users  
X-API-Key: abc123def456ghi789  
```

**Usage :** API publiques avec limitation de débit

#### 2. Bearer Token (JWT)

Token d'authentification dans le header `Authorization` :

```
GET /api/users  
Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...  
```

**Usage :** Authentification utilisateur, le plus courant aujourd'hui

#### 3. OAuth 2.0

Standard pour autoriser des applications tierces (connexion avec Google, Facebook, etc.)

```
1. L'utilisateur autorise votre app
2. Vous recevez un token d'accès
3. Vous utilisez ce token pour faire des requêtes
```

### Bonnes pratiques de sécurité

✅ **À FAIRE :**
- Toujours utiliser HTTPS (jamais HTTP pour des données sensibles)
- Ne jamais exposer les API keys dans le code source public
- Implémenter un rate limiting (limitation du nombre de requêtes)
- Valider toutes les entrées utilisateur
- Utiliser des tokens avec expiration

❌ **À ÉVITER :**
- Passer des credentials dans l'URL
- Stocker des mots de passe en clair
- Accepter des requêtes sans authentification pour des données sensibles

## Rate Limiting (Limitation de Débit)

Les API limitent souvent le nombre de requêtes pour éviter les abus :

```
X-RateLimit-Limit: 1000        → Limite maximale  
X-RateLimit-Remaining: 573     → Requêtes restantes  
X-RateLimit-Reset: 1634567890  → Timestamp de réinitialisation  
```

Si vous dépassez la limite :
```
HTTP/1.1 429 Too Many Requests  
Retry-After: 3600  

{
  "error": "Rate limit exceeded. Try again in 1 hour."
}
```

## Documentation d'API : Swagger/OpenAPI

Une bonne API REST doit être **documentée**. Le standard le plus utilisé est **OpenAPI** (anciennement Swagger).

### Exemple de documentation

```
Endpoint: GET /api/users/{id}

Description: Récupère les informations d'un utilisateur

Paramètres:
  - id (path, required): ID de l'utilisateur

Headers:
  - Authorization (required): Bearer token

Réponses:
  200 OK:
    {
      "id": 42,
      "nom": "Dupont",
      "email": "jean@example.com"
    }
  404 Not Found:
    {
      "error": "User not found"
    }
```

### Outils populaires

- **Swagger UI** : Interface interactive pour tester l'API
- **Postman** : Client API avec collections et tests
- **Insomnia** : Alternative à Postman

## Exemple Complet : API de Blog

Voici une API REST complète pour un blog :

```
┌─────────────────────────────────────────────────────────────────┐
│ RESSOURCE             │ MÉTHODE  │ URL                          │
├───────────────────────┼──────────┼──────────────────────────────┤
│ Articles              │          │                              │
│ - Lister tous         │ GET      │ /api/v1/articles             │
│ - Un article          │ GET      │ /api/v1/articles/123         │
│ - Créer               │ POST     │ /api/v1/articles             │
│ - Modifier            │ PUT      │ /api/v1/articles/123         │
│ - Supprimer           │ DELETE   │ /api/v1/articles/123         │
│                       │          │                              │
│ Commentaires          │          │                              │
│ - Lister              │ GET      │ /api/v1/articles/123/comments│
│ - Un commentaire      │ GET      │ /api/v1/comments/456         │
│ - Créer               │ POST     │ /api/v1/articles/123/comments│
│ - Supprimer           │ DELETE   │ /api/v1/comments/456         │
│                       │          │                              │
│ Auteurs               │          │                              │
│ - Lister tous         │ GET      │ /api/v1/authors              │
│ - Un auteur           │ GET      │ /api/v1/authors/789          │
│ - Articles d'auteur   │ GET      │ /api/v1/authors/789/articles │
└─────────────────────────────────────────────────────────────────┘
```

### Exemple de flux complet

```
1. Lister les articles
   GET /api/v1/articles?page=1&limit=10
   → Renvoie les 10 premiers articles

2. Lire un article
   GET /api/v1/articles/123
   → Renvoie l'article complet

3. Créer un commentaire
   POST /api/v1/articles/123/comments
   Authorization: Bearer token123
   Body: { "text": "Super article !", "author": "Jean" }
   → Renvoie 201 Created avec le commentaire créé

4. Lister les commentaires
   GET /api/v1/articles/123/comments
   → Renvoie tous les commentaires de l'article 123
```

## Gestion des Erreurs

Une bonne API REST renvoie des erreurs claires et exploitables.

### Format standard d'erreur

```json
{
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "Les données fournies sont invalides",
    "details": [
      {
        "field": "email",
        "message": "Format d'email invalide"
      },
      {
        "field": "age",
        "message": "L'âge doit être supérieur à 18"
      }
    ]
  }
}
```

### Exemples par code de statut

**400 Bad Request :**
```json
{
  "error": "Invalid JSON in request body"
}
```

**401 Unauthorized :**
```json
{
  "error": "Authentication required. Please provide a valid token."
}
```

**404 Not Found :**
```json
{
  "error": "User with ID 999 not found"
}
```

**429 Too Many Requests :**
```json
{
  "error": "Rate limit exceeded. Try again in 3600 seconds."
}
```

## REST vs Autres Architectures

### REST vs SOAP

```
REST                          SOAP
─────────────────            ──────────────────
✅ Simple                    ❌ Complexe
✅ JSON/XML                  ❌ XML uniquement
✅ HTTP                      ❌ Protocole plus lourd
✅ Léger                     ❌ Verbeux
❌ Pas de contrat strict     ✅ WSDL (contrat formel)
```

**Usage :** REST pour 95% des cas, SOAP pour systèmes legacy ou très stricts

### REST vs GraphQL

```
REST                          GraphQL
─────────────────            ──────────────────
✅ Simple à comprendre       ❌ Courbe d'apprentissage
✅ Cache HTTP standard       ❌ Cache plus complexe
❌ Sur/sous-fetching         ✅ Données exactes demandées
❌ Plusieurs endpoints       ✅ Un seul endpoint
✅ Standard établi           ✅ Moderne, flexible
```

**Usage :** REST pour la majorité, GraphQL pour applications complexes avec besoins variés

## Points Clés à Retenir

1. **REST** = style architectural pour créer des API web standardisées
2. **Ressources** = tout est une ressource accessible via une URL
3. **Méthodes HTTP** = GET (lire), POST (créer), PUT (modifier), DELETE (supprimer)
4. **Sans état** = chaque requête contient toutes les informations nécessaires
5. **JSON** = format de données standard pour REST
6. **Codes HTTP** = communiquent le résultat (2xx succès, 4xx erreur client, 5xx erreur serveur)
7. **URLs RESTful** = noms au pluriel, hiérarchie logique, pas de verbes
8. **Authentification** = API Key, Bearer Token, OAuth 2.0
9. **Versioning** = /v1/, /v2/ dans l'URL
10. **Documentation** = essentielle (Swagger/OpenAPI)

## Ce Qu'il Faut Retenir pour la Suite

Maintenant que vous comprenez les API REST, vous êtes prêt à :
- Consommer des API REST publiques dans vos applications FreePascal
- Comprendre la documentation d'API
- Utiliser **TFPHttpClient** pour faire des requêtes REST
- Parser les réponses **JSON**
- Gérer l'authentification et les erreurs

Dans les prochaines sections, nous allons mettre en pratique tout cela avec du code FreePascal concret !

⏭️ [Format JSON : structure et syntaxe](/17-communications-reseau-api-rest/04-format-json-structure-syntaxe.md)
