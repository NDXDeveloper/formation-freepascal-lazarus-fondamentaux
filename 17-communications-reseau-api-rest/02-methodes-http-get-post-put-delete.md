🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 17.2 Méthodes HTTP (GET, POST, PUT, DELETE)

## Introduction

Dans la section précédente, nous avons vu la structure générale d'une requête HTTP. Maintenant, nous allons nous concentrer sur les **méthodes HTTP**, aussi appelées **verbes HTTP**. Ces méthodes indiquent au serveur quelle **action** nous souhaitons effectuer sur une ressource.

Pensez aux méthodes HTTP comme aux verbes d'action dans une phrase : consulter, créer, modifier, supprimer. Chaque méthode a un rôle spécifique et des comportements bien définis.

## L'Analogie CRUD

En programmation, on parle souvent des opérations **CRUD** pour manipuler des données :
- **C**reate (Créer)
- **R**ead (Lire)
- **U**pdate (Mettre à jour)
- **D**elete (Supprimer)

Les méthodes HTTP correspondent à ces opérations :

```
CRUD          Méthode HTTP      Action
────────────────────────────────────────────
Create   →    POST              Créer une nouvelle ressource  
Read     →    GET               Lire/Consulter une ressource  
Update   →    PUT               Modifier une ressource existante  
Delete   →    DELETE            Supprimer une ressource  
```

## La Méthode GET - Lire des Données

### Concept

**GET** est la méthode la plus utilisée. Elle sert à **récupérer** des données depuis le serveur, sans les modifier.

### Analogie

Comme consulter un livre à la bibliothèque : vous le lisez, mais vous ne le modifiez pas.

### Caractéristiques

- **Lecture seule** : ne modifie jamais les données sur le serveur
- **Sûre** : peut être répétée sans danger
- **Cachable** : les réponses peuvent être mises en cache
- **Paramètres dans l'URL** : les données sont passées via l'URL

### Structure d'une requête GET

```
GET /api/users/42 HTTP/1.1  
Host: api.example.com  
Accept: application/json  
```

Pas de corps (body) dans une requête GET. Les paramètres sont dans l'URL :

```
GET /api/users?page=2&limit=10 HTTP/1.1
```

### Exemples d'utilisation

**Récupérer un utilisateur spécifique :**
```
GET /api/users/42
```
Signification : "Donne-moi les informations de l'utilisateur numéro 42"

**Récupérer une liste avec filtres :**
```
GET /api/products?category=electronics&price_max=500
```
Signification : "Donne-moi les produits électroniques coûtant maximum 500€"

**Rechercher :**
```
GET /api/search?q=freepascal&lang=fr
```
Signification : "Recherche 'freepascal' en français"

### Réponse typique

```
HTTP/1.1 200 OK  
Content-Type: application/json  

{
  "id": 42,
  "nom": "Dupont",
  "prenom": "Jean",
  "email": "jean.dupont@example.com"
}
```

### Codes de statut courants avec GET

- **200 OK** : ressource trouvée et renvoyée
- **404 Not Found** : ressource introuvable
- **304 Not Modified** : ressource non modifiée (cache valide)

### Bonnes pratiques

✅ **À FAIRE :**
- Utiliser GET pour toutes les lectures de données
- Rendre les requêtes GET idempotentes (même résultat à chaque appel)
- Utiliser des paramètres d'URL pour les filtres et la pagination

❌ **À ÉVITER :**
- Modifier des données avec GET (utiliser POST/PUT/DELETE)
- Envoyer des données sensibles dans l'URL (visibles dans les logs)
- Créer des URLs trop longues (limite ~2000 caractères selon navigateurs)

## La Méthode POST - Créer des Données

### Concept

**POST** sert à **créer** une nouvelle ressource sur le serveur. C'est comme ajouter une nouvelle fiche dans un fichier.

### Analogie

Comme remplir un formulaire d'inscription et le soumettre : vous créez un nouveau compte.

### Caractéristiques

- **Création** : ajoute une nouvelle ressource
- **Non idempotente** : répéter la requête crée plusieurs ressources
- **Corps de requête** : les données sont dans le body, pas dans l'URL
- **Non cachable** : la réponse n'est généralement pas mise en cache

### Structure d'une requête POST

```
POST /api/users HTTP/1.1  
Host: api.example.com  
Content-Type: application/json  
Content-Length: 98  

{
  "nom": "Martin",
  "prenom": "Sophie",
  "email": "sophie.martin@example.com"
}
```

### Exemples d'utilisation

**Créer un nouvel utilisateur :**
```
POST /api/users  
Body: { "nom": "Martin", "prenom": "Sophie", ... }  
```

**Soumettre un formulaire de contact :**
```
POST /api/contact  
Body: { "email": "...", "message": "..." }  
```

**Uploader un fichier :**
```
POST /api/upload  
Content-Type: multipart/form-data  
Body: [données du fichier]  
```

### Réponse typique

```
HTTP/1.1 201 Created  
Location: /api/users/123  
Content-Type: application/json  

{
  "id": 123,
  "nom": "Martin",
  "prenom": "Sophie",
  "email": "sophie.martin@example.com",
  "created_at": "2025-10-15T10:30:00Z"
}
```

### Codes de statut courants avec POST

- **201 Created** : ressource créée avec succès
- **200 OK** : succès (si pas de nouvelle ressource créée)
- **400 Bad Request** : données invalides
- **409 Conflict** : ressource déjà existante (ex: email déjà utilisé)

### Bonnes pratiques

✅ **À FAIRE :**
- Utiliser POST pour créer de nouvelles ressources
- Renvoyer le code 201 Created avec un header `Location`
- Valider les données côté serveur
- Renvoyer la ressource créée dans la réponse

❌ **À ÉVITER :**
- Utiliser POST pour de simples lectures (utiliser GET)
- Oublier de gérer les doublons

## La Méthode PUT - Modifier des Données

### Concept

**PUT** sert à **remplacer complètement** une ressource existante. On envoie la version complète et à jour de la ressource.

### Analogie

Comme réécrire entièrement une page d'un cahier : vous remplacez tout le contenu de la page.

### Caractéristiques

- **Mise à jour complète** : remplace l'intégralité de la ressource
- **Idempotente** : répéter la même requête donne le même résultat
- **Corps de requête** : contient la ressource complète
- **Nécessite l'ID** : l'URL doit identifier précisément la ressource

### Structure d'une requête PUT

```
PUT /api/users/123 HTTP/1.1  
Host: api.example.com  
Content-Type: application/json  

{
  "id": 123,
  "nom": "Martin",
  "prenom": "Sophie",
  "email": "sophie.nouveau@example.com",
  "telephone": "0612345678"
}
```

> **Important :** Notez qu'on envoie **TOUTES** les propriétés, pas seulement celles modifiées.

### Exemples d'utilisation

**Modifier un utilisateur :**
```
PUT /api/users/123  
Body: { version complète avec modifications }  
```

**Remplacer une configuration :**
```
PUT /api/settings  
Body: { configuration complète }  
```

### Réponse typique

```
HTTP/1.1 200 OK  
Content-Type: application/json  

{
  "id": 123,
  "nom": "Martin",
  "prenom": "Sophie",
  "email": "sophie.nouveau@example.com",
  "telephone": "0612345678",
  "updated_at": "2025-10-15T11:00:00Z"
}
```

### Codes de statut courants avec PUT

- **200 OK** : ressource mise à jour avec succès
- **204 No Content** : succès, pas de contenu à renvoyer
- **404 Not Found** : ressource à modifier introuvable
- **400 Bad Request** : données invalides

### PUT vs PATCH

Il existe aussi la méthode **PATCH** pour des modifications partielles :

```
PUT (remplacement complet)          PATCH (modification partielle)
────────────────────────────────────────────────────────────────
PUT /api/users/123                  PATCH /api/users/123
{                                   {
  "nom": "Martin",                    "email": "nouveau@example.com"
  "prenom": "Sophie",               }
  "email": "nouveau@example.com",
  "telephone": "0612345678"         → Modifie uniquement l'email
}
→ Remplace tout
```

**En pratique :**
- **PUT** = "Voici la nouvelle version complète"
- **PATCH** = "Voici uniquement ce qui change"

### Bonnes pratiques

✅ **À FAIRE :**
- Envoyer la ressource complète avec PUT
- Vérifier que la ressource existe avant de la modifier
- Utiliser PATCH si vous ne voulez modifier que quelques champs

❌ **À ÉVITER :**
- Utiliser PUT pour créer une ressource (utiliser POST)
- Envoyer des données partielles avec PUT (utiliser PATCH)

## La Méthode DELETE - Supprimer des Données

### Concept

**DELETE** sert à **supprimer** une ressource du serveur.

### Analogie

Comme déchirer une page d'un cahier : la page n'existe plus.

### Caractéristiques

- **Suppression** : retire la ressource du serveur
- **Idempotente** : supprimer plusieurs fois = même résultat (déjà supprimée)
- **Pas de corps** : généralement sans body
- **Nécessite l'ID** : l'URL identifie la ressource à supprimer

### Structure d'une requête DELETE

```
DELETE /api/users/123 HTTP/1.1  
Host: api.example.com  
Authorization: Bearer token123  
```

Pas de corps dans la plupart des cas.

### Exemples d'utilisation

**Supprimer un utilisateur :**
```
DELETE /api/users/123
```

**Supprimer un article :**
```
DELETE /api/articles/456
```

**Vider un panier :**
```
DELETE /api/cart
```

### Réponse typique

**Option 1 : Pas de contenu**
```
HTTP/1.1 204 No Content
```

**Option 2 : Confirmation avec détails**
```
HTTP/1.1 200 OK  
Content-Type: application/json  

{
  "message": "Utilisateur supprimé avec succès",
  "deleted_id": 123
}
```

### Codes de statut courants avec DELETE

- **204 No Content** : suppression réussie, pas de contenu
- **200 OK** : suppression réussie avec message de confirmation
- **404 Not Found** : ressource déjà supprimée ou inexistante
- **403 Forbidden** : pas le droit de supprimer cette ressource

### Suppression douce (Soft Delete)

En pratique, beaucoup d'API ne suppriment pas vraiment les données, mais les marquent comme "supprimées" :

```
DELETE /api/users/123

→ En base de données :
   UPDATE users SET deleted_at = NOW() WHERE id = 123

   Au lieu de :
   DELETE FROM users WHERE id = 123
```

**Avantages :**
- Possibilité de restaurer
- Historique conservé
- Conformité RGPD (traçabilité)

### Bonnes pratiques

✅ **À FAIRE :**
- Demander confirmation pour les suppressions importantes
- Implémenter une authentification/autorisation stricte
- Envisager la suppression douce pour les données critiques
- Renvoyer 204 No Content si pas de détails à communiquer

❌ **À ÉVITER :**
- Permettre la suppression sans authentification
- Supprimer des données sans possibilité de récupération sur des entités critiques

## Tableau Récapitulatif

| Méthode | Action | Idempotente ? | Sûre ? | Corps de requête | Codes courants |
|---------|--------|---------------|---------|------------------|----------------|
| **GET** | Lire | ✅ Oui | ✅ Oui | ❌ Non | 200, 404, 304 |
| **POST** | Créer | ❌ Non | ❌ Non | ✅ Oui | 201, 200, 400, 409 |
| **PUT** | Remplacer | ✅ Oui | ❌ Non | ✅ Oui | 200, 204, 404, 400 |
| **DELETE** | Supprimer | ✅ Oui | ❌ Non | ❌ Généralement non | 204, 200, 404, 403 |

### Explications des termes

**Idempotente** : Répéter plusieurs fois la même requête produit le même résultat qu'une seule fois.
- GET /users/42 → toujours le même utilisateur
- PUT /users/42 → même si répété, l'utilisateur a le même état final
- DELETE /users/42 → même si répété, l'utilisateur est supprimé
- POST /users → crée un NOUVEAU utilisateur à chaque fois ❌

**Sûre (Safe)** : Ne modifie pas l'état du serveur.
- GET est sûre (lecture seule)
- POST, PUT, DELETE ne sont pas sûres (modifications)

## Autres Méthodes HTTP (Aperçu)

Il existe d'autres méthodes HTTP, moins utilisées mais utiles à connaître :

### PATCH
Modification **partielle** d'une ressource (on a déjà vu la différence avec PUT).

### HEAD
Identique à GET mais ne renvoie que les en-têtes, pas le corps. Utile pour vérifier l'existence d'une ressource ou sa taille sans la télécharger.

```
HEAD /api/users/42 HTTP/1.1
→ Renvoie uniquement les headers (Content-Length, Last-Modified, etc.)
```

### OPTIONS
Demande au serveur quelles méthodes sont autorisées sur une ressource. Utilisé notamment pour CORS (Cross-Origin Resource Sharing).

```
OPTIONS /api/users HTTP/1.1
→ Réponse : Allow: GET, POST, PUT, DELETE
```

## Choisir la Bonne Méthode

Voici un guide de décision rapide :

```
Que voulez-vous faire ?
│
├─ Consulter des données ?
│  └─→ GET
│
├─ Créer quelque chose de nouveau ?
│  └─→ POST
│
├─ Modifier quelque chose qui existe ?
│  ├─ Remplacement complet ?
│  │  └─→ PUT
│  └─ Modification partielle ?
│     └─→ PATCH
│
└─ Supprimer quelque chose ?
   └─→ DELETE
```

## Exemple Concret : Gestion d'une Bibliothèque

Imaginons une API pour gérer des livres dans une bibliothèque :

```
# Consulter tous les livres
GET /api/books

# Consulter un livre spécifique
GET /api/books/123

# Rechercher des livres
GET /api/books?author=Hugo&available=true

# Ajouter un nouveau livre
POST /api/books  
Body: { "title": "Les Misérables", "author": "Victor Hugo", ... }  

# Modifier complètement un livre
PUT /api/books/123  
Body: { version complète du livre avec modifications }  

# Modifier seulement la disponibilité
PATCH /api/books/123  
Body: { "available": false }  

# Supprimer un livre
DELETE /api/books/123
```

## Points Clés à Retenir

1. **GET** = Lire (ne modifie jamais rien)
2. **POST** = Créer (ajoute quelque chose de nouveau)
3. **PUT** = Remplacer complètement (envoyer la version complète)
4. **DELETE** = Supprimer (retirer définitivement)

5. **Idempotence** : GET, PUT, DELETE peuvent être répétés sans danger. POST ne l'est pas.

6. **Sécurité** : Seul GET est "sûr" (lecture seule).

7. **Codes de statut** :
   - 2xx = Succès
   - 4xx = Erreur client (mauvaise requête)
   - 5xx = Erreur serveur

## Ce Qu'il Faut Retenir pour la Suite

Maintenant que vous connaissez les méthodes HTTP, vous êtes prêt à :
- Comprendre comment fonctionnent les **API REST**
- Utiliser **TFPHttpClient** pour envoyer des requêtes GET, POST, PUT et DELETE
- Manipuler des données distantes depuis vos applications FreePascal
- Construire des clients d'API robustes

Dans les prochaines sections, nous allons mettre en pratique ces méthodes avec du code FreePascal concret !

⏭️ [Introduction aux API REST](/17-communications-reseau-api-rest/03-introduction-api-rest.md)
