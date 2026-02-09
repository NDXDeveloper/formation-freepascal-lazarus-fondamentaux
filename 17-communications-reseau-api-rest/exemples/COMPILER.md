# Chapitre 17 : Communications Réseau et API REST — Compilation des exemples

## Prérequis
- Free Pascal Compiler (fpc) version 3.2.2 ou supérieure
- OpenSSL installé (`apt install openssl libssl-dev` sur Debian/Ubuntu)
- Connexion Internet pour les exemples réseau

## Compilation individuelle

### Unités (compiler en premier)
```bash
fpc JSONHelper.pas
fpc APIHelper.pas
fpc HTTPManager.pas
fpc AuthManager.pas
```

### Programmes
```bash
fpc 05-get-simple.pas
fpc 05-get-gestion-erreurs.pas
fpc 05-client-api-rest.pas
fpc 06-jsonplaceholder.pas
fpc 06-meteo.pas
fpc 06-utilisation-apihelper.pas
fpc 07-parsing-simple.pas
fpc 07-extraction-objet.pas
fpc 07-parsing-tableau.pas
fpc 07-json-imbrique.pas
fpc 07-erreurs-parsing.pas
fpc 07-creation-json.pas
fpc 07-utilisation-jsonhelper.pas
fpc 07-parse-utilisateurs.pas
fpc 08-utilisation-httpmanager.pas
fpc 09-auth-basique.pas
fpc 09-utilisation-authmanager.pas
```

## Compilation de tous les exemples

```bash
# Compiler les unités d'abord
for f in JSONHelper.pas APIHelper.pas HTTPManager.pas AuthManager.pas; do
  echo "=== $f ===" && fpc "$f"
done

# Puis les programmes
for f in 0*.pas; do echo "=== $f ===" && fpc "$f"; done
```

## Nettoyage

```bash
rm -f *.o *.ppu
rm -f 05-get-simple 05-get-gestion-erreurs 05-client-api-rest
rm -f 06-jsonplaceholder 06-meteo 06-utilisation-apihelper
rm -f 07-parsing-simple 07-extraction-objet 07-parsing-tableau
rm -f 07-json-imbrique 07-erreurs-parsing 07-creation-json
rm -f 07-utilisation-jsonhelper 07-parse-utilisateurs
rm -f 08-utilisation-httpmanager
rm -f 09-auth-basique 09-utilisation-authmanager
rm -f network_errors.log
```

## Liste des exemples (21 fichiers)

### Section 17.5 — Utilisation de TFPHttpClient (3 programmes)

| Fichier | Description |
|---------|-------------|
| `05-get-simple.pas` | Requête GET simple vers GitHub API |
| `05-get-gestion-erreurs.pas` | Requête GET avec gestion des erreurs try-except |
| `05-client-api-rest.pas` | Client REST complet avec CRUD (GET, POST, PUT, DELETE) |

### Section 17.6 — Consommation d'API publiques (3 programmes + 1 unité)

| Fichier | Description |
|---------|-------------|
| `06-jsonplaceholder.pas` | Récupération des utilisateurs via JSONPlaceholder |
| `06-meteo.pas` | Application météo OpenWeatherMap (nécessite clé API) |
| `APIHelper.pas` | Unité helper réutilisable pour consommer des API REST |
| `06-utilisation-apihelper.pas` | Utilisation de l'unité APIHelper |

### Section 17.7 — Parsing JSON avec fpjson (8 programmes + 1 unité)

| Fichier | Description |
|---------|-------------|
| `07-parsing-simple.pas` | Parsing JSON simple avec GetJSON |
| `07-extraction-objet.pas` | Extraction de valeurs depuis un objet JSON |
| `07-parsing-tableau.pas` | Parsing d'un tableau JSON |
| `07-json-imbrique.pas` | Parsing de JSON imbriqué (objets dans des objets) |
| `07-erreurs-parsing.pas` | Gestion des erreurs de parsing JSON |
| `07-creation-json.pas` | Création d'objets JSON avec fpjson |
| `JSONHelper.pas` | Unité helper pour simplifier le parsing JSON |
| `07-utilisation-jsonhelper.pas` | Utilisation de l'unité JSONHelper |
| `07-parse-utilisateurs.pas` | Parser la liste des utilisateurs de JSONPlaceholder |

### Section 17.8 — Gestion des erreurs réseau (1 programme + 1 unité)

| Fichier | Description |
|---------|-------------|
| `HTTPManager.pas` | Classe complète de gestion HTTP avec retry et logging |
| `08-utilisation-httpmanager.pas` | Utilisation de la classe HTTPManager |

### Section 17.9 — Headers et authentification basique (2 programmes + 1 unité)

| Fichier | Description |
|---------|-------------|
| `09-auth-basique.pas` | Authentification HTTP basique (Basic Auth) manuelle |
| `AuthManager.pas` | Unité de gestion des différents types d'authentification |
| `09-utilisation-authmanager.pas` | Utilisation de la classe AuthManager |

## Notes

### Programmes locaux (sans réseau)
Les programmes suivants fonctionnent sans connexion Internet (parsing JSON local) :
- `07-parsing-simple.pas`
- `07-extraction-objet.pas`
- `07-parsing-tableau.pas`
- `07-json-imbrique.pas`
- `07-erreurs-parsing.pas`
- `07-creation-json.pas`
- `07-utilisation-jsonhelper.pas`

### Programmes réseau
Les autres programmes nécessitent une connexion Internet et OpenSSL :
- **JSONPlaceholder** (`jsonplaceholder.typicode.com`) : API de test gratuite, sans authentification
- **GitHub API** (`api.github.com`) : nécessite un header User-Agent
- **httpbin.org** : service de test pour l'authentification HTTP
- **OpenWeatherMap** (`06-meteo.pas`) : nécessite une clé API gratuite (inscription sur openweathermap.org)

### Sections sans exemples compilables
- **17.1** Concepts fondamentaux du protocole HTTP (théorie pure)
- **17.2** Méthodes HTTP GET, POST, PUT, DELETE (théorie pure)
- **17.3** Introduction aux API REST (théorie pure)
- **17.4** Format JSON : structure et syntaxe (théorie + fragments)

### Corrections par rapport au .md
- `05-get-simple.pas` : ajout du header `User-Agent` car GitHub API renvoie 403 sans lui
- `05-get-gestion-erreurs.pas` : URL changée de `api.example.com` (fictive) vers `jsonplaceholder.typicode.com` (fonctionnelle)
- `08-utilisation-httpmanager.pas` : URL changée vers `jsonplaceholder.typicode.com`, variable `Result` renommée `Res` pour clarté
- `09-auth-basique.pas` : URL ajustée pour correspondre aux credentials utilisés (`httpbin.org/basic-auth/jean.dupont/monMotDePasse123`)
- `HTTPManager.pas` : `ShouldRetry` utilise `case` au lieu de `in [...]` (codes > 255 incompatibles avec les ensembles Pascal)
- Tous les `ReadLn` de fin de programme retirés (programmes non-interactifs)

### Parser JSON de FPC
Le parser JSON de FPC (`fpjson/jsonparser`) est plus tolérant que le standard JSON strict :
- Accepte les guillemets simples pour les chaînes
- Accepte les clés sans guillemets
- Cela explique pourquoi `07-erreurs-parsing.pas` affiche "JSON valide !" pour certains tests qui devraient être invalides selon le standard
