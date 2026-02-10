# Chapitre 13 : Gestion des exceptions — Compilation des exemples

## Prérequis
- Free Pascal Compiler (fpc) version 3.2.2 ou supérieure

## Compilation individuelle

```bash
fpc 02-try-except-finally.pas  
fpc 03-raise-validation.pas  
fpc 04-hierarchie-exceptions.pas  
fpc 05-exception-personnalisee.pas  
fpc 05-exception-avec-proprietes.pas  
fpc 06-exceptions-ressources.pas  
fpc 07-bonnes-pratiques-demo.pas  
fpc 08-debogage-exceptions.pas  
fpc 09-logging-erreurs.pas  
```

## Compilation de tous les exemples

```bash
for f in *.pas; do echo "=== $f ===" && fpc "$f"; done
```

## Nettoyage

```bash
rm -f *.o  
rm -f 02-try-except-finally 03-raise-validation 04-hierarchie-exceptions  
rm -f 05-exception-personnalisee 05-exception-avec-proprietes  
rm -f 06-exceptions-ressources 07-bonnes-pratiques-demo  
rm -f 08-debogage-exceptions 09-logging-erreurs  
```

## Liste des exemples (9 fichiers)

| Fichier | Section | Description |
|---------|---------|-------------|
| `02-try-except-finally.pas` | 13.2 | Try-except basique, types multiples, try-finally, propagation |
| `03-raise-validation.pas` | 13.3 | raise Exception.Create, CreateFmt, re-lever, validation |
| `04-hierarchie-exceptions.pas` | 13.4 | ClassName, Message, ordre de capture, types standards, test is |
| `05-exception-personnalisee.pas` | 13.5 | Exception personnalisée EAgeInvalide avec validation |
| `05-exception-avec-proprietes.pas` | 13.5 | Exceptions avec propriétés, code d'erreur, hiérarchie métier |
| `06-exceptions-ressources.pas` | 13.6 | try-finally fichier, objet, multiples ressources, FreeAndNil |
| `07-bonnes-pratiques-demo.pas` | 13.7 | Messages contextuels, validation précoce, ne pas ignorer, re-lever |
| `08-debogage-exceptions.pas` | 13.8 | Debug conversion, Assert, test d'exceptions, backtrace |
| `09-logging-erreurs.pas` | 13.9 | Logging simple dans fichier avec niveaux, LogException, workflow |

## Notes

- Tous les programmes sont non-interactifs (pas de ReadLn)
- La section 13.1 est purement conceptuelle (pas de programme)
- La section 13.5 a deux fichiers : un simple (EAgeInvalide) et un avancé (propriétés, codes, hiérarchie)
- Le fichier `09-logging-erreurs.pas` crée et supprime un fichier log temporaire pendant l'exécution
