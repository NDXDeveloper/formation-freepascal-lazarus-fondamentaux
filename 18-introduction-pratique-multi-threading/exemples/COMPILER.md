# Chapitre 18 : Introduction Pratique au Multi-threading — Exemples

## Programmes console (3 fichiers)

Compilation avec `fpc` :

```bash
fpc 03-tthread-creation.pas
fpc 04-cycle-vie-thread.pas
fpc 07-sections-critiques.pas
```

| Fichier | Section | Description |
|---------|---------|-------------|
| `03-tthread-creation.pas` | 18.3 | TThread basique : Create, Execute, FreeOnTerminate, WaitFor, constructeur personnalise, Terminated |
| `04-cycle-vie-thread.pas` | 18.4 | Cycle de vie : etats (cree, suspendu, running, termine, libere), deux strategies de liberation, try-finally |
| `07-sections-critiques.pas` | 18.7 | Race condition, TCriticalSection, InterlockedIncrement, liste partagee, producteur/consommateur |

## Projets Lazarus (5 sous-dossiers, 20 fichiers)

Construction avec `lazbuild` :

```bash
lazbuild 01-gel-interface/GelInterface.lpi
lazbuild 05-synchronize-demo/SynchronizeDemo.lpi
lazbuild 06-queue-vs-synchronize/QueueVsSync.lpi
lazbuild 08-barres-progression/BarresProgression.lpi
lazbuild 09-annulation/AnnulationDemo.lpi
```

| Sous-dossier | Section | Description |
|--------------|---------|-------------|
| `01-gel-interface/` | 18.1 | Gel GUI vs thread : calcul bloquant vs calcul en thread, test de reactivite |
| `05-synchronize-demo/` | 18.5 | Synchronize : mise a jour ProgressBar, Label et Memo depuis un thread |
| `06-queue-vs-synchronize/` | 18.6 | Queue vs Synchronize : comparaison visuelle avec deux Memos cote a cote |
| `08-barres-progression/` | 18.8 | Progression multi-niveaux (globale + etape), estimation temps restant |
| `09-annulation/` | 18.9 | Boutons Start/Annuler, Terminated, nettoyage dans finally, OnTerminate |

Chaque sous-dossier contient 4 fichiers :
- `.lpi` : fichier projet Lazarus
- `.lpr` : programme principal (point d'entree)
- `unit1.pas` : unite du formulaire (classes TThread + TForm)
- `unit1.lfm` : description visuelle du formulaire

## Sections sans exemple

| Section | Raison |
|---------|--------|
| 18.2 | Theorie pure (processus vs threads), aucun code executable |

## Nettoyage

```bash
# Console
rm -f *.o 03-tthread-creation 04-cycle-vie-thread 07-sections-critiques

# Lazarus
rm -rf 01-gel-interface/lib 01-gel-interface/GelInterface
rm -rf 05-synchronize-demo/lib 05-synchronize-demo/SynchronizeDemo
rm -rf 06-queue-vs-synchronize/lib 06-queue-vs-synchronize/QueueVsSync
rm -rf 08-barres-progression/lib 08-barres-progression/BarresProgression
rm -rf 09-annulation/lib 09-annulation/AnnulationDemo
```

## Notes techniques

- **cthreads** : tous les programmes/projets incluent `{$IFDEF UNIX}cthreads{$ENDIF}` (obligatoire sur Linux)
- **TCriticalSection** : dans l'unite `SyncObjs`
- **InterlockedIncrement** : dans `System` (pas de uses necessaire)
- **Procedures anonymes** : `Synchronize(@procedure begin ... end)` ne compile pas en FPC 3.2.2 `{$mode objfpc}` — utiliser des methodes nommees a la place
