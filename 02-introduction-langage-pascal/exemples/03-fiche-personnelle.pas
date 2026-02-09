{ ============================================================================
  Section 2.3 : Variables et constantes
  Description : Fiche personnelle avec calcul d'age a partir de l'annee
                de naissance
  Fichier source : 03-variables-constantes.md
  ============================================================================ }
program FichePersonnelle;

const
  AnneeEnCours = 2024;

var
  prenom: string;
  nom: string;
  anneeNaissance: integer;
  age: integer;
  ville: string;

begin
  // Affectation des valeurs
  prenom := 'Marie';
  nom := 'Dupont';
  anneeNaissance := 1990;
  ville := 'Paris';

  // Calcul de l'âge
  age := AnneeEnCours - anneeNaissance;

  // Affichage de la fiche
  writeln('=== FICHE PERSONNELLE ===');
  writeln('Nom : ', nom);
  writeln('Prénom : ', prenom);
  writeln('Année de naissance : ', anneeNaissance);
  writeln('Âge : ', age, ' ans');
  writeln('Ville : ', ville);
end.
