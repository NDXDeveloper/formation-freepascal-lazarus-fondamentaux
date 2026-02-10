{ ============================================================================
  Section 2.6 : Entrees/sorties console
  Description : Collecte d'informations personnelles avec ReadLn et
                affichage d'un recapitulatif formate
  Fichier source : 06-entrees-sorties-console.md
  ============================================================================ }
program FicheIdentite;  
var  
  nom, prenom, ville: string;
  age: integer;
  taille: real;
begin
  // En Pascal, '' (deux apostrophes) insere une apostrophe litterale dans une chaine
  WriteLn('=== FICHE D''IDENTITÉ ===');
  WriteLn;

  Write('Prénom : ');
  ReadLn(prenom);

  Write('Nom : ');
  ReadLn(nom);

  Write('Âge : ');
  ReadLn(age);

  Write('Taille (en m) : ');
  ReadLn(taille);

  Write('Ville : ');
  ReadLn(ville);

  WriteLn;
  WriteLn('========================');
  WriteLn('RÉCAPITULATIF');
  WriteLn('========================');
  WriteLn('Prénom      : ', prenom);
  WriteLn('Nom         : ', nom);
  WriteLn('Âge         : ', age, ' ans');
  WriteLn('Taille      : ', taille:0:2, ' m');
  WriteLn('Ville       : ', ville);
  WriteLn('========================');

  WriteLn;
  Write('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
