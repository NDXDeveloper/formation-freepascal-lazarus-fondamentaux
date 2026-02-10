{ ============================================================================
  Section 5.7 : Types enumeres
  Description : Fonction Succ() pour obtenir la valeur suivante
  Fichier source : 07-types-enumeres.md
  ============================================================================ }
program FonctionSucc;  
type  
  TJourSemaine = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);

var
  jour: TJourSemaine;
begin
  jour := Mardi;
  WriteLn('Aujourd''hui : Mardi');

  jour := Succ(jour);
  WriteLn('Demain ce sera : Mercredi (valeur suivante)');

  // Attention : Succ du dernier élément provoque une erreur !
  // jour := Dimanche;
  // jour := Succ(jour);  // ERREUR à l'exécution !
end.
