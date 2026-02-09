{ ============================================================================
  Section 5.7 : Types enumeres
  Description : Fonction Pred() pour obtenir la valeur precedente
  Fichier source : 07-types-enumeres.md
  ============================================================================ }
program FonctionPred;
type
  TJourSemaine = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);

var
  jour: TJourSemaine;
begin
  jour := Jeudi;
  WriteLn('Aujourd''hui : Jeudi');

  jour := Pred(jour);
  WriteLn('Hier c''était : Mercredi (valeur précédente)');

  // Attention : Pred du premier élément provoque une erreur !
  // jour := Lundi;
  // jour := Pred(jour);  // ERREUR à l'exécution !
end.
