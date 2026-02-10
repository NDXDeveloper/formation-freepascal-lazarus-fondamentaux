{ ============================================================================
  Section 6.4 : Pointeurs et Tableaux
  Description : Redimensionnement dynamique d'un tableau avec SetLength
  Fichier source : 04-pointeurs-tableaux.md
  ============================================================================ }
program Redimensionnement;  
var  
  data: array of Integer;
  ancienneTaille, nouvelleTaille: Integer;
begin
  // Démarrer avec 5 éléments
  SetLength(data, 5);
  data[0] := 10;
  data[4] := 50;

  ancienneTaille := Length(data);
  WriteLn('Ancienne taille : ', ancienneTaille);

  // Agrandir à 10 (garde les anciennes valeurs)
  nouvelleTaille := 10;
  SetLength(data, nouvelleTaille);

  WriteLn('Nouvelle taille : ', Length(data));
  WriteLn('Premier élément conservé : ', data[0]);  // 10
  WriteLn('Dernier ancien conservé : ', data[4]);   // 50
end.
