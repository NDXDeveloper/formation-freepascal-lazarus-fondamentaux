{ ============================================================================
  Section 3.7 : Gestion des erreurs simples
  Description : Racine carree securisee avec verification du nombre negatif
  Fichier source : 07-gestion-erreurs-simples.md
  ============================================================================ }
program RacineCarreeSecurisee;
var
  nombre, racine: Real;
begin
  Write('Entrez un nombre : ');
  ReadLn(nombre);

  if nombre < 0 then
  begin
    WriteLn('ERREUR : Impossible de calculer la racine carrée d''un nombre négatif.');
    WriteLn('Conseil : Utilisez un nombre positif ou nul.');
  end
  else
  begin
    racine := Sqrt(nombre);
    WriteLn('La racine carrée de ', nombre:0:2, ' est ', racine:0:4);
  end;
end.
