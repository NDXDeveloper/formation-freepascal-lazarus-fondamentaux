{ ============================================================================
  Section 3.4 : Boucles conditionnelles (while-do, repeat-until)
  Description : Calcul de moyenne avec valeur sentinelle (0 pour terminer)
  Fichier source : 04-boucles-conditionnelles-while-repeat.md
  ============================================================================ }

program CalculMoyenneSentinelle;
var
  nombre: Real;
  somme: Real;
  compteur: Integer;
  moyenne: Real;
begin
  somme := 0;
  compteur := 0;
  WriteLn('=== CALCUL DE MOYENNE ===');
  WriteLn('Entrez des nombres (0 pour terminer)');
  WriteLn;
  repeat
    Write('Nombre ', compteur + 1, ' : ');
    ReadLn(nombre);
    if nombre <> 0 then
    begin
      somme := somme + nombre;
      compteur := compteur + 1;
    end;
  until nombre = 0;
  WriteLn;
  if compteur > 0 then
  begin
    moyenne := somme / compteur;
    WriteLn('Nombre de valeurs : ', compteur);
    WriteLn('Somme : ', somme:0:2);
    WriteLn('Moyenne : ', moyenne:0:2);
  end
  else
    WriteLn('Aucune valeur saisie.');
end.
