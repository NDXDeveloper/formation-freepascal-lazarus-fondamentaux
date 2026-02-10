{ ============================================================================
  Section 3.5 : Instructions break et continue
  Description : Filtrage de donnees avec continue pour ignorer les negatifs
  Fichier source : 05-instructions-break-continue.md
  ============================================================================ }
program FiltrageNombres;  
var  
  i, nombre: Integer;
  somme, compteur: Integer;
begin
  somme := 0;
  compteur := 0;

  WriteLn('Entrez 10 nombres (négatifs ignorés) :');

  for i := 1 to 10 do
  begin
    Write('Nombre ', i, ' : ');
    ReadLn(nombre);

    if nombre < 0 then
    begin
      WriteLn('→ Nombre négatif ignoré');
      continue;  // Passe au nombre suivant
    end;

    somme := somme + nombre;
    compteur := compteur + 1;
    WriteLn('→ Nombre ajouté');
  end;

  WriteLn;
  WriteLn('Nombres positifs entrés : ', compteur);
  if compteur > 0 then
    WriteLn('Somme : ', somme)
  else
    WriteLn('Aucun nombre positif');
end.
