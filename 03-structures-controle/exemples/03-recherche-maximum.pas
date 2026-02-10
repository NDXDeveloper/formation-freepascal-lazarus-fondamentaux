{ ============================================================================
  Section 3.3 : Boucles compteur (for-do)
  Description : Recherche du maximum parmi 5 nombres saisis
  Fichier source : 03-boucles-compteur-for-do.md
  ============================================================================ }

program RechercheMaximum;  
var  
  i, nombre, maximum: Integer;
begin
  WriteLn('Entrez 5 nombres :');
  Write('Nombre 1 : ');
  ReadLn(maximum);
  for i := 2 to 5 do
  begin
    Write('Nombre ', i, ' : ');
    ReadLn(nombre);
    if nombre > maximum then
      maximum := nombre;
  end;
  WriteLn;
  WriteLn('Le plus grand nombre est : ', maximum);
end.
