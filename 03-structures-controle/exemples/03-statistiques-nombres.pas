{ ============================================================================
  Section 3.3 : Boucles compteur (for-do)
  Description : Calcul de statistiques (somme, moyenne, min, max) sur des nombres saisis
  Fichier source : 03-boucles-compteur-for-do.md
  ============================================================================ }

program StatistiquesNombres;  
var  
  i, n, nombre: Integer;
  somme, minimum, maximum: Integer;
  moyenne: Real;
begin
  Write('Combien de nombres voulez-vous entrer ? ');
  ReadLn(n);
  WriteLn;
  Write('Nombre 1 : ');
  ReadLn(nombre);
  somme := nombre;
  minimum := nombre;
  maximum := nombre;
  for i := 2 to n do
  begin
    Write('Nombre ', i, ' : ');
    ReadLn(nombre);
    somme := somme + nombre;
    if nombre < minimum then
      minimum := nombre;
    if nombre > maximum then
      maximum := nombre;
  end;
  { / donne un resultat Real (division reelle) ; div donnerait un Integer }
  moyenne := somme / n;
  WriteLn;
  WriteLn('=== STATISTIQUES ===');
  WriteLn('Somme : ', somme);
  WriteLn('Moyenne : ', moyenne:0:2);  { :0:2 = pas de largeur min, 2 decimales }
  WriteLn('Minimum : ', minimum);
  WriteLn('Maximum : ', maximum);
end.
