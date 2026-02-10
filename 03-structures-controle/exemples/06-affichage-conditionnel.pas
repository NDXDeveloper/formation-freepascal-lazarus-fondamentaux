{ ============================================================================
  Section 3.6 : Imbrication de structures
  Description : Boucle FOR dans IF - affichage detaille ou simple selon le choix
  Fichier source : 06-imbrication-structures.md
  ============================================================================ }
program AffichageConditionnel;  
var  
  i, n: Integer;
  afficher: Boolean;
  reponse: String;
begin
  Write('Combien de nombres voulez-vous ? ');
  ReadLn(n);
  Write('Afficher les détails ? (true/false) : ');
  ReadLn(reponse);
  afficher := (reponse = 'true');
  WriteLn;

  if afficher then
  begin
    WriteLn('=== AFFICHAGE DÉTAILLÉ ===');
    for i := 1 to n do
      WriteLn('Nombre ', i, ' : carré = ', i*i, ', cube = ', i*i*i);
  end
  else
  begin
    WriteLn('=== AFFICHAGE SIMPLE ===');
    for i := 1 to n do
      Write(i, ' ');
    WriteLn;
  end;
end.
