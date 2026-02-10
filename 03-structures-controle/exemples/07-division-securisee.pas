{ ============================================================================
  Section 3.7 : Gestion des erreurs simples
  Description : Division securisee avec verification du diviseur
  Fichier source : 07-gestion-erreurs-simples.md
  ============================================================================ }
program DivisionSecurisee;  
var  
  a, b: Real;
  resultat: Real;
begin
  Write('Dividende : ');
  ReadLn(a);
  Write('Diviseur : ');
  ReadLn(b);

  if b = 0 then
  begin
    WriteLn('═══════════════════════════');
    WriteLn('   ERREUR : Division par 0');
    WriteLn('═══════════════════════════');
    WriteLn('Le diviseur ne peut pas être zéro.');
  end
  else
  begin
    resultat := a / b;
    WriteLn('Résultat : ', a:0:2, ' / ', b:0:2, ' = ', resultat:0:2);
  end;
end.
