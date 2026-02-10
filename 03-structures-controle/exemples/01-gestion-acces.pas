{ ============================================================================
  Section 3.1 : Instructions conditionnelles (if-then-else)
  Description : Exercice pratique - système de gestion d'accès avec conditions imbriquées
  Fichier source : 01-instructions-conditionnelles-if-then-else.md
  ============================================================================ }
program GestionAcces;  
var  
  age: Integer;
  membre: Boolean;
  solde: Real;
  acces: Boolean;
  reponse: String;
begin
  WriteLn('=== Système de gestion d''accès ===');
  WriteLn;
  Write('Entrez votre âge : ');
  ReadLn(age);
  Write('Êtes-vous membre ? (true/false) : ');
  ReadLn(reponse);
  membre := (reponse = 'true');
  Write('Solde du compte : ');
  ReadLn(solde);
  WriteLn;
  WriteLn('--- Analyse ---');
  if age < 18 then
  begin
    WriteLn('Vous êtes mineur.');
    WriteLn('Accès refusé pour les mineurs.');
    acces := false;
  end
  else
  begin
    WriteLn('Vous êtes majeur.');
    if membre then
    begin
      WriteLn('Vous êtes membre.');
      if solde >= 0 then
      begin
        WriteLn('Votre compte est créditeur.');
        acces := true;
      end
      else
      begin
        WriteLn('Votre compte est débiteur.');
        acces := false;
      end;
    end
    else
    begin
      WriteLn('Vous n''êtes pas membre.');
      acces := false;
    end;
  end;
  WriteLn;
  WriteLn('--- Résultat ---');
  if acces then
    WriteLn('ACCÈS AUTORISÉ')
  else
    WriteLn('ACCÈS REFUSÉ');
end.
