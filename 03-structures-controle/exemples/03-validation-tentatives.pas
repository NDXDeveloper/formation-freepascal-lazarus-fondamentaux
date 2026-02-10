{ ============================================================================
  Section 3.3 : Boucles compteur (for-do)
  Description : Validation de code avec nombre limite de tentatives et Break
  Fichier source : 03-boucles-compteur-for-do.md
  ============================================================================ }

program ValidationAvecTentatives;  
var  
  i, nombre: Integer;
  correct: Boolean;
const
  MOT_DE_PASSE = 1234;
  MAX_TENTATIVES = 3;
begin
  correct := false;
  WriteLn('=== SYSTÈME DE SÉCURITÉ ===');
  WriteLn('Vous avez ', MAX_TENTATIVES, ' tentatives.');
  WriteLn;
  for i := 1 to MAX_TENTATIVES do
  begin
    Write('Tentative ', i, ' - Entrez le code : ');
    ReadLn(nombre);
    if nombre = MOT_DE_PASSE then
    begin
      correct := true;
      WriteLn('Code correct ! Accès autorisé.');
      Break;  { Break sort immediatement de la boucle for englobante }
    end
    else
    begin
      if i < MAX_TENTATIVES then
        WriteLn('Code incorrect. Il vous reste ', MAX_TENTATIVES - i, ' tentative(s).')
      else
        WriteLn('Code incorrect. Accès bloqué !');
    end;
    WriteLn;
  end;
end.
