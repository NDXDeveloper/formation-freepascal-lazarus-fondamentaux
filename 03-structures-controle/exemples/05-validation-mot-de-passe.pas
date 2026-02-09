{ ============================================================================
  Section 3.5 : Instructions break et continue
  Description : Validation de mot de passe avec nombre limite de tentatives
  Fichier source : 05-instructions-break-continue.md
  ============================================================================ }
program ValidationMotDePasse;
const
  MAX_TENTATIVES = 3;
  MOT_DE_PASSE = 'secret123';
var
  tentative: Integer;
  saisie: String;
  succes: Boolean;
begin
  succes := False;

  WriteLn('=== AUTHENTIFICATION ===');

  for tentative := 1 to MAX_TENTATIVES do
  begin
    Write('Tentative ', tentative, '/', MAX_TENTATIVES, ' - Mot de passe : ');
    ReadLn(saisie);

    if saisie = MOT_DE_PASSE then
    begin
      WriteLn('Authentification réussie !');
      succes := True;
      break;  // Plus besoin de continuer
    end
    else
    begin
      if tentative < MAX_TENTATIVES then
        WriteLn('Incorrect. Il reste ', MAX_TENTATIVES - tentative, ' tentative(s)')
      else
        WriteLn('Accès refusé. Nombre maximum de tentatives atteint.');
    end;
  end;

  if succes then
    WriteLn('Bienvenue dans le système !');
end.
