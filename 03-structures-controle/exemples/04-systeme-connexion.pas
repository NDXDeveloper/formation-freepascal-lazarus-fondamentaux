{ ============================================================================
  Section 3.4 : Boucles conditionnelles (while-do, repeat-until)
  Description : Systeme de connexion avec mot de passe et 3 tentatives max
  Fichier source : 04-boucles-conditionnelles-while-repeat.md
  ============================================================================ }

program SystemeConnexion;
const
  MAX_TENTATIVES = 3;
  MOT_DE_PASSE_CORRECT = 'pascal2024';
var
  motDePasse: String;
  tentatives: Integer;
  connecte: Boolean;
begin
  tentatives := 0;
  connecte := False;
  WriteLn('=== CONNEXION ===');
  WriteLn;
  { not connecte : l'operateur not inverse un Boolean (True -> False, False -> True) }
  while (tentatives < MAX_TENTATIVES) and (not connecte) do
  begin
    tentatives := tentatives + 1;
    Write('Mot de passe (tentative ', tentatives, '/', MAX_TENTATIVES, ') : ');
    ReadLn(motDePasse);
    if motDePasse = MOT_DE_PASSE_CORRECT then
    begin
      connecte := True;
      WriteLn;
      WriteLn('Connexion réussie !');
      WriteLn('Bienvenue dans le système.');
    end
    else
    begin
      if tentatives < MAX_TENTATIVES then
        WriteLn('Mot de passe incorrect. Il vous reste ',
                MAX_TENTATIVES - tentatives, ' tentative(s).')
      else
        WriteLn('Accès refusé. Nombre maximum de tentatives atteint.');
    end;
    WriteLn;
  end;
end.
