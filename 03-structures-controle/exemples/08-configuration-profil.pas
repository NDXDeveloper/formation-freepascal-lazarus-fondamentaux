{ ============================================================================
  Section 3.8 : Validation des entrees utilisateur
  Description : Configuration de profil utilisateur - pseudo, avatar et notifications
  Fichier source : 08-validation-entrees-utilisateur.md
  ============================================================================ }
program ConfigurationProfil;
var
  pseudo: String;
  avatar: Integer;
  notifications: Char;
  i: Integer;
  valide: Boolean;
begin
  WriteLn('═══════════════════════════════');
  WriteLn('   CONFIGURATION DU PROFIL');
  WriteLn('═══════════════════════════════');
  WriteLn;

  // Pseudo
  repeat
    Write('Pseudo (3-15 caractères, lettres et chiffres uniquement) : ');
    ReadLn(pseudo);
    valide := True;

    if (Length(pseudo) < 3) or (Length(pseudo) > 15) then
    begin
      WriteLn('❌ Le pseudo doit contenir entre 3 et 15 caractères');
      valide := False;
    end
    else
    begin
      for i := 1 to Length(pseudo) do
      begin
        if not (pseudo[i] in ['A'..'Z', 'a'..'z', '0'..'9']) then
        begin
          WriteLn('❌ Le pseudo ne doit contenir que des lettres et chiffres');
          valide := False;
          break;
        end;
      end;
    end;
  until valide;

  // Choix d'avatar
  WriteLn;
  WriteLn('Avatars disponibles :');
  WriteLn('1. 😊 Souriant');
  WriteLn('2. 😎 Cool');
  WriteLn('3. 🤓 Geek');
  WriteLn('4. 🐱 Chat');
  WriteLn('5. 🦊 Renard');
  WriteLn;

  repeat
    Write('Choisissez votre avatar (1-5) : ');
    ReadLn(avatar);

    if (avatar < 1) or (avatar > 5) then
      WriteLn('❌ Veuillez choisir entre 1 et 5');
  until (avatar >= 1) and (avatar <= 5);

  // Notifications
  WriteLn;
  repeat
    Write('Activer les notifications ? (O/N) : ');
    ReadLn(notifications);
    notifications := UpCase(notifications);

    if not (notifications in ['O', 'N']) then
      WriteLn('❌ Répondez par O (Oui) ou N (Non)');
  until notifications in ['O', 'N'];

  // Récapitulatif
  WriteLn;
  WriteLn('═══════════════════════════════');
  WriteLn('   PROFIL CONFIGURÉ');
  WriteLn('═══════════════════════════════');
  WriteLn('Pseudo : ', pseudo);
  Write('Avatar : ');
  case avatar of
    1: WriteLn('😊 Souriant');
    2: WriteLn('😎 Cool');
    3: WriteLn('🤓 Geek');
    4: WriteLn('🐱 Chat');
    5: WriteLn('🦊 Renard');
  end;
  Write('Notifications : ');
  if notifications = 'O' then
    WriteLn('Activées')
  else
    WriteLn('Désactivées');
  WriteLn('═══════════════════════════════');
  WriteLn('✓ Configuration enregistrée !');
end.
