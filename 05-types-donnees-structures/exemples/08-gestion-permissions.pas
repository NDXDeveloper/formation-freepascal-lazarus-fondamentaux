{ ============================================================================
  Section 5.8 : Types ensemble (Set)
  Description : Gestion des permissions avec ensembles
  Fichier source : 08-types-ensemble-set.md
  ============================================================================ }
program GestionPermissions;  
type  
  TPermission = (Lecture, Ecriture, Execution, Suppression);
  TPermissions = set of TPermission;

var
  admin, utilisateur, invite: TPermissions;

procedure AfficherPermissions(perms: TPermissions; nom: String);  
begin  
  WriteLn('Permissions de ', nom, ' :');
  if Lecture in perms then WriteLn('  - Lecture');
  if Ecriture in perms then WriteLn('  - Écriture');
  if Execution in perms then WriteLn('  - Exécution');
  if Suppression in perms then WriteLn('  - Suppression');
  if perms = [] then WriteLn('  - Aucune permission');
  WriteLn;
end;

function PeutModifier(perms: TPermissions): Boolean;  
begin  
  // Peut modifier si a lecture ET écriture
  PeutModifier := [Lecture, Ecriture] <= perms;
end;

begin
  // Définir les permissions par rôle
  admin := [Lecture, Ecriture, Execution, Suppression];
  utilisateur := [Lecture, Ecriture];
  invite := [Lecture];

  AfficherPermissions(admin, 'Administrateur');
  AfficherPermissions(utilisateur, 'Utilisateur');
  AfficherPermissions(invite, 'Invité');

  // Tests
  if PeutModifier(admin) then
    WriteLn('Admin peut modifier les fichiers');

  if PeutModifier(utilisateur) then
    WriteLn('Utilisateur peut modifier les fichiers');

  if not PeutModifier(invite) then
    WriteLn('Invité ne peut PAS modifier les fichiers');
end.
