{ ============================================================================
  Section 4.6 : Parametres par defaut
  Description : Systeme de notification avec parametres optionnels
  Fichier source : 06-parametres-par-defaut.md
  ============================================================================ }
{$mode objfpc}{$H+}
program SystemeNotification;

type
  TNiveauUrgence = (Basse, Normale, Haute, Critique);  // Type énuméré : liste de valeurs nommées

procedure EnvoyerNotification(
  const destinataire: String;
  const message: String;
  urgence: TNiveauUrgence = Normale;
  afficherTimestamp: Boolean = True;
  sonore: Boolean = False
);
var
  urgenceTexte: String;
begin
  // Déterminer le texte d'urgence
  case urgence of
    Basse:     urgenceTexte := 'INFO';
    Normale:   urgenceTexte := 'NORMAL';
    Haute:     urgenceTexte := 'IMPORTANT';
    Critique:  urgenceTexte := 'CRITIQUE';
  end;

  // Afficher timestamp si demandé
  if afficherTimestamp then
    Write('[2025-10-12 14:30] ');

  // Afficher la notification
  WriteLn('À: ', destinataire);
  WriteLn('Niveau: ', urgenceTexte);
  WriteLn('Message: ', message);

  // Son si demandé
  if sonore then
    WriteLn('*BEEP*');

  WriteLn('---');
end;

begin
  // Notification simple
  EnvoyerNotification('admin@example.com', 'Système démarré');

  // Notification importante
  EnvoyerNotification('admin@example.com', 'Mise à jour disponible', Haute);

  // Notification critique avec son
  EnvoyerNotification('admin@example.com', 'Erreur système !', Critique, True, True);

  // Sans timestamp
  EnvoyerNotification('user@example.com', 'Bienvenue', Basse, False);
end.
