{ ============================================================================
  Section 4.5 : Parametres constants (const)
  Description : Systeme de gestion de messages avec const, var et Result
  Fichier source : 05-parametres-constants-const.md
  ============================================================================ }
{$mode objfpc}{$H+}
program GestionMessages;

type
  TMessage = record
    Expediteur: String;
    Destinataire: String;
    Contenu: String;
    DateHeure: String;
  end;

// Affichage (lecture seule, pas de modification)
procedure AfficherMessage(const msg: TMessage);
begin
  WriteLn('=== MESSAGE ===');
  WriteLn('De : ', msg.Expediteur);
  WriteLn('À : ', msg.Destinataire);
  WriteLn('Date : ', msg.DateHeure);
  WriteLn('---');
  WriteLn(msg.Contenu);
  WriteLn('===============');
end;

// Vérification (lecture seule)
function EstMessageVide(const msg: TMessage): Boolean;
begin
  Result := (Length(msg.Contenu) = 0);
end;

// Création (modification nécessaire)
procedure CreerMessage(var msg: TMessage;
                      const exp, dest, contenu: String);
begin
  msg.Expediteur := exp;
  msg.Destinataire := dest;
  msg.Contenu := contenu;
  msg.DateHeure := '2025-10-12 14:30';  // Simplifié
end;

var
  monMessage: TMessage;
begin
  CreerMessage(monMessage, 'Alice', 'Bob', 'Bonjour Bob !');

  if not EstMessageVide(monMessage) then
    AfficherMessage(monMessage);
end.
