{ ============================================================================
  Section 8.5 : Gestion des erreurs I/O
  Description : Gestion des erreurs I/O avec try-except (approche moderne)
  Fichier source : 05-gestion-erreurs-io.md
  ============================================================================ }
{$mode objfpc}{$H+}
program GestionModerne;

uses
  SysUtils;  // Necessaire pour les exceptions

var
  F: TextFile;
  Ligne: string;

begin
  Assign(F, 'donnees.txt');

  try
    Reset(F);
    WriteLn('Fichier ouvert.');

    while not EOF(F) do
    begin
      ReadLn(F, Ligne);
      WriteLn(Ligne);
    end;

  except
    on E: EInOutError do
    begin
      WriteLn('Erreur I/O : ', E.Message);
      WriteLn('Code d''erreur : ', E.ErrorCode);
    end;

    on E: Exception do
      WriteLn('Erreur inattendue : ', E.Message);
  end;

  // Fermeture securisee
  try
    Close(F);
  except
    // Ignorer les erreurs de fermeture
  end;

  WriteLn('Programme terminé.');
end.
