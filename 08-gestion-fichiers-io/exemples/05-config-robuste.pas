{ ============================================================================
  Section 8.5 : Gestion des erreurs I/O
  Description : Application robuste lisant un fichier de configuration
  Fichier source : 05-gestion-erreurs-io.md
  ============================================================================ }
{$mode objfpc}{$H+}
program ConfigRobuste;

uses
  SysUtils;

type
  TConfig = record
    NomUtilisateur: string;
    CouleurTheme: string;
    TaillePolice: Integer;
  end;

var
  Config: TConfig;

function MessageErreur(Code: Integer): string;
begin
  case Code of
    2: Result := 'Fichier de configuration introuvable';
    5: Result := 'Accès refusé au fichier de configuration';
    100: Result := 'Erreur de lecture du disque';
  else
    Result := 'Erreur inconnue (code ' + IntToStr(Code) + ')';
  end;
end;

function ChargerConfig(NomFichier: string; var Cfg: TConfig): Boolean;
var
  F: TextFile;
  Ligne, Cle, Valeur: string;
  PosEgal: Integer;
  CodeErreur: Integer;
begin
  ChargerConfig := False;

  // Valeurs par defaut
  Cfg.NomUtilisateur := 'Invité';
  Cfg.CouleurTheme := 'Bleu';
  Cfg.TaillePolice := 12;

  WriteLn('Chargement de la configuration...');

  // Verification d'existence
  if not FileExists(NomFichier) then
  begin
    WriteLn('→ Fichier non trouvé, utilisation des valeurs par défaut');
    ChargerConfig := True;  // Pas une erreur critique
    Exit;
  end;

  Assign(F, NomFichier);

  // Ouverture
  {$I-}
  Reset(F);
  {$I+}

  CodeErreur := IOResult;

  if CodeErreur <> 0 then
  begin
    WriteLn('ERREUR : ', MessageErreur(CodeErreur));
    Exit;
  end;

  // Lecture ligne par ligne
  try
    while not EOF(F) do
    begin
      {$I-}
      ReadLn(F, Ligne);
      {$I+}

      if IOResult <> 0 then
      begin
        WriteLn('Erreur lors de la lecture du fichier');
        Close(F);
        Exit;
      end;

      // Ignorer les lignes vides et commentaires
      Ligne := Trim(Ligne);
      if (Ligne = '') or (Ligne[1] = '#') then
        Continue;

      // Parser la ligne : Cle=Valeur
      PosEgal := Pos('=', Ligne);
      if PosEgal > 0 then
      begin
        Cle := Trim(Copy(Ligne, 1, PosEgal - 1));
        Valeur := Trim(Copy(Ligne, PosEgal + 1, Length(Ligne)));

        // Affecter la configuration
        if Cle = 'NomUtilisateur' then
          Cfg.NomUtilisateur := Valeur
        else if Cle = 'CouleurTheme' then
          Cfg.CouleurTheme := Valeur
        else if Cle = 'TaillePolice' then
        begin
          try
            Cfg.TaillePolice := StrToInt(Valeur);
          except
            WriteLn('Attention : TaillePolice invalide, valeur par défaut utilisée');
          end;
        end;
      end;
    end;

    ChargerConfig := True;

  finally
    {$I-}
    Close(F);
    {$I+}
    IOResult;  // Vider le code d'erreur
  end;

  WriteLn('→ Configuration chargée avec succès');
end;

begin
  if ChargerConfig('config.ini', Config) then
  begin
    WriteLn;
    WriteLn('=== CONFIGURATION ===');
    WriteLn('Utilisateur  : ', Config.NomUtilisateur);
    WriteLn('Thème        : ', Config.CouleurTheme);
    WriteLn('Taille police: ', Config.TaillePolice);
  end
  else
    WriteLn('Impossible de charger la configuration !');
end.
