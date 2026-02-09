{ ============================================================================
  Section 19.4 : Unites specifiques a chaque plateforme
  Description : Ecriture dans le registre Windows (alternative sous Linux)
  Fichier source : 04-unites-specifiques-plateforme.md
  ============================================================================ }
program EcritureRegistre;

{$mode objfpc}{$H+}

uses
  SysUtils
  {$IFDEF WINDOWS}
  , Registry
  {$ENDIF};

procedure SauvegarderParametre(const Valeur: string);
{$IFDEF WINDOWS}
var
  Reg: TRegistry;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    // Creer la cle si elle n'existe pas
    if Reg.OpenKey('Software\MonApplication', True) then
    begin
      Reg.WriteString('Parametres', Valeur);
      WriteLn('Parametre sauvegarde dans le registre');
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
  {$ELSE}
  WriteLn('Registre non disponible - sauvegarde dans un fichier INI');
  {$ENDIF}
end;

begin
  SauvegarderParametre('Ma valeur de test');
end.
