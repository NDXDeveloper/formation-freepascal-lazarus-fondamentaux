{ ============================================================================
  Section 19.4 : Unites specifiques a chaque plateforme
  Description : Lecture du registre Windows (alternative portable sous Linux)
  Fichier source : 04-unites-specifiques-plateforme.md
  ============================================================================ }
program LectureRegistre;

{$mode objfpc}{$H+}

uses
  SysUtils
  {$IFDEF WINDOWS}
  , Registry
  {$ENDIF};

procedure LireParametre;
{$IFDEF WINDOWS}
var
  Reg: TRegistry;
  Valeur: string;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  Reg := TRegistry.Create;
  try
    // Ouvrir une cle en lecture seule
    Reg.RootKey := HKEY_CURRENT_USER;

    if Reg.OpenKeyReadOnly('Software\MonApplication') then
    begin
      if Reg.ValueExists('Parametres') then
      begin
        Valeur := Reg.ReadString('Parametres');
        WriteLn('Parametre trouve : ', Valeur);
      end
      else
        WriteLn('Parametre non trouve');

      Reg.CloseKey;
    end
    else
      WriteLn('Cle non trouvee');
  finally
    Reg.Free;
  end;
  {$ELSE}
  WriteLn('Le registre Windows n''existe pas sous Unix/Linux');
  WriteLn('Utiliser des fichiers de configuration a la place');
  {$ENDIF}
end;

begin
  LireParametre;
end.
