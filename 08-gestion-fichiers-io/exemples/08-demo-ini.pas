{ ============================================================================
  Section 8.8 : Fichiers INI pour configuration
  Description : Demonstration de TIniFile - ecriture, lecture, sections, cles
  Fichier source : 08-fichiers-ini-configuration.md
  ============================================================================ }
{$mode objfpc}{$H+}
program DemoIni;

uses
  IniFiles, SysUtils, Classes;

var
  IniFile: TIniFile;
  Sections, Cles, Valeurs: TStringList;
  i: Integer;
  FichierIni: string;

begin
  FichierIni := 'demo_config.ini';

  WriteLn('=== Démonstration TIniFile ===');
  WriteLn;

  // 1. Ecriture de valeurs
  WriteLn('--- Écriture des valeurs ---');
  IniFile := TIniFile.Create(FichierIni);
  try
    IniFile.WriteString('Interface', 'Langue', 'Français');
    IniFile.WriteString('Interface', 'Theme', 'Clair');
    IniFile.WriteInteger('Interface', 'TaillePolice', 12);

    IniFile.WriteString('Connexion', 'Serveur', '192.168.1.100');
    IniFile.WriteInteger('Connexion', 'Port', 8080);
    IniFile.WriteInteger('Connexion', 'Timeout', 30);
    IniFile.WriteBool('Connexion', 'SSL', False);

    IniFile.WriteBool('Fenetre', 'Maximisee', False);
    IniFile.WriteInteger('Fenetre', 'Largeur', 1024);
    IniFile.WriteInteger('Fenetre', 'Hauteur', 768);

    WriteLn('Valeurs écrites dans ', FichierIni);
  finally
    IniFile.Free;
  end;

  WriteLn;

  // 2. Lecture de valeurs
  WriteLn('--- Lecture des valeurs ---');
  IniFile := TIniFile.Create(FichierIni);
  try
    WriteLn('Langue : ', IniFile.ReadString('Interface', 'Langue', 'Français'));
    WriteLn('Port : ', IniFile.ReadInteger('Connexion', 'Port', 8080));
    WriteLn('Maximisée : ', IniFile.ReadBool('Fenetre', 'Maximisee', False));
  finally
    IniFile.Free;
  end;

  WriteLn;

  // 3. Lister les sections
  WriteLn('--- Sections du fichier ---');
  IniFile := TIniFile.Create(FichierIni);
  Sections := TStringList.Create;
  try
    IniFile.ReadSections(Sections);
    for i := 0 to Sections.Count - 1 do
      WriteLn('  [', Sections[i], ']');
  finally
    Sections.Free;
    IniFile.Free;
  end;

  WriteLn;

  // 4. Lister les cles d'une section
  WriteLn('--- Clés de [Interface] ---');
  IniFile := TIniFile.Create(FichierIni);
  Cles := TStringList.Create;
  try
    IniFile.ReadSection('Interface', Cles);
    for i := 0 to Cles.Count - 1 do
      WriteLn('  ', Cles[i]);
  finally
    Cles.Free;
    IniFile.Free;
  end;

  WriteLn;

  // 5. Lister les valeurs d'une section
  WriteLn('--- Contenu de [Connexion] ---');
  IniFile := TIniFile.Create(FichierIni);
  Valeurs := TStringList.Create;
  try
    IniFile.ReadSectionValues('Connexion', Valeurs);
    for i := 0 to Valeurs.Count - 1 do
      WriteLn('  ', Valeurs[i]);
  finally
    Valeurs.Free;
    IniFile.Free;
  end;

  WriteLn;

  // 6. Verifier existence et supprimer
  WriteLn('--- Vérification et suppression ---');
  IniFile := TIniFile.Create(FichierIni);
  try
    if IniFile.SectionExists('Interface') then
      WriteLn('La section [Interface] existe');

    if IniFile.ValueExists('Interface', 'Langue') then
      WriteLn('La clé "Langue" existe dans [Interface]');

    IniFile.DeleteKey('Interface', 'Theme');
    WriteLn('Clé "Theme" supprimée de [Interface]');
  finally
    IniFile.Free;
  end;

  // Nettoyage
  if FileExists(FichierIni) then
    DeleteFile(FichierIni);

  WriteLn;
  WriteLn('Fichier de démonstration nettoyé.');
end.
