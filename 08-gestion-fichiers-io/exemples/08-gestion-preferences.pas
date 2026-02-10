{ ============================================================================
  Section 8.8 : Fichiers INI pour configuration
  Description : Gestionnaire interactif de preferences utilisateur
  Fichier source : 08-fichiers-ini-configuration.md
  Note : Programme interactif (ReadLn)
  ============================================================================ }
{$mode objfpc}{$H+}
program GestionPreferences;

uses
  IniFiles, SysUtils;

type
  TPreferences = record
    Langue: string;
    Theme: string;
    TaillePolice: Integer;
    FenetreMaximisee: Boolean;
    FenetreLargeur: Integer;
    FenetreHauteur: Integer;
    DernierFichierOuvert: string;
  end;

var
  Prefs: TPreferences;

procedure ChargerPreferences(var Prefs: TPreferences);  
var  
  IniFile: TIniFile;
  CheminIni: string;
begin
  CheminIni := ExtractFilePath(ParamStr(0)) + 'config.ini';

  IniFile := TIniFile.Create(CheminIni);
  try
    WriteLn('Chargement des préférences depuis : ', CheminIni);
    WriteLn;

    Prefs.Langue := IniFile.ReadString('Interface', 'Langue', 'Français');
    Prefs.Theme := IniFile.ReadString('Interface', 'Theme', 'Clair');
    Prefs.TaillePolice := IniFile.ReadInteger('Interface', 'TaillePolice', 11);

    Prefs.FenetreMaximisee := IniFile.ReadBool('Fenetre', 'Maximisee', False);
    Prefs.FenetreLargeur := IniFile.ReadInteger('Fenetre', 'Largeur', 1024);
    Prefs.FenetreHauteur := IniFile.ReadInteger('Fenetre', 'Hauteur', 768);

    Prefs.DernierFichierOuvert := IniFile.ReadString('Fichiers', 'DernierOuvert', '');

    WriteLn('Préférences chargées avec succès');
  finally
    IniFile.Free;
  end;
end;

procedure SauvegarderPreferences(const Prefs: TPreferences);  
var  
  IniFile: TIniFile;
  CheminIni: string;
begin
  CheminIni := ExtractFilePath(ParamStr(0)) + 'config.ini';

  IniFile := TIniFile.Create(CheminIni);
  try
    WriteLn('Sauvegarde des préférences...');

    IniFile.WriteString('Interface', 'Langue', Prefs.Langue);
    IniFile.WriteString('Interface', 'Theme', Prefs.Theme);
    IniFile.WriteInteger('Interface', 'TaillePolice', Prefs.TaillePolice);

    IniFile.WriteBool('Fenetre', 'Maximisee', Prefs.FenetreMaximisee);
    IniFile.WriteInteger('Fenetre', 'Largeur', Prefs.FenetreLargeur);
    IniFile.WriteInteger('Fenetre', 'Hauteur', Prefs.FenetreHauteur);

    IniFile.WriteString('Fichiers', 'DernierOuvert', Prefs.DernierFichierOuvert);

    WriteLn('Préférences sauvegardées avec succès');
  finally
    IniFile.Free;
  end;
end;

procedure AfficherPreferences(const Prefs: TPreferences);  
begin  
  WriteLn;
  WriteLn('=== PRÉFÉRENCES ACTUELLES ===');
  WriteLn('Langue          : ', Prefs.Langue);
  WriteLn('Thème           : ', Prefs.Theme);
  WriteLn('Taille police   : ', Prefs.TaillePolice);
  WriteLn('Fenêtre maximisée : ', Prefs.FenetreMaximisee);
  WriteLn('Largeur         : ', Prefs.FenetreLargeur);
  WriteLn('Hauteur         : ', Prefs.FenetreHauteur);
  WriteLn('Dernier fichier : ', Prefs.DernierFichierOuvert);
  WriteLn('============================');
end;

procedure ModifierPreferences(var Prefs: TPreferences);  
var  
  Choix: Integer;
begin
  WriteLn;
  WriteLn('Que voulez-vous modifier ?');
  WriteLn('1. Langue');
  WriteLn('2. Thème');
  WriteLn('3. Taille de police');
  WriteLn('0. Retour');
  Write('Choix : ');
  ReadLn(Choix);

  case Choix of
    1: begin
         Write('Nouvelle langue : ');
         ReadLn(Prefs.Langue);
       end;
    2: begin
         Write('Nouveau thème : ');
         ReadLn(Prefs.Theme);
       end;
    3: begin
         Write('Nouvelle taille de police : ');
         ReadLn(Prefs.TaillePolice);
       end;
  end;
end;

var
  Choix: Integer;

begin
  WriteLn('=================================');
  WriteLn('  GESTIONNAIRE DE PRÉFÉRENCES   ');
  WriteLn('=================================');

  ChargerPreferences(Prefs);

  repeat
    WriteLn;
    WriteLn('1. Afficher les préférences');
    WriteLn('2. Modifier les préférences');
    WriteLn('3. Sauvegarder');
    WriteLn('0. Quitter');
    Write('Choix : ');
    ReadLn(Choix);

    case Choix of
      1: AfficherPreferences(Prefs);
      2: ModifierPreferences(Prefs);
      3: SauvegarderPreferences(Prefs);
      0: begin
           WriteLn('Sauvegarde automatique avant de quitter...');
           SauvegarderPreferences(Prefs);
           WriteLn('Au revoir !');
         end;
    else
      WriteLn('Choix invalide !');
    end;

  until Choix = 0;
end.
