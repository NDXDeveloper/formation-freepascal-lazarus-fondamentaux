{ ============================================================================
  Section 8.7 : Chemins et noms de fichiers
  Description : Gestionnaire interactif de chemins
  Fichier source : 07-chemins-noms-fichiers.md
  Note : Programme interactif (ReadLn)
  ============================================================================ }
{$mode objfpc}{$H+}
program GestionnaireChemin;

uses
  SysUtils;

procedure AfficherMenu;
begin
  WriteLn;
  WriteLn('=== GESTIONNAIRE DE CHEMINS ===');
  WriteLn('1. Analyser un chemin');
  WriteLn('2. Changer l''extension');
  WriteLn('3. Convertir en absolu');
  WriteLn('4. Extraire le chemin relatif');
  WriteLn('5. Vérifier l''existence');
  WriteLn('6. Construire un chemin');
  WriteLn('0. Quitter');
  Write('Choix : ');
end;

procedure AnalyserChemin;
var
  Chemin: string;
begin
  Write('Chemin à analyser : ');
  ReadLn(Chemin);
  WriteLn;
  WriteLn('=== ANALYSE ===');
  WriteLn('Chemin complet     : ', Chemin);
  WriteLn('Lecteur            : ', ExtractFileDrive(Chemin));
  WriteLn('Répertoire         : ', ExtractFilePath(Chemin));
  WriteLn('Nom du fichier     : ', ExtractFileName(Chemin));
  WriteLn('Extension          : ', ExtractFileExt(Chemin));
  WriteLn('Nom sans extension : ', ChangeFileExt(ExtractFileName(Chemin), ''));
  WriteLn('Chemin absolu      : ', ExpandFileName(Chemin));
end;

procedure ChangerExtension;
var
  Chemin, NouvelleExt, Resultat: string;
begin
  Write('Chemin du fichier : ');
  ReadLn(Chemin);
  Write('Nouvelle extension (avec le point) : ');
  ReadLn(NouvelleExt);

  Resultat := ChangeFileExt(Chemin, NouvelleExt);
  WriteLn('Résultat : ', Resultat);
end;

procedure ConvertirEnAbsolu;
var
  CheminRelatif, CheminAbsolu: string;
begin
  WriteLn('Répertoire courant : ', GetCurrentDir);
  Write('Chemin relatif : ');
  ReadLn(CheminRelatif);

  CheminAbsolu := ExpandFileName(CheminRelatif);
  WriteLn('Chemin absolu : ', CheminAbsolu);
end;

procedure ExtraireCheminRelatif;
var
  Base, Cible, Relatif: string;
begin
  Write('Chemin de base : ');
  ReadLn(Base);
  Write('Chemin cible : ');
  ReadLn(Cible);

  if not DirectoryExists(Base) then
    Base := ExtractFilePath(Base);

  Relatif := ExtractRelativePath(Base, Cible);
  WriteLn('Chemin relatif : ', Relatif);
end;

procedure VerifierExistence;
var
  Chemin: string;
begin
  Write('Chemin à vérifier : ');
  ReadLn(Chemin);
  WriteLn;

  if FileExists(Chemin) then
    WriteLn('V Le fichier existe')
  else if DirectoryExists(Chemin) then
    WriteLn('V Le répertoire existe')
  else
    WriteLn('X N''existe pas');
end;

procedure ConstruireCheminInteractif;
var
  NbSegments, i: Integer;
  Segments: array[1..10] of string;
  Resultat: string;
begin
  Write('Nombre de segments (max 10) : ');
  ReadLn(NbSegments);

  if (NbSegments < 1) or (NbSegments > 10) then
  begin
    WriteLn('Nombre invalide !');
    Exit;
  end;

  for i := 1 to NbSegments do
  begin
    Write('Segment ', i, ' : ');
    ReadLn(Segments[i]);
  end;

  Resultat := Segments[1];

  for i := 2 to NbSegments do
    Resultat := IncludeTrailingPathDelimiter(Resultat) + Segments[i];

  WriteLn;
  WriteLn('Chemin construit : ', Resultat);
end;

var
  Choix: Integer;

begin
  repeat
    AfficherMenu;
    ReadLn(Choix);

    case Choix of
      1: AnalyserChemin;
      2: ChangerExtension;
      3: ConvertirEnAbsolu;
      4: ExtraireCheminRelatif;
      5: VerifierExistence;
      6: ConstruireCheminInteractif;
      0: WriteLn('Au revoir !');
    else
      WriteLn('Choix invalide !');
    end;

  until Choix = 0;
end.
