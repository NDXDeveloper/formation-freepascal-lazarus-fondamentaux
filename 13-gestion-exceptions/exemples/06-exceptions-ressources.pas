{ ============================================================================
  Section 13.6 : Exceptions et ressources
  Description : try-finally fichier, objet, multiples ressources, FreeAndNil
  Fichier source : 06-exceptions-ressources.md
  ============================================================================ }
program ExceptionsRessources;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

{ --- Demonstration 1 : try-finally pour fichier --- }
procedure DemoFichier;
var
  f: TextFile;
  nomFichier: String;
  ligne: String;
begin
  WriteLn('=== 1. Try-finally pour fichier ===');
  nomFichier := 'test_ressource.txt';

  { Creer un fichier de test }
  AssignFile(f, nomFichier);
  Rewrite(f);
  WriteLn(f, 'Ligne 1');
  WriteLn(f, 'Ligne 2');
  WriteLn(f, 'Ligne 3');
  CloseFile(f);

  { Lire avec try-finally }
  AssignFile(f, nomFichier);
  Reset(f);
  try
    while not EOF(f) do
    begin
      ReadLn(f, ligne);
      WriteLn('  Lu : ', ligne);
    end;
  finally
    CloseFile(f);
    WriteLn('  Fichier ferme (finally execute)');
  end;

  { Nettoyage }
  DeleteFile(nomFichier);
  WriteLn;
end;

{ --- Demonstration 2 : try-finally pour objet --- }
procedure DemoObjet;
var
  liste: TStringList;
begin
  WriteLn('=== 2. Try-finally pour objet ===');
  liste := TStringList.Create;
  try
    liste.Add('Premier');
    liste.Add('Deuxieme');
    liste.Add('Troisieme');
    liste.Sort;
    WriteLn('  Liste triee : ', liste.CommaText);
  finally
    liste.Free;
    WriteLn('  Liste liberee (finally execute)');
  end;
  WriteLn;
end;

{ --- Demonstration 3 : ressources multiples avec init a nil + FreeAndNil --- }
procedure DemoMultiplesRessources;
var
  liste1, liste2, liste3: TStringList;
begin
  WriteLn('=== 3. Multiples ressources avec init a nil ===');
  liste1 := nil;
  liste2 := nil;
  liste3 := nil;

  try
    liste1 := TStringList.Create;
    liste1.Add('Ressource 1');
    WriteLn('  Liste 1 creee');

    liste2 := TStringList.Create;
    liste2.Add('Ressource 2');
    WriteLn('  Liste 2 creee');

    liste3 := TStringList.Create;
    liste3.Add('Ressource 3');
    WriteLn('  Liste 3 creee');

    WriteLn('  Toutes les ressources actives');
    WriteLn('  Contenu : ', liste1[0], ', ', liste2[0], ', ', liste3[0]);
  finally
    FreeAndNil(liste3);
    WriteLn('  Liste 3 liberee');
    FreeAndNil(liste2);
    WriteLn('  Liste 2 liberee');
    FreeAndNil(liste1);
    WriteLn('  Liste 1 liberee');
  end;
  WriteLn;
end;

{ --- Demonstration 4 : multiples ressources avec erreur au milieu --- }
procedure DemoRessourcesAvecErreur;
var
  liste1, liste2, liste3: TStringList;
begin
  WriteLn('=== 4. Multiples ressources avec erreur ===');
  liste1 := nil;
  liste2 := nil;
  liste3 := nil;

  try
    try
      liste1 := TStringList.Create;
      WriteLn('  Liste 1 creee');

      liste2 := TStringList.Create;
      WriteLn('  Liste 2 creee');

      { Simuler une erreur avant la creation de liste3 }
      raise Exception.Create('Erreur simulee pendant l''initialisation');

      liste3 := TStringList.Create;
      WriteLn('  Liste 3 creee - jamais execute');
    finally
      { FreeAndNil est sur meme si l'objet est nil }
      FreeAndNil(liste3);
      WriteLn('  Liste 3 : ', BoolToStr(liste3 = nil, 'etait nil, rien a faire', 'liberee'));
      FreeAndNil(liste2);
      WriteLn('  Liste 2 liberee');
      FreeAndNil(liste1);
      WriteLn('  Liste 1 liberee');
    end;
  except
    on E: Exception do
      WriteLn('  Exception geree : ', E.Message);
  end;
  WriteLn;
end;

{ --- Demonstration 5 : combinaison try-except + try-finally --- }
procedure DemoCombinaison;
var
  liste: TStringList;
begin
  WriteLn('=== 5. Combinaison try-except + try-finally ===');
  try
    liste := TStringList.Create;
    try
      liste.Add('Donnee valide');
      WriteLn('  Donnee ajoutee : ', liste[0]);
      { Simuler une erreur de traitement }
      raise Exception.Create('Erreur pendant le traitement des donnees');
    finally
      liste.Free;
      WriteLn('  Liste liberee dans finally');
    end;
  except
    on E: Exception do
      WriteLn('  Exception geree dans except : ', E.Message);
  end;
  WriteLn;
end;

{ --- Demonstration 6 : try-finally imbrique pour deux fichiers --- }
procedure DemoFichiersImbriques;
var
  fSource, fDest: TextFile;
  ligne: String;
  nomSource, nomDest: String;
begin
  WriteLn('=== 6. Try-finally imbrique (deux fichiers) ===');
  nomSource := 'source_test.txt';
  nomDest := 'dest_test.txt';

  { Creer le fichier source }
  AssignFile(fSource, nomSource);
  Rewrite(fSource);
  WriteLn(fSource, 'Alpha');
  WriteLn(fSource, 'Beta');
  WriteLn(fSource, 'Gamma');
  CloseFile(fSource);

  { Copier avec deux try-finally imbriques }
  AssignFile(fSource, nomSource);
  Reset(fSource);
  try
    AssignFile(fDest, nomDest);
    Rewrite(fDest);
    try
      while not EOF(fSource) do
      begin
        ReadLn(fSource, ligne);
        WriteLn(fDest, ligne);
        WriteLn('  Copie : ', ligne);
      end;
    finally
      CloseFile(fDest);
      WriteLn('  Fichier destination ferme');
    end;
  finally
    CloseFile(fSource);
    WriteLn('  Fichier source ferme');
  end;

  { Nettoyage }
  DeleteFile(nomSource);
  DeleteFile(nomDest);
  WriteLn;
end;

{ === Programme principal === }
begin
  WriteLn('--- Chapitre 13.6 : Exceptions et ressources ---');
  WriteLn;

  DemoFichier;
  DemoObjet;
  DemoMultiplesRessources;
  DemoRessourcesAvecErreur;
  DemoCombinaison;
  DemoFichiersImbriques;

  WriteLn('--- Fin des demonstrations ---');
end.
