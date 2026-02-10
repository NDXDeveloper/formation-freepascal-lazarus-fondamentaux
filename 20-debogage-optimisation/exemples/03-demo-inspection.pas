{
  Section 20.3 - Inspection de variables et expressions
  Description : Programme avec structures complexes pour pratiquer
                l'inspection de variables (records, tableaux, objets, espions)
  Fichier source : 03-inspection-variables-expressions.md
}
program DemoInspection;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

type
  TAdresse = record
    Rue: String;
    Ville: String;
    CodePostal: String;
  end;

  TPersonne = record
    Nom: String;
    Prenom: String;
    Age: Integer;
    Actif: Boolean;
    Adresse: TAdresse;
    Notes: array[1..5] of Double;
  end;

procedure DemoRecords;  
var  
  p: TPersonne;
  i: Integer;
  moyenne: Double;
begin
  WriteLn('--- Records imbriqués ---');

  p.Nom := 'Dupont';
  p.Prenom := 'Jean';
  p.Age := 30;
  p.Actif := True;
  p.Adresse.Rue := '15 rue de la Paix';
  p.Adresse.Ville := 'Paris';
  p.Adresse.CodePostal := '75001';
  p.Notes[1] := 15.5;
  p.Notes[2] := 12.0;
  p.Notes[3] := 18.0;
  p.Notes[4] := 14.5;
  p.Notes[5] := 16.0;

  { Point d'arrêt ici : inspectez p avec l'Inspecteur
    - Explorez p.Adresse.Ville
    - Vérifiez p.Notes[3]
    - Ajoutez un espion sur p.Age }

  moyenne := 0;
  for i := 1 to 5 do
    moyenne := moyenne + p.Notes[i];
  moyenne := moyenne / 5;

  WriteLn('  ', p.Prenom, ' ', p.Nom, ', ', p.Age, ' ans');
  WriteLn('  ', p.Adresse.Rue, ', ', p.Adresse.CodePostal, ' ', p.Adresse.Ville);
  WriteLn('  Moyenne des notes : ', moyenne:0:2);
end;

procedure DemoTableaux;  
var  
  entiers: array[1..10] of Integer;
  chaines: array[1..5] of String;
  matrice: array[1..3, 1..3] of Integer;
  i, j: Integer;
begin
  WriteLn;
  WriteLn('--- Tableaux ---');

  for i := 1 to 10 do
    entiers[i] := i * i;

  chaines[1] := 'Alpha';
  chaines[2] := 'Beta';
  chaines[3] := 'Gamma';
  chaines[4] := 'Delta';
  chaines[5] := 'Epsilon';

  for i := 1 to 3 do
    for j := 1 to 3 do
      matrice[i, j] := i * 10 + j;

  { Point d'arrêt ici : inspectez les tableaux
    - Espion sur entiers[5] (devrait valoir 25)
    - Espion sur chaines[3] (devrait être 'Gamma')
    - Espion sur matrice[2,3] (devrait valoir 23) }

  Write('  Carrés : ');
  for i := 1 to 10 do
    Write(entiers[i], ' ');
  WriteLn;

  Write('  Chaînes : ');
  for i := 1 to 5 do
    Write(chaines[i], ' ');
  WriteLn;

  WriteLn('  Matrice :');
  for i := 1 to 3 do
  begin
    Write('    ');
    for j := 1 to 3 do
      Write(matrice[i, j]:4);
    WriteLn;
  end;
end;

procedure DemoObjets;  
var  
  listeNoms: TStringList;
  listeValeurs: TList;
  i: Integer;
  p: ^Integer;
begin
  WriteLn;
  WriteLn('--- Objets (TStringList, TList) ---');

  listeNoms := TStringList.Create;
  listeValeurs := TList.Create;
  try
    listeNoms.Add('Alice');
    listeNoms.Add('Bob');
    listeNoms.Add('Charlie');
    listeNoms.Add('Diana');
    listeNoms.Add('Edgar');

    for i := 1 to 5 do
    begin
      New(p);
      p^ := i * 100;
      listeValeurs.Add(p);
    end;

    { Point d'arrêt ici : inspectez les objets
      - listeNoms.Count (devrait être 5)
      - listeNoms[2] (devrait être 'Charlie')
      - listeValeurs.Count }

    WriteLn('  Noms (', listeNoms.Count, ') :');
    for i := 0 to listeNoms.Count - 1 do
      WriteLn('    [', i, '] ', listeNoms[i]);

    WriteLn('  Valeurs (', listeValeurs.Count, ') :');
    for i := 0 to listeValeurs.Count - 1 do
    begin
      p := listeValeurs[i];
      WriteLn('    [', i, '] ', p^);
    end;
  finally
    for i := 0 to listeValeurs.Count - 1 do
      Dispose(PInteger(listeValeurs[i]));
    listeValeurs.Free;
    listeNoms.Free;
  end;
end;

procedure DemoEvaluationExpressions;  
var  
  x, y: Integer;
  s: String;
  prix: Double;
  quantite: Integer;
begin
  WriteLn;
  WriteLn('--- Évaluation d''expressions ---');

  x := 42;
  y := 17;
  s := 'Bonjour le monde';
  prix := 29.99;
  quantite := 5;

  { Point d'arrêt ici : utilisez Ctrl+F7 (Évaluer/Modifier) pour tester :
    - x + y          (résultat : 59)
    - x * y          (résultat : 714)
    - Length(s)       (résultat : 16)
    - prix * quantite (résultat : 149.95)
    - Copy(s, 1, 7)  (résultat : 'Bonjour') }

  WriteLn('  x=', x, ' y=', y);
  WriteLn('  x + y = ', x + y);
  WriteLn('  x * y = ', x * y);
  WriteLn('  s = "', s, '" (longueur : ', Length(s), ')');
  WriteLn('  Total = ', prix * quantite:0:2, ' EUR');
end;

begin
  WriteLn('=== Demo Inspection de Variables ===');
  WriteLn('Ce programme crée des structures complexes pour pratiquer');
  WriteLn('l''inspection avec le débogueur Lazarus.');
  WriteLn;

  DemoRecords;
  DemoTableaux;
  DemoObjets;
  DemoEvaluationExpressions;

  WriteLn;
  WriteLn('Programme terminé.');
end.
