{
  Section 20.1 - Utilisation avancée du débogueur Lazarus
  Description : Programme de démonstration pour pratiquer le débogueur
                Appels imbriqués (pile d'appels), types variés, exception
                Ouvrir dans Lazarus, placer des points d'arrêt, utiliser F7/F8
  Fichier source : 01-utilisation-avancee-debogueur-lazarus.md
}
program DemoDebogage;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  TPersonne = record
    Nom: String;
    Age: Integer;
    Actif: Boolean;
  end;

procedure NiveauC(valeur: Integer);
var
  resultat: Integer;
begin
  { Pratiquez ici : inspectez 'valeur' et 'resultat' }
  resultat := valeur * 3;
  WriteLn('  NiveauC: valeur=', valeur, ' resultat=', resultat);
end;

procedure NiveauB(x: Integer);
var
  y: Integer;
begin
  y := x + 10;
  WriteLn(' NiveauB: x=', x, ' y=', y);
  NiveauC(y);
end;

procedure NiveauA;
var
  i: Integer;
begin
  WriteLn('NiveauA: debut');
  for i := 1 to 3 do
  begin
    WriteLn('NiveauA: iteration ', i);
    NiveauB(i * 5);
  end;
  WriteLn('NiveauA: fin');
end;

procedure DemoVariables;
var
  entier: Integer;
  reel: Double;
  chaine: String;
  booleen: Boolean;
  personne: TPersonne;
  tableau: array[1..5] of Integer;
  i: Integer;
begin
  WriteLn;
  WriteLn('=== Demo Variables ===');

  entier := 42;
  reel := 3.14159;
  chaine := 'Bonjour le monde';
  booleen := True;

  personne.Nom := 'Dupont';
  personne.Age := 30;
  personne.Actif := True;

  for i := 1 to 5 do
    tableau[i] := i * i;

  { Point d'arrêt ici : inspectez toutes les variables }
  WriteLn('entier=', entier, ' reel=', reel:0:5);
  WriteLn('chaine=', chaine, ' booleen=', booleen);
  WriteLn('personne: ', personne.Nom, ', age ', personne.Age);
  Write('tableau: ');
  for i := 1 to 5 do
    Write(tableau[i], ' ');
  WriteLn;
end;

procedure DemoException;
var
  tab: array[1..10] of Integer;
  i, indice: Integer;
begin
  WriteLn;
  WriteLn('=== Demo Exception ===');
  for i := 1 to 10 do
    tab[i] := i * 10;

  indice := 5;
  WriteLn('tab[', indice, '] = ', tab[indice]);

  { Le débogueur s'arrête ici en cas d'exception }
  try
    indice := 0;
    WriteLn('Tentative d''accès à l''indice ', indice, '...');
    WriteLn('Résultat : ', 100 div indice);
  except
    on E: Exception do
      WriteLn('Exception attrapée : ', E.ClassName, ' - ', E.Message);
  end;
end;

begin
  WriteLn('=== Demo Debogage ===');
  WriteLn('Ce programme est conçu pour pratiquer le débogueur Lazarus.');
  WriteLn('Ouvrez-le dans Lazarus et utilisez F7 (pas à pas approfondi)');
  WriteLn('et F8 (pas à pas) pour explorer l''exécution.');
  WriteLn;

  WriteLn('=== Demo Pile d''appels ===');
  NiveauA;

  DemoVariables;

  DemoException;

  WriteLn;
  WriteLn('Programme terminé.');
end.
