{ ============================================================================
  Section 4.11 : Organisation modulaire du code
  Description : Version bien organisee en modules (bonnes pratiques)
  Fichier source : 11-organisation-modulaire-code.md
  ============================================================================ }
{$mode objfpc}{$H+}
program Bon;

uses SysUtils;

// === MODULE INTERFACE UTILISATEUR ===

procedure AfficherMenu;  
begin  
  WriteLn('=== MENU ===');
  WriteLn('1. Inscription');
  WriteLn('2. Calculer moyenne');
  WriteLn('3. Quitter');
end;

function LireChoix: Integer;  
var  
  texte: String;
begin
  Write('Choix : ');
  ReadLn(texte);
  Result := StrToIntDef(texte, 0);
end;

// === MODULE INSCRIPTION ===

function LireTexte(const prompt: String): String;  
begin  
  Write(prompt, ' : ');
  ReadLn(Result);
end;

function LireAge: Integer;  
var  
  texte: String;
begin
  Write('Age : ');
  ReadLn(texte);
  Result := StrToIntDef(texte, 0);
end;

function EstMajeur(age: Integer): Boolean;  
begin  
  Result := age >= 18;
end;

procedure Inscrire;  
var  
  nom, prenom, email: String;
  age: Integer;
begin
  nom := LireTexte('Nom');
  prenom := LireTexte('Prénom');
  email := LireTexte('Email');
  age := LireAge;

  if EstMajeur(age) then
    WriteLn('Statut : Majeur')
  else
    WriteLn('Statut : Mineur');

  WriteLn(Format('Inscription de %s %s (%s)', [prenom, nom, email]));
end;

// === MODULE CALCULS ===

function LirePrix(const prompt: String): Real;  
var  
  texte: String;
begin
  Write(prompt, ' : ');
  ReadLn(texte);
  Result := StrToFloatDef(texte, 0.0);  // Convertit en réel, retourne 0.0 si invalide
end;

function CalculerMoyenne(v1, v2, v3: Real): Real;  
begin  
  Result := (v1 + v2 + v3) / 3;
end;

procedure AfficherMoyenne;  
var  
  prix1, prix2, prix3, moyenne: Real;
begin
  prix1 := LirePrix('Prix 1');
  prix2 := LirePrix('Prix 2');
  prix3 := LirePrix('Prix 3');
  moyenne := CalculerMoyenne(prix1, prix2, prix3);
  WriteLn('Moyenne : ', moyenne:0:2);
end;

// === PROGRAMME PRINCIPAL ===

var
  choix: Integer;
begin
  AfficherMenu;
  choix := LireChoix;

  case choix of
    1: Inscrire;
    2: AfficherMoyenne;
    3: WriteLn('Au revoir !');
  else
    WriteLn('Choix invalide');
  end;
end.
