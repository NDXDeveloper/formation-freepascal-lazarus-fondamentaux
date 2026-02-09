{ ============================================================================
  Section 4.11 : Organisation modulaire du code
  Description : Programme structure avec sections logiques et gestion utilisateurs
  Fichier source : 11-organisation-modulaire-code.md
  ============================================================================ }
{$mode objfpc}{$H+}
program MonProgramme;

uses
  SysUtils;  // Unités nécessaires

// ========================================
// SECTION 1 : CONSTANTES ET TYPES
// ========================================

const
  VERSION = '1.0';
  MAX_USERS = 100;

type
  TUtilisateur = record
    Nom: String;
    Age: Integer;
  end;

// ========================================
// SECTION 2 : VARIABLES GLOBALES (si nécessaire)
// ========================================

var
  utilisateurs: array[1..MAX_USERS] of TUtilisateur;
  nbUtilisateurs: Integer;

// ========================================
// SECTION 3 : FONCTIONS UTILITAIRES DE BAS NIVEAU
// ========================================

function EstChaineVide(const s: String): Boolean;
begin
  Result := Trim(s) = '';
end;

function LireEntierPositif(const prompt: String): Integer;
var
  texte: String;
  valeur: Integer;
begin
  repeat
    Write(prompt, ' : ');
    ReadLn(texte);
    valeur := StrToIntDef(texte, -1);
  until valeur >= 0;
  Result := valeur;
end;

// ========================================
// SECTION 4 : FONCTIONS MÉTIER
// ========================================

function CreerUtilisateur(const nom: String; age: Integer): TUtilisateur;
begin
  Result.Nom := nom;
  Result.Age := age;
end;

procedure AjouterUtilisateur(const user: TUtilisateur);
begin
  if nbUtilisateurs < MAX_USERS then
  begin
    Inc(nbUtilisateurs);
    utilisateurs[nbUtilisateurs] := user;
  end;
end;

// ========================================
// SECTION 5 : INTERFACE UTILISATEUR
// ========================================

procedure AfficherMenu;
begin
  WriteLn('=== MENU ===');
  WriteLn('1. Ajouter utilisateur');
  WriteLn('2. Lister utilisateurs');
  WriteLn('0. Quitter');
end;

procedure TraiterAjout;
var
  nom: String;
  age: Integer;
  user: TUtilisateur;
begin
  Write('Nom : ');
  ReadLn(nom);
  age := LireEntierPositif('Age');
  user := CreerUtilisateur(nom, age);
  AjouterUtilisateur(user);
  WriteLn('Utilisateur ajouté !');
end;

// ========================================
// SECTION 6 : PROGRAMME PRINCIPAL
// ========================================

var
  choix: Integer;
begin
  nbUtilisateurs := 0;

  repeat
    AfficherMenu;
    choix := LireEntierPositif('Choix');

    case choix of
      1: TraiterAjout;
      0: WriteLn('Au revoir !');
    else
      WriteLn('Choix invalide');
    end;
  until choix = 0;
end.
