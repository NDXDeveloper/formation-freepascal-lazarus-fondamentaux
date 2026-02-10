{ ============================================================================
  Section 10.3 : Declaration de classes
  Description : Programme simple avec classe TPersonne (getters/setters/validation)
  Fichier source : 03-declaration-classes.md
  ============================================================================ }
program GestionPersonnes;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  // Déclaration de la classe
  TPersonne = class
  private
    FNom: string;
    FAge: Integer;
  public
    procedure DefinirNom(const Valeur: string);
    procedure DefinirAge(Valeur: Integer);
    function ObtenirNom: string;
    function ObtenirAge: Integer;
    procedure Afficher;
  end;

// Implémentation des méthodes
procedure TPersonne.DefinirNom(const Valeur: string);  
begin  
  FNom := Valeur;
end;

procedure TPersonne.DefinirAge(Valeur: Integer);  
begin  
  if (Valeur >= 0) and (Valeur <= 150) then
    FAge := Valeur
  else
    WriteLn('Erreur : âge invalide');
end;

function TPersonne.ObtenirNom: string;  
begin  
  Result := FNom;
end;

function TPersonne.ObtenirAge: Integer;  
begin  
  Result := FAge;
end;

procedure TPersonne.Afficher;  
begin  
  WriteLn('Nom : ', FNom);
  WriteLn('Age : ', FAge, ' ans');
end;

// Programme principal
var
  Personne1: TPersonne;
begin
  Personne1 := TPersonne.Create;
  try
    Personne1.DefinirNom('Marie Curie');
    Personne1.DefinirAge(66);
    Personne1.Afficher;
  finally
    Personne1.Free;
  end;
end.
