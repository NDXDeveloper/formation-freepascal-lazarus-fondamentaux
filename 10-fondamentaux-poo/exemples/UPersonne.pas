{ ============================================================================
  Section 10.3 : Declaration de classes
  Description : Unite avec une classe TPersonne (exemple d'organisation en unite)
  Fichier source : 03-declaration-classes.md
  ============================================================================ }
unit UPersonne;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  // Déclaration dans la section interface
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

implementation

// Implémentation dans la section implementation
procedure TPersonne.DefinirNom(const Valeur: string);  
begin  
  FNom := Valeur;
end;

procedure TPersonne.DefinirAge(Valeur: Integer);  
begin  
  if (Valeur >= 0) and (Valeur <= 150) then
    FAge := Valeur;
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
  WriteLn('Nom : ', FNom, ', Age : ', FAge, ' ans');
end;

end.
