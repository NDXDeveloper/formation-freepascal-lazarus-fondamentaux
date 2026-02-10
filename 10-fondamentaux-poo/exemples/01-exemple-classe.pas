{ ============================================================================
  Section 10.1 : Concepts : Classes et Objets
  Description : Exemple complet d'une classe TCompteur avec attribut et methodes
  Fichier source : 01-concepts-classes-objets.md
  ============================================================================ }
program ExempleClasse;

{$mode objfpc}{$H+}

type
  TCompteur = class
  private
    FValeur: Integer;
  public
    procedure Incrementer;
    procedure Decrementer;
    procedure Afficher;
  end;

procedure TCompteur.Incrementer;  
begin  
  FValeur := FValeur + 1;
end;

procedure TCompteur.Decrementer;  
begin  
  FValeur := FValeur - 1;
end;

procedure TCompteur.Afficher;  
begin  
  WriteLn('Valeur du compteur : ', FValeur);
end;

var
  MonCompteur: TCompteur;
begin
  // Création de l'objet
  MonCompteur := TCompteur.Create;

  // Utilisation de l'objet
  MonCompteur.Incrementer;
  MonCompteur.Incrementer;
  MonCompteur.Afficher;  // Affiche : Valeur du compteur : 2

  MonCompteur.Decrementer;
  MonCompteur.Afficher;  // Affiche : Valeur du compteur : 1

  // Libération de la mémoire
  MonCompteur.Free;
end.
