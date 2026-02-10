{ ============================================================================
  Section 10.7 : Self et reference a l'objet courant
  Description : Classe TPoint avec chainage de methodes et comparaisons via Self
  Fichier source : 07-self-reference-objet-courant.md
  ============================================================================ }
program ExempleSelf;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  TPoint = class
  private
    FX: Real;
    FY: Real;
  public
    constructor Create(X, Y: Real);

    // Méthodes de modification qui retournent Self (chaînage)
    function Deplacer(DeltaX, DeltaY: Real): TPoint;
    function DefinirX(X: Real): TPoint;
    function DefinirY(Y: Real): TPoint;

    // Méthodes de comparaison utilisant Self
    function DistanceVers(Autre: TPoint): Real;
    function EstPlusProcheQue(Point1, Point2: TPoint): Boolean;
    function EstALOrigine: Boolean;

    // Méthodes utilitaires
    function Cloner: TPoint;
    procedure Afficher;
    procedure CopierVers(Destination: TPoint);
  end;

// === IMPLÉMENTATION ===

constructor TPoint.Create(X, Y: Real);  
begin  
  inherited Create;
  Self.FX := X;  // Self explicite pour la clarté
  Self.FY := Y;
  WriteLn('Point créé : (', X:0:2, ', ', Y:0:2, ')');
end;

function TPoint.Deplacer(DeltaX, DeltaY: Real): TPoint;  
begin  
  FX := FX + DeltaX;
  FY := FY + DeltaY;
  Result := Self;  // Retourne l'objet pour permettre le chaînage
end;

function TPoint.DefinirX(X: Real): TPoint;  
begin  
  Self.FX := X;
  Result := Self;
end;

function TPoint.DefinirY(Y: Real): TPoint;  
begin  
  Self.FY := Y;
  Result := Self;
end;

function TPoint.DistanceVers(Autre: TPoint): Real;  
var  
  DX, DY: Real;
begin
  // Self représente le point actuel, Autre est le point de comparaison
  DX := Self.FX - Autre.FX;
  DY := Self.FY - Autre.FY;
  Result := Sqrt(DX * DX + DY * DY);
end;

function TPoint.EstPlusProcheQue(Point1, Point2: TPoint): Boolean;  
var  
  Distance1, Distance2: Real;
begin
  // Self est le point de référence
  Distance1 := Self.DistanceVers(Point1);
  Distance2 := Self.DistanceVers(Point2);
  Result := Distance1 < Distance2;
end;

function TPoint.EstALOrigine: Boolean;  
begin  
  Result := (Self.FX = 0) and (Self.FY = 0);
end;

function TPoint.Cloner: TPoint;  
begin  
  // Crée une copie de Self
  Result := TPoint.Create(Self.FX, Self.FY);
end;

procedure TPoint.Afficher;  
begin  
  WriteLn('Point : (', FX:0:2, ', ', FY:0:2, ')');
  WriteLn('Adresse mémoire : ', IntToHex(PtrUInt(Self), 16));
end;

procedure TPoint.CopierVers(Destination: TPoint);  
begin  
  // Copie les coordonnées de Self vers Destination
  Destination.FX := Self.FX;
  Destination.FY := Self.FY;
end;

// === PROGRAMME PRINCIPAL ===

var
  P1, P2, P3: TPoint;
begin
  WriteLn('=== Création des points ===');
  P1 := TPoint.Create(0, 0);
  P2 := TPoint.Create(3, 4);
  P3 := TPoint.Create(10, 10);
  WriteLn;

  WriteLn('=== Affichage initial ===');
  P1.Afficher;
  P2.Afficher;
  P3.Afficher;
  WriteLn;

  WriteLn('=== Chaînage de méthodes ===');
  P1.DefinirX(5).DefinirY(5).Deplacer(2, 2);
  WriteLn('P1 après chaînage :');
  P1.Afficher;
  WriteLn;

  WriteLn('=== Calcul de distances ===');
  WriteLn('Distance P1 -> P2 : ', P1.DistanceVers(P2):0:2);
  WriteLn('Distance P1 -> P3 : ', P1.DistanceVers(P3):0:2);
  WriteLn;

  WriteLn('=== Comparaisons ===');
  if P1.EstPlusProcheQue(P2, P3) then
    WriteLn('P2 est plus proche de P1 que P3')
  else
    WriteLn('P3 est plus proche de P1 que P2');
  WriteLn;

  WriteLn('=== Clonage ===');
  P3.Free;
  P3 := P2.Cloner;  // P3 est maintenant une copie de P2
  WriteLn('P3 après clonage de P2 :');
  P3.Afficher;
  WriteLn;

  WriteLn('=== Copie de coordonnées ===');
  P1.CopierVers(P2);  // Copie P1 dans P2
  WriteLn('P2 après copie depuis P1 :');
  P2.Afficher;
  WriteLn;

  // Libération
  P1.Free;
  P2.Free;
  P3.Free;

  WriteLn('Programme terminé');
end.
