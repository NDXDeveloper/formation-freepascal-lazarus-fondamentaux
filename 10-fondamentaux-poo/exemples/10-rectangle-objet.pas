{ ============================================================================
  Section 10.10 : Comparaison procedural vs objet
  Description : Gestion d'un rectangle - approche orientee objet (classe)
  Fichier source : 10-comparaison-procedural-vs-objet.md
  ============================================================================ }
program RectangleObjet;

{$mode objfpc}{$H+}

type
  TRectangle = class
  private
    FLargeur: Real;
    FHauteur: Real;
  public
    constructor Create(L, H: Real);
    function CalculerSurface: Real;
    function CalculerPerimetre: Real;
    procedure Redimensionner(Facteur: Real);
    procedure Afficher;
    property Largeur: Real read FLargeur write FLargeur;
    property Hauteur: Real read FHauteur write FHauteur;
  end;

constructor TRectangle.Create(L, H: Real);  
begin  
  inherited Create;
  FLargeur := L;
  FHauteur := H;
end;

function TRectangle.CalculerSurface: Real;  
begin  
  Result := FLargeur * FHauteur;
end;

function TRectangle.CalculerPerimetre: Real;  
begin  
  Result := 2 * (FLargeur + FHauteur);
end;

procedure TRectangle.Redimensionner(Facteur: Real);  
begin  
  FLargeur := FLargeur * Facteur;
  FHauteur := FHauteur * Facteur;
end;

procedure TRectangle.Afficher;  
begin  
  WriteLn('Rectangle:');
  WriteLn('  Largeur: ', FLargeur:0:2);
  WriteLn('  Hauteur: ', FHauteur:0:2);
  WriteLn('  Surface: ', CalculerSurface:0:2);
  WriteLn('  Périmètre: ', CalculerPerimetre:0:2);
end;

var
  MonRectangle: TRectangle;
begin
  MonRectangle := TRectangle.Create(10, 5);
  try
    MonRectangle.Afficher;

    MonRectangle.Redimensionner(2);
    WriteLn;
    WriteLn('Après redimensionnement:');
    MonRectangle.Afficher;
  finally
    MonRectangle.Free;
  end;
end.
