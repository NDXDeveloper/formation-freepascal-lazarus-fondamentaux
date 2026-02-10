{ ============================================================================
  Section 10.10 : Comparaison procedural vs objet
  Description : Gestion d'un rectangle - approche procedurale (record + procedures)
  Fichier source : 10-comparaison-procedural-vs-objet.md
  ============================================================================ }
program RectangleProcedural;

{$mode objfpc}{$H+}

type
  TRectangle = record
    Largeur: Real;
    Hauteur: Real;
  end;

// Procédures et fonctions séparées
procedure InitialiserRectangle(var R: TRectangle; L, H: Real);  
begin  
  R.Largeur := L;
  R.Hauteur := H;
end;

function CalculerSurface(R: TRectangle): Real;  
begin  
  Result := R.Largeur * R.Hauteur;
end;

function CalculerPerimetre(R: TRectangle): Real;  
begin  
  Result := 2 * (R.Largeur + R.Hauteur);
end;

procedure Redimensionner(var R: TRectangle; Facteur: Real);  
begin  
  R.Largeur := R.Largeur * Facteur;
  R.Hauteur := R.Hauteur * Facteur;
end;

procedure AfficherRectangle(R: TRectangle);  
begin  
  WriteLn('Rectangle:');
  WriteLn('  Largeur: ', R.Largeur:0:2);
  WriteLn('  Hauteur: ', R.Hauteur:0:2);
  WriteLn('  Surface: ', CalculerSurface(R):0:2);
  WriteLn('  Périmètre: ', CalculerPerimetre(R):0:2);
end;

var
  MonRectangle: TRectangle;
begin
  InitialiserRectangle(MonRectangle, 10, 5);
  AfficherRectangle(MonRectangle);

  Redimensionner(MonRectangle, 2);
  WriteLn;
  WriteLn('Après redimensionnement:');
  AfficherRectangle(MonRectangle);
end.
