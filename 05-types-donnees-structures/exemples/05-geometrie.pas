{ ============================================================================
  Section 5.5 : Enregistrements imbriques
  Description : Gestion de points geometriques, lignes et rectangles imbriques
  Fichier source : 05-enregistrements-imbriques.md
  ============================================================================ }
program Geometrie;  
type  
  TPoint = record
    x: Real;
    y: Real;
  end;

  TLigne = record
    depart: TPoint;
    arrivee: TPoint;
  end;

  TRectangle = record
    coinHautGauche: TPoint;
    coinBasDroite: TPoint;
  end;

function Distance(p1, p2: TPoint): Real;  
begin  
  Distance := Sqrt(Sqr(p2.x - p1.x) + Sqr(p2.y - p1.y));
end;

function LongueurLigne(ligne: TLigne): Real;  
begin  
  LongueurLigne := Distance(ligne.depart, ligne.arrivee);
end;

function SurfaceRectangle(rect: TRectangle): Real;  
var  
  largeur, hauteur: Real;
begin
  largeur := Abs(rect.coinBasDroite.x - rect.coinHautGauche.x);
  hauteur := Abs(rect.coinBasDroite.y - rect.coinHautGauche.y);
  SurfaceRectangle := largeur * hauteur;
end;

var
  ligne: TLigne;
  rectangle: TRectangle;
begin
  // Définir une ligne
  ligne.depart.x := 0;
  ligne.depart.y := 0;
  ligne.arrivee.x := 3;
  ligne.arrivee.y := 4;

  WriteLn('Longueur de la ligne : ', LongueurLigne(ligne):0:2);

  // Définir un rectangle
  rectangle.coinHautGauche.x := 0;
  rectangle.coinHautGauche.y := 10;
  rectangle.coinBasDroite.x := 5;
  rectangle.coinBasDroite.y := 0;

  WriteLn('Surface du rectangle : ', SurfaceRectangle(rectangle):0:2);
end.
