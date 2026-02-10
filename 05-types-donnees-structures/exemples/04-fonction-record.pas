{ ============================================================================
  Section 5.4 : Enregistrements (Records)
  Description : Fonction retournant un enregistrement et calcul de distance
  Fichier source : 04-enregistrements-records.md
  ============================================================================ }
program FonctionRecord;  
type  
  TPoint = record
    x: Real;
    y: Real;
  end;

function CreerPoint(valX, valY: Real): TPoint;  
var  
  p: TPoint;
begin
  p.x := valX;
  p.y := valY;
  CreerPoint := p;  // Retourner l'enregistrement
end;

function Distance(p1, p2: TPoint): Real;  
begin  
  Distance := Sqrt(Sqr(p2.x - p1.x) + Sqr(p2.y - p1.y));
end;

var
  pointA, pointB: TPoint;
  dist: Real;
begin
  pointA := CreerPoint(0, 0);
  pointB := CreerPoint(3, 4);

  dist := Distance(pointA, pointB);
  WriteLn('Distance entre les points : ', dist:0:2);  // 5.00
end.
