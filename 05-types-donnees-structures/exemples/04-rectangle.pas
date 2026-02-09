{ ============================================================================
  Section 5.4 : Enregistrements (Records)
  Description : Calcul de surface et perimetre d'un rectangle
  Fichier source : 04-enregistrements-records.md
  ============================================================================ }
program Rectangle;
type
  TRectangle = record
    largeur: Real;
    hauteur: Real;
  end;

function Surface(r: TRectangle): Real;
begin
  Surface := r.largeur * r.hauteur;
end;

function Perimetre(r: TRectangle): Real;
begin
  Perimetre := 2 * (r.largeur + r.hauteur);
end;

function EstCarre(r: TRectangle): Boolean;
begin
  EstCarre := r.largeur = r.hauteur;
end;

var
  rect: TRectangle;
begin
  Write('Largeur : ');
  ReadLn(rect.largeur);
  Write('Hauteur : ');
  ReadLn(rect.hauteur);

  WriteLn('Surface : ', Surface(rect):0:2);
  WriteLn('Périmètre : ', Perimetre(rect):0:2);

  if EstCarre(rect) then
    WriteLn('C''est un carré !')
  else
    WriteLn('Ce n''est pas un carré');
end.
