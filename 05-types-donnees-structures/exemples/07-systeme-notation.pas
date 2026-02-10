{ ============================================================================
  Section 5.7 : Types enumeres
  Description : Systeme de notation avec mention basee sur un type enumere
  Fichier source : 07-types-enumeres.md
  ============================================================================ }
program SystemeNotation;  
type  
  TMention = (Insuffisant, Passable, AssezBien, Bien, TresBien, Excellent);

function ObtenirMention(note: Real): TMention;  
begin  
  if note < 10 then
    ObtenirMention := Insuffisant
  else if note < 12 then
    ObtenirMention := Passable
  else if note < 14 then
    ObtenirMention := AssezBien
  else if note < 16 then
    ObtenirMention := Bien
  else if note < 18 then
    ObtenirMention := TresBien
  else
    ObtenirMention := Excellent;
end;

procedure AfficherMention(m: TMention);  
begin  
  Write('Mention : ');
  case m of
    Insuffisant: WriteLn('Insuffisant');
    Passable:    WriteLn('Passable');
    AssezBien:   WriteLn('Assez bien');
    Bien:        WriteLn('Bien');
    TresBien:    WriteLn('Très bien');
    Excellent:   WriteLn('Excellent');
  end;
end;

var
  note: Real;
  mention: TMention;
begin
  Write('Entrez une note sur 20 : ');
  ReadLn(note);

  mention := ObtenirMention(note);
  AfficherMention(mention);
end.
