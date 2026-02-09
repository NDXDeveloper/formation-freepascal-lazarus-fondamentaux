{ ============================================================================
  Section 5.7 : Types enumeres
  Description : Gestion de feux tricolores avec cycle automatique
  Fichier source : 07-types-enumeres.md
  ============================================================================ }
program FeuxTricolores;
type
  TFeu = (Rouge, Orange, Vert);

var
  etatFeu: TFeu;

procedure AfficherFeu(feu: TFeu);
begin
  Write('Feu ');
  case feu of
    Rouge:  WriteLn('ROUGE - STOP');
    Orange: WriteLn('ORANGE - Attention');
    Vert:   WriteLn('VERT - Passez');
  end;
end;

function FeuSuivant(feu: TFeu): TFeu;
begin
  case feu of
    Rouge:  FeuSuivant := Vert;
    Vert:   FeuSuivant := Orange;
    Orange: FeuSuivant := Rouge;
  end;
end;

begin
  etatFeu := Rouge;
  WriteLn('État initial :');
  AfficherFeu(etatFeu);

  WriteLn;
  WriteLn('Cycle du feu :');
  etatFeu := FeuSuivant(etatFeu);
  AfficherFeu(etatFeu);

  etatFeu := FeuSuivant(etatFeu);
  AfficherFeu(etatFeu);

  etatFeu := FeuSuivant(etatFeu);
  AfficherFeu(etatFeu);
end.
