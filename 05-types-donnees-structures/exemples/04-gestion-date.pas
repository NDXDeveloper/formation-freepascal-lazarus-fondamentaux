{ ============================================================================
  Section 5.4 : Enregistrements (Records)
  Description : Gestion d'une date avec validation et affichage
  Fichier source : 04-enregistrements-records.md
  ============================================================================ }
program GestionDate;
type
  TDate = record
    jour: Integer;
    mois: Integer;
    annee: Integer;
  end;

function DateValide(d: TDate): Boolean;
begin
  DateValide := (d.jour >= 1) and (d.jour <= 31) and
                (d.mois >= 1) and (d.mois <= 12) and
                (d.annee > 0);
end;

procedure AfficherDate(d: TDate);
begin
  WriteLn(d.jour, '/', d.mois, '/', d.annee);
end;

var
  aujourd: TDate;
begin
  aujourd.jour := 15;
  aujourd.mois := 3;
  aujourd.annee := 2025;

  if DateValide(aujourd) then
  begin
    Write('Date valide : ');
    AfficherDate(aujourd);
  end
  else
    WriteLn('Date invalide');
end.
