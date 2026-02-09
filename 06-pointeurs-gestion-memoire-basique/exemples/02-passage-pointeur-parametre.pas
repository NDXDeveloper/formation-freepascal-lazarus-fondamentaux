{ ============================================================================
  Section 6.2 : Declaration et Utilisation de Pointeurs
  Description : Passage de pointeur en parametre de procedure
  Fichier source : 02-declaration-utilisation-pointeurs.md
  ============================================================================ }
program PassagePointeurParametre;
type
  TGrosseStructure = record
    donnees: array[1..1000] of Integer;
  end;
  PGrosseStructure = ^TGrosseStructure;

procedure TraiterDonnees(p: PGrosseStructure);
begin
  // Accès direct aux données via le pointeur
  if p <> nil then
  begin
    p^.donnees[1] := 999;  // p^ déréférence, puis .donnees accède au champ du record
    WriteLn('Première donnée modifiée');
  end;
end;

var
  maStructure: TGrosseStructure;
begin
  TraiterDonnees(@maStructure);
  WriteLn(maStructure.donnees[1]);  // Affiche 999
end.
