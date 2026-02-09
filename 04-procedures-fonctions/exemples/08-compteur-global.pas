{ ============================================================================
  Section 4.8 : Variables locales vs globales
  Description : Variable globale compteur partagee entre procedures
  Fichier source : 08-variables-locales-vs-globales.md
  ============================================================================ }
program CompteurGlobal;

var
  compteur: Integer;  // Variable globale

procedure Incrementer;
begin
  compteur := compteur + 1;
end;

procedure Afficher;
begin
  WriteLn('Compteur : ', compteur);
end;

begin
  compteur := 0;
  Afficher;       // Compteur : 0

  Incrementer;
  Afficher;       // Compteur : 1

  Incrementer;
  Incrementer;
  Afficher;       // Compteur : 3
end.
