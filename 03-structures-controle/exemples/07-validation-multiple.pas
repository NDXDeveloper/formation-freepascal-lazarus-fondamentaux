{ ============================================================================
  Section 3.7 : Gestion des erreurs simples
  Description : Validation multiple d'une note avec plusieurs criteres
  Fichier source : 07-gestion-erreurs-simples.md
  ============================================================================ }
program ValidationMultiple;  
var  
  note: Real;
  erreur: String;
begin
  WriteLn('Entrez une note (0.0 à 20.0) :');

  repeat
    erreur := '';  // Pas d'erreur par défaut
    Write('Note : ');
    ReadLn(note);

    // Vérifications multiples
    if note < 0 then
      erreur := 'La note ne peut pas être négative'
    else if note > 20 then
      erreur := 'La note ne peut pas dépasser 20'
    // Trunc supprime les décimales sans arrondir ; cette astuce détecte plus d'1 décimale
    else if (note * 10) <> Trunc(note * 10) then
      erreur := 'Utilisez maximum 1 décimale (ex: 15.5)';

    if erreur <> '' then
      WriteLn('❌ ERREUR : ', erreur)
    else
      WriteLn('✓ Note valide : ', note:0:1, '/20');
  until erreur = '';
end.
