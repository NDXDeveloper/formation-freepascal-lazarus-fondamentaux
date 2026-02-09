{ ============================================================================
  Section 5.3 : Chaines de caracteres (String, ShortString)
  Description : Differences entre String et ShortString
  Fichier source : 03-chaines-caracteres-string.md
  ============================================================================ }
program DifferenceTypes;
var
  court: ShortString;
  long: String;
begin
  // ShortString : max 255 caractères
  court := 'Texte court';
  // court := 'Texte très très très long...' (+ de 255 car) -> ERREUR !

  // String : pas de limite pratique
  long := 'Texte extrêmement long sans limite...';

  WriteLn('ShortString : ', Length(court));
  WriteLn('String : ', Length(long));
end.
