{ ============================================================================
  Section 3.3 : Boucles compteur (for-do)
  Description : Compte a rebours avec downto
  Fichier source : 03-boucles-compteur-for-do.md
  ============================================================================ }

program CompteARebours;  
var  
  i: Integer;
begin
  WriteLn('Compte à rebours pour le lancement :');
  WriteLn;
  { downto : compte a rebours, i est decremente de 1 a chaque iteration }
  for i := 10 downto 1 do
  begin
    WriteLn(i, '...');
  end;
  WriteLn('DÉCOLLAGE !');
end.
