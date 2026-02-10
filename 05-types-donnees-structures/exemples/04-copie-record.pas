{ ============================================================================
  Section 5.4 : Enregistrements (Records)
  Description : Copie d'enregistrements et independance des copies
  Fichier source : 04-enregistrements-records.md
  ============================================================================ }
program CopieRecord;  
type  
  TPoint = record
    x: Real;
    y: Real;
  end;

var
  point1, point2: TPoint;
begin
  // Définir le premier point
  point1.x := 10.5;
  point1.y := 20.3;

  // Copier tous les champs en une seule instruction
  point2 := point1;

  WriteLn('Point 1 : (', point1.x:0:1, ', ', point1.y:0:1, ')');
  WriteLn('Point 2 : (', point2.x:0:1, ', ', point2.y:0:1, ')');

  // Modification de point2 n'affecte pas point1
  point2.x := 30.0;
  WriteLn('Après modification de point2 :');
  WriteLn('Point 1 : (', point1.x:0:1, ', ', point1.y:0:1, ')');
  WriteLn('Point 2 : (', point2.x:0:1, ', ', point2.y:0:1, ')');
end.
