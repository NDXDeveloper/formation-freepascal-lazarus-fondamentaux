{ ============================================================================
  Section 5.4 : Enregistrements (Records)
  Description : Instruction WITH pour simplifier l'acces aux champs
  Fichier source : 04-enregistrements-records.md
  ============================================================================ }
program ExempleWith;  
type  
  TProduit = record
    code: String;
    designation: String;
    prix: Real;
    stock: Integer;
  end;

var
  article: TProduit;
begin
  with article do
  begin
    code := 'P001';
    designation := 'Clavier mécanique';
    prix := 89.99;
    stock := 15;

    WriteLn('Produit : ', designation);
    WriteLn('Code : ', code);
    WriteLn('Prix : ', prix:0:2, ' €');
    WriteLn('Stock : ', stock, ' unités');
  end;
end.
