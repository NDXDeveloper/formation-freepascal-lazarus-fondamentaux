{ ============================================================================
  Section 6.5 : Pointeurs et Enregistrements
  Description : File d'attente (Queue) avec operations Enfiler/Defiler
  Fichier source : 05-pointeurs-enregistrements.md
  ============================================================================ }
{$mode objfpc}{$H+}
program FileAttente;  
type  
  PNoeud = ^TNoeud;
  TNoeud = record
    donnee: String;
    suivant: PNoeud;
  end;

  TFile = record
    premier: PNoeud;
    dernier: PNoeud;
  end;

procedure Enfiler(var f: TFile; valeur: String);  
var  
  nouveau: PNoeud;
begin
  New(nouveau);
  nouveau^.donnee := valeur;
  nouveau^.suivant := nil;

  if f.dernier = nil then
  begin
    f.premier := nouveau;
    f.dernier := nouveau;
  end
  else
  begin
    f.dernier^.suivant := nouveau;
    f.dernier := nouveau;
  end;
end;

function Defiler(var f: TFile): String;  
var  
  temp: PNoeud;
begin
  if f.premier = nil then
  begin
    Result := '';
    Exit;
  end;

  Result := f.premier^.donnee;
  temp := f.premier;
  f.premier := f.premier^.suivant;

  if f.premier = nil then
    f.dernier := nil;

  Dispose(temp);
end;

var
  maFile: TFile;
begin
  maFile.premier := nil;
  maFile.dernier := nil;

  Enfiler(maFile, 'Client 1');
  Enfiler(maFile, 'Client 2');
  Enfiler(maFile, 'Client 3');

  WriteLn('Service : ', Defiler(maFile));  // Client 1
  WriteLn('Service : ', Defiler(maFile));  // Client 2
end.
