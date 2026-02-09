{ ============================================================================
  Section 2.9 : Conventions de nommage
  Description : Gestion d'un article avec type personnalise (record),
                prefixes T pour types, conventions PascalCase et camelCase
  Fichier source : 09-conventions-nommage.md
  ============================================================================ }
{$mode objfpc}{$H+}
program GestionArticles;

const
  TAUX_TVA = 20.0;
  STOCK_MINIMUM = 5;

type
  TArticle = record
    code: string;
    designation: string;
    prixHT: real;
    quantiteStock: integer;
  end;

var
  article: TArticle;
  prixTTC: real;
  estEnRupture: boolean;

{ var devant le paramètre = passage par référence : les modifications
  sont répercutées sur la variable de l'appelant }
procedure InitialiserArticle(var unArticle: TArticle);
begin
  unArticle.code := 'ART001';
  unArticle.designation := 'Clavier USB';
  unArticle.prixHT := 25.00;
  unArticle.quantiteStock := 15;
end;

function CalculerPrixTTC(prixHT: real): real;
begin
  { Result est la variable spéciale pour renvoyer la valeur d'une fonction }
  Result := prixHT * (1 + TAUX_TVA / 100);
end;

function EstEnRuptureStock(quantite: integer): boolean;
begin
  Result := quantite < STOCK_MINIMUM;
end;

procedure AfficherArticle(unArticle: TArticle);
begin
  WriteLn('Code : ', unArticle.code);
  WriteLn('Désignation : ', unArticle.designation);
  WriteLn('Prix HT : ', unArticle.prixHT:0:2, ' €');
  WriteLn('Prix TTC : ', CalculerPrixTTC(unArticle.prixHT):0:2, ' €');
  WriteLn('Stock : ', unArticle.quantiteStock);

  if EstEnRuptureStock(unArticle.quantiteStock) then
    WriteLn('ATTENTION : Stock faible !');
end;

begin
  InitialiserArticle(article);
  AfficherArticle(article);
end.
