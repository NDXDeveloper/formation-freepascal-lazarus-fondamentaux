uses  
  SysUtils, BaseUnix, Unix;

function ObtenirVersionOS: string;  
var  
  Info: UtsName;
begin
  if FpUname(Info) = 0 then
    Result := Format('%s %s', [Info.sysname, Info.release])
  else
    Result := 'Unix version inconnue';
end;

function ObtenirNomMachine: string;  
var  
  Info: UtsName;
begin
  if FpUname(Info) = 0 then
    Result := Info.nodename
  else
    Result := 'Inconnu';
end;
