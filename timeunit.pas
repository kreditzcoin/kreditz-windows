unit TimeUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,fphttpclient,IdSNTP,dateutils;

function GetNetworkTimestamp():String;
function GetLocalTimestamp():string;
function GetTimestamp():string;
Procedure InicializeTime();

var
  GLOBAL_TimeDiff : Integer = 0;

implementation

// INITIALIZE TIME
Procedure InicializeTime();
Begin
GLOBAL_TimeDiff := StrToInt64(GetNetworkTimestamp())-StrToInt64(GetLocalTimestamp());
end;

// RETURNS GLOBAL TIMESTAMP
function GetNetworkTimestamp():String;
var
  NTPClient: TIdSNTP;
  OffSet : Integer;
begin
OffSet := GetLocalTimeOffset * 60;
NTPClient := TIdSNTP.Create(nil);
   try
   NTPClient.Host := 'ntp.amnic.net';
   NTPClient.Active := True;
   result := IntToStr(DateTimeToUnix(NTPClient.DateTime)+Offset)+IntToStr(100+Random(899));
   finally
   NTPClient.Free;
   end;
end;

// RETURNS LOCAL TIMESTAMP
function GetLocalTimestamp():string;
Begin
GetLocalTimestamp := inttostr(Trunc((Now - EncodeDate(1970, 1 ,1)) * 24 * 60 * 60))+'000';
end;

// RETURNS P2P TIMESTAMP
function GetTimestamp():string;
Begin
Result := IntToStr(StrToInt64(GetLocalTimestamp)+GLOBAL_TimeDiff);
end;

END. // END UNIT

