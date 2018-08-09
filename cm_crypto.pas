unit CM_Crypto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MD5, StrUtils, DateUtils, Process, fileutil,DCPsha256, DCPripemd160;

function HashMD5File(FileToHash:String):String;
function HashMD5String(StringToHash:String):String;
function RunMiner(aParam:Pointer):PtrInt;
Procedure CloseAllMiningThreads();
function VerifyMinerResult(Solution,Difficulty,Target:String;block:int64):int64;
function RunOpenSSLCommand(textline:String):boolean;
function CreateKeysPair():Boolean;
function GetPublicKeyFromPem():String;
function GetPrivateKeyFromPem():String;
function GetHashSeed(longitude:int64):String;
function HashSha256String(StringToHash:string):string;
function GetAddressFromPublicKey(PubKey:String):String;
function IsValidAddress(Address:String):boolean;
function GetStringSigned(StringtoSign, PrivateKey:String):String;
function GetBase64TextFromFile(fileb64:string):string;
function VerifySignedString(StringToVerify,B64String,PublicKey:String):boolean;

implementation

Uses
  Master, MC_Main, Protocol, CommandLineParser;

// RETURN THE MD5 HASH OF A FILE
Function HashMD5File(FileToHash:String):String;
Begin
result := UpperCase(MD5Print(MD5File(FileToHash)));
End;

// RETURNS THE MD5 HASH OF A STRING
Function HashMD5String(StringToHash:String):String;
Begin
result := Uppercase(MD5Print(MD5String(StringToHash)));
end;

// RETURNS THE SHA256 OF A STRING
function HashSha256String(StringToHash:string):string;
var
  Hash: TDCP_sha256;
  Digest: array[0..31] of byte;  // sha256 produces a 256bit digest (32bytes)
  Source: string;
  i: integer;
  str1: string;
begin
Source:= StringToHash;  // here your string for get sha256
if Source <> '' then
   begin
   Hash:= TDCP_sha256.Create(nil);  // create the hash
   Hash.Init;                        // initialize it
   Hash.UpdateStr(Source);
   Hash.Final(Digest);               // produce the digest
   str1:= '';
   for i:= 0 to 31 do
   str1:= str1 + IntToHex(Digest[i],2);
      //form1.Edit2.Text:= s;                   // display the digest in lower case
   Result:=UpperCase(str1);         // display the digest in capital letter
   end;
end;

// RUNS POW
function RunMiner(aParam:Pointer):PtrInt;
var
  HashValue : String = '';
  Solution : String = '';
begin
while MINER_FoundedSteps < MINER_Steps do
   begin
   while ((MINER_HashCounter < 99999999) and (OptionsData.Mining) and (STATUS_Updated)
     and (MINER_IsMinerOn) and (not MINER_BlockFound)) do
      begin
      Solution := MINER_HashSeed+IntToStr(MINER_HashCounter);
      HashValue := HashMD5String(Solution);
      if AnsiContainsStr(HashValue,MINER_CurrStepTarget) then // Step founded
         begin
         MINER_ResultHash := MINER_ResultHash+Solution;
         MINER_FoundedSteps := MINER_FoundedSteps+1;
         if MINER_FoundedSteps = MINER_Steps then
            begin
            MINER_BlockFound := true;
            end
         else
            begin
            MINER_CurrStepTarget:= Copy(HashMD5String(Solution+IntToStr(MINER_BlockDiffSet)),1,MINER_TargetChars); // voy por aqui
            MINER_HashCounter := 10000000;
            MINER_HashSeed := GetHashSeed(MINER_TargetChars);
            end;
         end;
      MINER_HashCounter := MINER_HashCounter+1;
      If MINER_HashCounter = 99999999 then
         begin
         MINER_HashCounter := 0;
         MINER_HashSeed := GetHashSeed(MINER_TargetChars);
         end;
      end;
   end;
MINER_IsMinerOn := false;
end;

// CLOSE ALL OPENEND MINING THREADS
Procedure CloseAllMiningThreads();
Var
  Counter : integer;
Begin
for counter := 0 to Length(ArrayThreads)-1 do
   begin
      Try
      KillThread(ArrayThreads[counter]);
      OutputText('Thread Closed',false);
      except on E:Exception do
         begin

         end;
      end;
   end;
SetLength(ArrayThreads,0);
End;

// VERIFY THE BLOCK SOLUTION
function VerifyMinerResult(Solution,Difficulty,Target:String;block:int64):int64;
var
  chars,steps,solutionlength:int64;
  SLSolutions : TStringList;
  Contador : integer;
  FisrtChar : int64;
  CurrTarget : String;
Begin
Result := 0;
SLSolutions := TStringList.Create;
chars := GetCharsFromMinerDiff(Difficulty);
steps := GetStepsFromMinerDiff(Difficulty);
solutionlength := chars+8;
for contador := 0 to steps-1 do
   begin
   FisrtChar := 1+(contador*solutionlength);
   SLSolutions.Add(copy(Solution,FisrtChar,solutionlength));
   end;
Currtarget := Target;
for contador := 0 to SLSolutions.Count-1 do
   begin
   if AnsiContainsStr(HashMD5String(SLSolutions[contador]),currtarget) then  // correct step
      begin
      currtarget := copy(HashMD5String(SLSolutions[contador]+IntToStr(block)),1,chars);
      end
   else
      Begin
      OutputText('FAILED>>'+Currtarget+': '+SLSolutions[contador]+'=>'+HashMD5String(SLSolutions[contador]),false);
      SLSolutions.Free;
      exit;
      end;
   end;
Outputtext('Solution Verified Block '+IntToStr(block),false);
SLSolutions.Free;
End;

// EXECUTES A OPENSSL COMMAND
function RunOpenSSLCommand(textline:String):boolean;
var
  ArrParameters : Array of string;
  contador : integer = 1;
  ThisParam : String = '';
  MoreParam: boolean = true;
  MyProcess : TProcess;
  Resultado, Errores : TStringList;
Begin
result := false;
Resultado := TStringList.Create;
Errores := TStringList.Create;
SetLength(ArrParameters,0);
while MoreParam do
   begin
   ThisParam := GetParameterFromCommandLine(textline,contador);
   if thisparam = '' then MoreParam := false
   else
     begin
     SetLength(ArrParameters,length(ArrParameters)+1);
     ArrParameters[length(ArrParameters)-1] := ThisParam;
     end;
   contador := contador+1;
   end;
MyProcess:= TProcess.Create(nil);
MyProcess.Executable := OptionsData.OpenSSLPath;
For contador := 0 to length(ArrParameters)-1 do
  MyProcess.Parameters.Add(ArrParameters[contador]);
MyProcess.Options := MyProcess.Options + [poWaitOnExit, poUsePipes, poNoConsole];
MyProcess.Execute;
Resultado.LoadFromStream(MyProcess.Output);
Errores.LoadFromStream(MyProcess.stderr);
if ((Resultado.Count>0) and (Resultado[0] = 'Verified OK')) then result := true;
while Resultado.Count > 0 do
   begin
   OutPutText(Resultado[0],false);
   Resultado.Delete(0);
   end;
if Errores.Count > 0 then
   begin
   OutputText('ERRORS',false);
   while Errores.Count > 0 do
      begin
      OutPutText(Errores[0],false);
      Errores.Delete(0);
      end;
   end;
MyProcess.Free;Resultado.Free; Errores.Free;
end;

// CREATES KEYS PAIR
Function CreateKeysPair():Boolean;
var
  MyProcess, MyProcess2 : TProcess;
Begin
//Generates the private
MyProcess:= TProcess.Create(nil);
MyProcess.Executable := OptionsData.OpenSSLPath;
MyProcess.Parameters.Add('ecparam');
MyProcess.Parameters.Add('-name');
MyProcess.Parameters.Add('secp256k1');
MyProcess.Parameters.Add('-genkey');
MyProcess.Parameters.Add('-noout');
MyProcess.Parameters.Add('-out');
MyProcess.Parameters.Add('DATA/private.pem');
MyProcess.Options := MyProcess.Options + [poWaitOnExit, poUsePipes, poNoConsole];
MyProcess.Execute;
// Extract public key
MyProcess2:= TProcess.Create(nil);
MyProcess2.Executable := OptionsData.OpenSSLPath;
MyProcess2.Parameters.Add('ec');
MyProcess2.Parameters.Add('-in');
MyProcess2.Parameters.Add('DATA/private.pem');
MyProcess2.Parameters.Add('-pubout');
MyProcess2.Parameters.Add('-out');
MyProcess2.Parameters.Add('DATA/public.pem');
MyProcess2.Options := MyProcess2.Options + [poWaitOnExit, poUsePipes, poNoConsole];
MyProcess2.Execute;
if ((FileExists('DATA/private.pem')) and (FileExists('DATA/public.pem'))) then result := true
else result := false;
MyProcess.Free;MyProcess2.Free;
End;

// RETURNS THE PUBLIC KEY WHEN CREATED
Function GetPublicKeyFromPem():String;
var
  KeyFile: TextFile;
  LineText, Resultado : String;
Begin
Resultado := '';
AssignFile(KeyFile,'DATA/public.pem');
   Try
   reset(Keyfile);
   while not eof(KeyFile) do
      begin
      readln(KeyFile, LineText);
      If AnsiContainsStr(LineText,'-----') = false then
         Resultado := Resultado + LineText;
      end;
   Closefile(Keyfile);
   except
   on E: EInOutError do
      OutPutText('Public key file not found');
   end;
if Resultado <>'' then Result := Resultado
else result := 'Err';
end;

// RETURNS THE PRIVATE KEY WHEN CREATED
Function GetPrivateKeyFromPem():String;
var
  KeyFile: TextFile;
  LineText, Resultado : String;
Begin
Resultado := '';
AssignFile(KeyFile,'DATA/private.pem');
   Try
   reset(Keyfile);
   while not eof(KeyFile) do
      begin
      readln(KeyFile, LineText);
      If AnsiContainsStr(LineText,'-----') = false then
         Resultado := Resultado + LineText;
      end;
   Closefile(Keyfile);
   except
   on E: EInOutError do
      OutPutText('Public key file not found');
   end;
if Resultado <>'' then Result := Resultado
else result := 'Err';
end;

// RETURN THE HASH SEED FOR THE MINER
function GetHashSeed(longitude:int64):String;
var
  contador : integer;
begin
result := '';
for contador := 1 to longitude do
   begin
   result := result+chr(random(26)+65);
   end;
end;

// RETURNS AN ADDRESS FROM A PUBLIC LEY
function GetAddressFromPublicKey(PubKey:String):String;
var
  PubSHAHashed,Hash1,Hash2,KeyChar1,KeyChar2:String;
Begin
PubSHAHashed := HashSha256String(PubKey);
Hash1 := HashMD5String(PubSHAHashed);
Hash2 := HashMD5String('K'+Hash1);
KeyChar1 := Copy(Hash2,16,1);
KeyChar2 := Copy(Hash2,17,1);
Result := 'K'+KeyChar1+Hash1+KeyChar2;
End;

// RETURNS IF AN ADDRESS IS VALID OR NOT
function IsValidAddress(Address:String):boolean;
var
  OrigHash, NewHash : String;
  KeyChar1,KeyChar2:String;
Begin
OrigHash := Copy(Address,3,32);
NewHash := HashMD5String('K'+OrigHash);
KeyChar1 := Copy(NewHash,16,1);
KeyChar2 := Copy(NewHash,17,1);
NewHash := 'K'+KeyChar1+OrigHash+KeyChar2;
If NewHash = Address then result := true else result := false;
End;

// GET THE BASE64STRING OF A SIGNED STRING
function GetStringSigned(StringtoSign, PrivateKey:String):String;
var
  FileToSign :Textfile;
  FilePrivate : TextFile;
Begin
//creates the file with the string to be signed
AssignFile(FileToSign, 'temp_string.txt');
rewrite(FileToSign);
write(FileToSign,StringtoSign);
CloseFile(FileToSign);
//creates the file with the private key
AssignFile(FilePrivate, 'temp_priv.pem');
rewrite(FilePrivate);
writeln(FilePrivate,'-----BEGIN EC PRIVATE KEY-----');
writeln(FilePrivate,copy(PrivateKey,1,64));
writeln(FilePrivate,copy(PrivateKey,65,64));
writeln(FilePrivate,copy(PrivateKey,129,64));
writeln(FilePrivate,'-----END EC PRIVATE KEY-----');
CloseFile(FilePrivate);
RunOpenSSLCommand('openssl dgst -sha1 -out temp_test.bin -sign temp_priv.pem temp_string.txt');
RunOpenSSLCommand('openssl base64 -in temp_test.bin -out temp_test.b64');
result := GetBase64TextFromFile('temp_test.b64');
Deletefile('temp_string.txt');
Deletefile('temp_priv.pem');
DeleteFile('temp_test.bin');
Deletefile('temp_test.b64');
End;

// RETURNS THE BASE64 STRING FROM A FILE
function GetBase64TextFromFile(fileb64:string):string;
var
  KeyFile: TextFile;
  LineText, Resultado : String;
Begin
Resultado := '';
AssignFile(KeyFile,fileb64);
   Try
   reset(Keyfile);
   while not eof(KeyFile) do
      begin
      readln(KeyFile, LineText);
      If AnsiContainsStr(LineText,'-----') = false then
         Resultado := Resultado + LineText;
      end;
   Closefile(Keyfile);
   except
   on E: EInOutError do
      OutPutText('B64 file not found');
   end;
Result := Resultado;
end;

// VERIFY IF A SIGNED STRING IS VALID
function VerifySignedString(StringToVerify,B64String,PublicKey:String):boolean;
var
  FileToSign :Textfile;
  FilePublic : TextFile;
  FileB64 : Textfile;
Begin
//creates the file with the string to be verified
AssignFile(FileToSign, 'temp_string.txt');
rewrite(FileToSign);
write(FileToSign,StringToVerify);
CloseFile(FileToSign);
//creates the file with the private key
AssignFile(FilePublic, 'temp_pub.pem');
rewrite(FilePublic);
writeln(FilePublic,'-----BEGIN PUBLIC KEY-----');
writeln(FilePublic,copy(PublicKey,1,64));
writeln(FilePublic,copy(PublicKey,65,64));
writeln(FilePublic,'-----END PUBLIC KEY-----');
CloseFile(FilePublic);
//creates the file containing the base64 data
AssignFile(FileB64, 'temp_test.b64');
rewrite(FileB64);
writeln(FileB64,copy(B64String,1,64));
writeln(FileB64,copy(B64String,65,64));
CloseFile(FileB64);
//get the binary file from base64
RunOpenSSLCommand('openssl base64 -d -in temp_test.b64 -out temp_test.bin');
if RunOpenSSLCommand('openssl dgst -sha1 -verify temp_pub.pem -signature temp_test.bin temp_string.txt') then
   begin
   result := true;
   outputText('Signed Verification Ok');
   end
else
   begin
   result := false;
   outputText('Signed Verification FAILED');
   end;
Deletefile('temp_string.txt');
Deletefile('temp_pub.pem');
DeleteFile('temp_test.bin');
Deletefile('temp_test.b64');
End;

END.  // END UNIT

