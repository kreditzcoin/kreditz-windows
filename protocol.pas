unit Protocol;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Master, CM_Crypto, CommandLineParser, TimeUnit, Blocks,
  IdGlobal, Zipper;

function IsValidProtocol(line:String):Boolean;
procedure ParseProtocolConnection(Lines:TStringList;Slot:Integer);
procedure PTC_Join(TextLine:String;slot:integer;Answer:boolean);
procedure PTC_ServerClose(slot:integer);
procedure SendOutGoingMessages();
procedure SendPTCMessage(Slot:Integer;Message:String);
procedure PTC_ACRE(Textline:String;slot:integer);
function TranxAlreadyPending(TypeOfTrx, ValTrx:String;ParamNum:Integer):boolean;
procedure PTC_SendPending(Slot:Integer);
function ReplaceTimestamp(TextLine:String):String;
procedure AddPendingByTimestamp(Pending:String);
procedure PTC_Newblock(TextLine:String);
procedure SendAccDataFile(slot:integer);
procedure PTC_TRFR(Textline:string;slot:integer);
procedure PTC_GetNodes(Textline:string;slot:integer);
procedure PTC_SaveNodes(Textline:string;slot:integer);
function GetComisionValue(Monto:int64):int64;
function SendFundsFromAddress(Destination:String; AddressIndex,Restante:int64):int64;
function GetAddressPaymentsOnPending(Address:String):Int64;
Procedure PTC_SendZipedBlocks(textline:string;slot:integer);
procedure UnzipBlockFile(filename:String);
function GetCharsFromMinerDiff(minerdiff:string):integer;
Function GetStepsFromMinerDiff(minerdiff:string):integer;
function GetNodeFromString(DataString:String):NodeData;
procedure PTC_InBlRe(Textline:string;slot:integer);

implementation

Uses
  MC_Main;

// RETURNS IF A STRING IS A VALID PROTOCOL MESSAGE
function IsValidProtocol(line:String):Boolean;
Var
  Start, Finish:String;
Begin
Start := copy(line,1,4);finish:= copy(line,length(line)-3,4);
if ((start='{MIC') and (finish='END}')) then result := true
else result := false;
End;

// PARSES A PROTOCOL LINE
Procedure ParseProtocolConnection(Lines:TStringList;Slot:Integer);
var
  CommandUsed : String = '';
  TextLine : String = '';
Begin
while lines.Count > 0 do
   begin
   if ((not IsValidProtocol(lines[0])) and (not Conexiones[slot].Autentic)) then
      begin
      OutputText('CONNECTION REJECTED: INVALID PROTOCOL -> '+conexiones[slot].ip);
      if not BLNodeExists(conexiones[slot].ip,'') then AddNewBLNode(conexiones[slot].ip,'');
      CloseConnectionSlot(Slot);
      exit;
      end;
   TextLine := copy(lines[0],6,length(lines[0])-10);
   CommandUsed :=GetCommandLineCommand(TextLine);
   if UpperCase(CommandUsed) = 'JOIN' then PTC_Join(TextLine,slot,true)
   else if UpperCase(CommandUsed) = 'JOIR' then PTC_Join(TextLine,slot,false)
   else if UpperCase(CommandUsed) = 'SERVERCLOSE' then PTC_ServerClose(slot)
   else if UpperCase(CommandUsed) = 'ACRE' then PTC_ACRE(Textline, slot)
   else if UpperCase(CommandUsed) = 'GETPENDING' then PTC_SendPending(slot)
   else if UpperCase(CommandUsed) = 'NWBL' then PTC_Newblock(Textline)
   else if UpperCase(CommandUsed) = 'LASTBLOCK' then PTC_SendZipedBlocks(Textline, slot)
   else if UpperCase(CommandUsed) = 'LASTACC' then SendAccDataFile(slot)
   else if UpperCase(CommandUsed) = 'TRFR' then PTC_TRFR(Textline,slot)
   else if UpperCase(CommandUsed) = 'GETNODES' then PTC_GetNodes(Textline,slot)
   else if UpperCase(CommandUsed) = 'NODES' then PTC_SaveNodes(Textline,slot)
   else if UpperCase(CommandUsed) = 'INVALIDBLOCKREQUEST' then PTC_InBlRe(Textline,slot)
   else Outputtext('Unknown command: '+TextLine+' from '+conexiones[slot].ip);
   if lines.count > 0 then lines.Delete(0);
   end;
end;

// HELLO MESSAGES BETWEN PEERS
Procedure PTC_Join(TextLine:String;slot:integer;Answer:boolean);
var
  conections,block,lastblockhash,account,pending,port,accsumhash:String;
Begin
STATUS_IncomingPings := STATUS_IncomingPings+1;
conections := GetParameterFromCommandLine(TextLine,1);
block := GetParameterFromCommandLine(TextLine,3);
lastblockhash := GetParameterFromCommandLine(TextLine,4);
account := GetParameterFromCommandLine(TextLine,5);
pending := GetParameterFromCommandLine(TextLine,6);
port := GetParameterFromCommandLine(TextLine,7);
accsumhash := GetParameterFromCommandLine(TextLine,8);
if not IsValidInt(conections) then exit;
conexiones[slot].Autentic:=true;
conexiones[slot].Connections:=StrToInt(conections);
conexiones[slot].Lastblock:=block;
conexiones[slot].LastblockHash:=lastblockhash;
conexiones[slot].Accounts:=account;
conexiones[slot].Pending:=pending;
conexiones[slot].ListenPort:=port;
conexiones[slot].lastping:=GetTimeStamp();
conexiones[slot].AccountsHash:=accsumhash;
if not NodeExists(conexiones[slot].ip,conexiones[slot].ListenPort) then
   AddNewNode(conexiones[slot].ip,conexiones[slot].ListenPort);
if Answer then
   Begin
   SendPTCMessage(slot,'{MIC JOIR '+
   IntTostr(GetTotalConex)+' '+
   GetTimeStamp()+' '+
   IntToStr(LOCAL_MyLastBlock)+' '+
   LOCAL_LastBlockHash+' '+
   IntToStr(LOCAL_MyLastAccount)+' '+
   IntToStr(PendingTXs.Count)+' '+
   OptionsData.ListeningPort+' '+
   LOCAL_MyAccsumHash+
   ' END}');
   end;
STATUS_LastPing := StrToInt(copy(conexiones[slot].lastping,1,10));
end;

// SEND A PROTOCOL MESSAGE TO A SPECIFIC SLOT
Procedure SendPTCMessage(Slot:Integer;Message:String);
Begin
if conexiones[Slot].tipo='client' then
   begin
      try
      Conexiones[Slot].context.Connection.IOHandler.WriteLn(Message);
      except
        On E :Exception do
           begin
           outputtext(E.Message);
           ClearConection('client',conexiones[Slot].ip);
           end;
      end;
   end;
if conexiones[Slot].tipo='server' then
   begin
      try
      ConexionesCliente[Slot].IOHandler.WriteLn(Message);
      except
        On E :Exception do
           begin
           outputtext(E.Message);
           ClearConection('server',conexiones[Slot].ip);
           end;
      end;
   end;
end;

// CLIENT RECEIVES A NOTIFICATION THAT A SERVER CLOSED THE CONNECTION
procedure PTC_ServerClose(slot:integer);
Begin
ReadsFromSlots[slot].Clear;
ConexionesCliente[Slot].Disconnect;
OutputText('Outgoing connection closed: '+conexiones[slot].ip+' -> Server disconnected');
ClearConection('server',conexiones[Slot].ip);
End;

// SEND ALL OUTGOING MESSAGES TO ALL AVAILABLE PEERS
Procedure SendOutGoingMessages();
Var
  Slot :Integer = 1;
Begin
While OutGoingMessages.Count > 0 do
   begin
   For Slot := 1 to CONST_MAXConections do
      begin
      if conexiones[Slot].tipo <> '' then SendPTCMessage(Slot,ReplaceTimeStamp(OutGoingMessages[0]));
      end;
   if OutGoingMessages.Count > 0 then OutGoingMessages.Delete(0);
   end;
End;

// PROCESS A NEW ACCOUNT REQUEST
Procedure PTC_ACRE(Textline:String;slot:Integer);
var
  timestamp,ip,publickey,ADhash,OPHash,SignedString:String;
  ResultStr : String;
Begin
timestamp := GetParameterFromCommandLine(TextLine,1);
ip := GetParameterFromCommandLine(TextLine,2);
publickey := GetParameterFromCommandLine(TextLine,3);
ADhash := GetParameterFromCommandLine(TextLine,4);
OPHash := GetParameterFromCommandLine(TextLine,5);
SignedString := GetParameterFromCommandLine(TextLine,6);
if TranxAlreadyPending('ACRE',ADHash,5) then exit;
if GetAddressPubKey(ADHash) <> '' then exit;
if GetAddressFromPublicKey(publickey) <> ADhash then exit;
if not IsValidAddress(ADHash) then exit;
if not VerifySignedString('MY ADDRESS',SignedString,publickey) then exit;
if ip = '0.0.0.0' then ip := conexiones[slot].ip;
ResultStr := ('{MIC ACRE '+
   timestamp+' '+
   Ip+' '+
   publickey+' '+
   ADhash+' '+
   ophash+' '+
   SignedString+
   ' END}');
AddPendingByTimestamp(ResultStr); // ORDERED BY TIMESTAMP
OutGoingMessages.Add(ResultStr);
End;

// REPLACE TIMESTAMP FOR OUTGOING MESSAGES
function ReplaceTimestamp(TextLine:String):String;
Begin
result := StringReplace(TextLine,'$timestamp$',GetTImeStamp(),[rfReplaceAll, rfIgnoreCase]);
End;

// ADD A PENDING TRX BY TIMESTAMP
Procedure AddPendingByTimestamp(Pending:String);
var
  contador : integer = 0;
  Timevalue, TimePending : Int64;
  resultado : integer = 0;
  Insertar : Boolean = false;
Begin
Timevalue := StrToInt64(GetParameterFromCommandLine(Pending,2));
while contador < PendingTXs.Count do
   begin
   TimePending := StrToInt64(GetParameterFromCommandLine(PendingTXs[contador],2));
   if TimeValue < TimePending then
      begin
      Resultado := contador;
      Insertar := true;
      Break;
      end;
   contador := contador + 1;
   end;
if Insertar then PendingTxs.Insert(Resultado,Pending)
else PendingTxs.Add(Pending);
UpdateMyAddresses();
UpdateMyPendingTxs();
End;

// GET A NEW BLOCK MESSAGES
Procedure PTC_Newblock(TextLine:String);
var
  timestamp ,Account, Solution, NewBlHash, blockNumber, TargetHash, Difficulty: String;
  DoIt : boolean = true;
Begin
if not STATUS_Updated then
   begin
   OutGoingMessages.Add('{MIC '+Textline+' END}');
   DoIt := false;
   end;
timestamp := GetParameterFromCommandLine(TextLine,1);
blockNumber := GetParameterFromCommandLine(TextLine,2);
Account := GetParameterFromCommandLine(TextLine,3);
Solution := GetParameterFromCommandLine(TextLine,4);
NewBlHash := GetParameterFromCommandLine(TextLine,5);
TargetHash := GetParameterFromCommandLine(TextLine,6);
Difficulty := GetParameterFromCommandLine(TextLine,7);
if StrToInt(blockNumber) < BlockSumLastBlock() then exit; // if nwbl < last is not valid
if StrToInt(blockNumber) = BlockSumLastBlock() then  // If the same number, detect the valid
   begin
   if NewBlHash = GetBlockData(StrToInt(blockNumber)).BlockHash then // the same
      begin
      OutputText('Block '+(blockNumber)+' same hash received');
      exit;
      end
   else // NOT IDENTICAL
      begin
      if StrToInt64(timestamp) >= StrToInt64(GetBlockData(StrToInt(blockNumber)).TimeEnd) then
         begin
         OutputText('Block '+(blockNumber)+' older received: Omitted');
         exit;
         end
      else if StrToInt64(timestamp) < StrToInt64(GetBlockData(StrToInt(blockNumber)).TimeEnd) then // the new is the good one
         begin
         OutputText('*************************************');
         OutputText('Better block '+blockNumber+' received');
         OutputText('*************************************');
         UndoneLastBlock(StrToInt(blockNumber));
         BuildNewBlock(StrToInt(blockNumber),TimeStamp,Account,Solution,NewBlHash,TargetHash,Difficulty);
         exit;
         end;
      end;
   end;
if StrToInt(blockNumber) = BlockSumLastBlock()+1 then
   begin
   if VerifyMinerResult(Solution,Difficulty,TargetHash,StrToInt(blockNumber)) > 0 then
      begin
      outputtext('Wrong Solution Block '+blockNumber+' : '+solution+' for '+MINER_TargetHash);
      DoIt := false;
      end;
   if DoIT then
      begin
      OutputText('Solution Ok for block: '+blockNumber+ '. Building block');
      BuildNewBlock(StrToInt(blockNumber),TimeStamp,Account,Solution,NewBlHash,TargetHash,Difficulty);
      end;
   end;
End;

// RETURNS IF A TRANSACTION IS ALREADY PENDING / PARAMNUM IS +1 SINCE COMMAND=1
function TranxAlreadyPending(TypeOfTrx, ValTrx:String;ParamNum:Integer):boolean;
var
  contador : integer = 0;
  TypeofPending : String;
  ValPending : String;
Begin
result := false;
if PendingTXs.Count > 0 then
   begin
   for contador := 0 to PendingTXs.Count-1 do
      begin
      TypeofPending := GetParameterFromCommandLine(PendingTxs[contador],1);
      ValPending := GetParameterFromCommandLine(PendingTxs[contador],ParamNum);
      if ((TypeofPending=TypeOfTrx) and (ValPending = ValTrx)) then result := true;
      end;
   end;
End;

// SEND ALL PENDING TRX TO PEER
procedure PTC_SendPending(Slot:Integer);
var
  contador : Integer;
Begin
if PendingTXs.Count > 0 then
   begin
   for contador := 0 to PendingTXs.Count-1 do
      begin
      SendPTCMessage(Slot, PendingTXs[contador]);
      end;
   end;
End;

// SEND ACCDATA FILE
procedure SendAccDataFile(slot:integer);
var
  AFileStream : TFileStream;
begin
AFileStream := TFileStream.Create(CONST_ArchivoAccData, fmOpenRead + fmShareDenyNone);
   try
   if conexiones[Slot].tipo='client' then
      begin
      Conexiones[Slot].context.Connection.IOHandler.WriteLn('FILEACCSUM');
      Conexiones[Slot].context.connection.IOHandler.Write(AFileStream,0,true);
      end;
   if conexiones[Slot].tipo='server' then
      begin
      ConexionesCliente[Slot].IOHandler.WriteLn('FILEACCSUM');
      ConexionesCliente[Slot].IOHandler.Write(AFileStream,0,true);
      end;
   finally
   AFileStream.Free;
   end;
End;

// PROCESS A TRANSFER REQUEST
procedure PTC_TRFR(Textline:string;slot:integer);
var
  TimeStamp, Sender, Destination, Monto, SigHash, OpHash : String;
  Proceder : Boolean = true;
Begin
TimeStamp := GetParameterFromCommandLine(Textline,1);
Sender := GetParameterFromCommandLine(Textline,2);
Destination := GetParameterFromCommandLine(Textline,3);
Monto := GetParameterFromCommandLine(Textline,4);
SigHash := GetParameterFromCommandLine(Textline,5);
OpHash := GetParameterFromCommandLine(Textline,6);
if GetAddressBalanceFromDisk(Sender)-GetAddressPaymentsOnPending(sender) < StrToInt(Monto) then Proceder := false;
if TranxAlreadyPending('TRFR',OpHash,7) then Proceder := false;
// if sighash is invalid then proceder := false;
if not VerifySignedString(TimeStamp+Sender+Destination+Monto,SigHash,GetAddressPubKey(Sender)) then exit;
if proceder then
   begin
   Textline := '{MIC '+Textline+' END}';
   AddPendingByTimestamp(Textline); // ORDERED BY TIMESTAMP
   OutGoingMessages.Add(Textline);
   end;
End;

// RETURNS THE COMISION FOR A TRANSACTION
function GetComisionValue(Monto:int64):int64;
var
  Value : int64;
Begin
Value := monto div 10000;
if Value < 10 then value := 10;
result := value;
End;

// SEND THE FUNDS FROM AN SPECIFIED ADDRESS
function SendFundsFromAddress(Destination:string; AddressIndex,Restante:int64):int64;
var
  MontoFinal : Int64;
  TimeStamp,Sender, SignedHash, TrxHash : String;
Begin
if GetAddressAvailable(ArrayMyAddresses[AddressIndex].Hash) > Restante then MontoFinal := Restante
else MontoFinal := GetAddressAvailable(ArrayMyAddresses[AddressIndex].Hash);
Sender := ArrayMyAddresses[AddressIndex].Hash;
TimeStamp := GetTimeStamp();
SignedHash := GetStringSigned(TimeStamp+Sender+Destination+IntToStr(MontoFinal),ArrayMyAddresses[AddressIndex].PrivateKey);
TrxHash := HashMD5String(TimeStamp+Sender+Destination+IntToStr(MontoFinal)+SignedHash);
outputtext('Send '+IntToStr(montofinal)+' from '+ArrayMyAddresses[AddressIndex].Hash);
OutGoingMessages.Add('{MIC TRFR '+
timestamp+' '+
Sender+' '+
Destination+' '+
IntToStr(MontoFinal)+' '+
SignedHash+' '+
TrxHash+' '+
'END}');
Result := MontoFinal;
End;

// RETURNS THE TOTAL PAYMENTS PENDING FOR AN ADDRESS
function GetAddressPaymentsOnPending(Address:String):Int64;
var
  contador : Integer;
  MontoTotal : Int64 = 0;
  Tipo, Sender : String;
Begin
if PendingTxs.Count > 0 then
   begin
   For contador := 0 to PendingTxs.Count-1 do
      begin
      Tipo := GetParameterFromCommandLine(PendingTxs[contador],1);
      Sender := GetParameterFromCommandLine(PendingTxs[contador],3);
      if ((Tipo = 'TRFR') and (Sender = Address)) then
         begin
         MontoTotal := MontoTotal+ StrToInt64(GetParameterFromCommandLine(PendingTxs[contador],5));
         end;
      end;
   end;
Result := MontoTotal;
End;

// SEND THE BLOCS ZIPPED
Procedure PTC_SendZipedBlocks(textline:string;slot:integer);
var
  FirstBlock, LastBlock : Integer;
  MyZipFile: TZipper;
  contador : integer;
  AFileStream : TFileStream;
begin
if not IsValidInt(GetParameterFromCommandLine(textline,1)) then
   begin
   SendPTCMessage(slot,'{MIC INVALIDBLOCKREQUEST END}');
   exit;
   end;
FirstBlock := StrToInt(GetParameterFromCommandLine(textline,1))+1;
LastBlock := FirstBlock+99;
If LastBlock > LOCAL_MyLastBlock then LastBlock := LOCAL_MyLastBlock;
MyZipFile := TZipper.Create;
MyZipFile.FileName := CONST_DirBlocks+'Blocks_'+IntToStr(FirstBlock)+'_'+IntToStr(LastBlock)+'.zip';
for contador := FirstBlock to LastBlock do
   begin
   MyZipFile.Entries.AddFileEntry(CONST_DirBlocks+IntToStr(contador)+'.blk');
   end;
MyZipFile.ZipAllFiles;
AFileStream := TFileStream.Create(MyZipFile.FileName , fmOpenRead + fmShareDenyNone);
   try
   if conexiones[Slot].tipo='client' then
      begin
      Conexiones[Slot].context.Connection.IOHandler.WriteLn('BLOCKZIP');
      Conexiones[Slot].context.connection.IOHandler.Write(AFileStream,0,true);
      end;
   if conexiones[Slot].tipo='server' then
      begin
      ConexionesCliente[Slot].IOHandler.WriteLn('BLOCKZIP');
      ConexionesCliente[Slot].IOHandler.Write(AFileStream,0,true);
      end;
   finally
   AFileStream.Free;
   end;
MyZipFile.Free;
deletefile(CONST_DirBlocks+'Blocks_'+IntToStr(FirstBlock)+'_'+IntToStr(LastBlock)+'.zip');
end;

// UNZIP THE RECEIVED BLOCKS
procedure UnzipBlockFile(filename:String);
var
  UnZipper: TUnZipper;
begin
UnZipper := TUnZipper.Create;
   try
   UnZipper.FileName := filename;
   UnZipper.OutputPath := '';
   UnZipper.Examine;
   UnZipper.UnZipAllFiles;
   finally
   UnZipper.Free;
   end;
deletefile(filename);
end;

// OBTAINS THE NUMBER OF CHARACTERS FROM A DIFFICUL STRING
function GetCharsFromMinerDiff(minerdiff:string):integer;
var
  Lettra : Char;
Begin
Lettra := minerdiff[1];
Result := Ord(Lettra)-96;
End;

// OBTAINS THE NUMBER OF STEPS FROM A DIFFICUL STRING
Function GetStepsFromMinerDiff(minerdiff:string):integer;
var
  Lettra : Char;
Begin
Lettra := minerdiff[2];
Result := Ord(Lettra)-96;
End;

// SEND THE NODES TO PEER
procedure PTC_GetNodes(Textline:string;slot:integer);
var
  NodesString : String = '';
  NodesAdded : Integer = 0;
  Counter : Integer;
Begin
for counter := 0 to length(ArrayNodos)-1 do
   begin
   NodesString := NodesString+' '+ArrayNodos[counter].ip+':'+ArrayNodos[counter].port+':';
   NodesAdded := NodesAdded+1;
   if NodesAdded>50 then break;
   end;
NodesString := '{MIC NODES'+NodesString+' END}';
SendPTCMessage(slot,NodesString);
End;

// SAVE NODES RECEIVED FROM PEER
procedure PTC_SaveNodes(Textline:string;slot:integer);
var
  ArrParameters : Array of string;
  contador : integer = 1;
  ThisParam : String = '';
  MoreParam: boolean = true;
  ThisNode : NodeData;
Begin
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
for contador := 0 to length(ArrParameters)-1 do
   begin
   thisnode := GetNodeFromString(ArrParameters[contador]);
   if uppercase(thisnode.ip) = 'LOCALHOST' then thisnode.ip := '127.0.0.1';
   if not NodeExists(thisnode.ip,thisnode.port) then AddNewNode(thisnode.ip,thisnode.port);
   end;
End;

// GET NODE DATA FROM STRING
function GetNodeFromString(DataString:String):NodeData;
var
  counter: integer;
  Founded: Boolean = false;
  ThisData: String = '';
  Resultado : NodeData;
  ThisChar : Char;
Begin
for counter := 1 to length(DataString) do
   begin
   ThisChar := DataString[counter];
   if ThisChar = ':' then
      begin
      if not Founded then
         begin
         Resultado.ip := ThisData;
         Founded := true;
         ThisData := '';
         end
      else
         begin
         Resultado.port:= ThisData;
         end;
      end
   else
      begin
      ThisData := ThisData + ThisChar;
      end;
   end;
Resultado.LastAvailable:='';
result := Resultado;
End;

// READJUST LAST BLOCK REQUESTED IF PEER SAYS WE MADE A BAD REQUEST
procedure PTC_InBlRe(Textline:string;slot:integer);
Begin
if LOCAL_MyLastBlock < STATUS_LastBlockRequested then
   STATUS_LastBlockRequested := LOCAL_MyLastBlock;
End;

END.  // END UNIT

