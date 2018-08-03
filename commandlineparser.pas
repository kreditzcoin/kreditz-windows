unit CommandLineParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, master, CM_Crypto, TimeUnit, Forms;

   Procedure ParseCommandLine(Linetext:String);
   Function GetCommandLineCommand(LineText:String):String;
   Function GetParameterFromCommandLine(LineText:String;ParamNumber:Integer):String;
   procedure ShowHelp();
   procedure ShowNodes();
   procedure ShowBLNodes();
   Procedure AddNode(linetext:String);
   Procedure DeleteNode(linetext:String);
   Procedure DeleteBLNode(linetext:String);
   procedure ShowStatus();
   Procedure ShowConexSlots();
   Procedure GetNewWallet();
   Procedure CreateNewAddress();
   Procedure ShowOutGoing();
   Procedure ShowPending();
   procedure showaccounts();
   procedure TurnMinerOn();
   Procedure TurnMinerOff();
   Procedure SendFunds(linetext:string);
   Procedure ShowBlockSum();
   Procedure GetCurrCiff();
   Procedure ReconnectOn();
   Procedure ReconnectOff();
   Procedure SetPort(linetext:string);
   procedure ShowSha256String(linetext:String);
   Procedure CheckAddress(linetext:String);
   Procedure MinerInfo();
   Procedure ClientCount();
   Procedure SetCPUMiners(linetext:String);
   Procedure MinToTask(linetext:String);
   Procedure AutoConn(linetext:String);
   Procedure FullNode(linetext:String);
   Procedure UpdateNodes(linetext:String);

implementation

Uses
  MC_Main, Protocol,Blocks;

// PARSES COMANDLINE
Procedure ParseCommandLine(Linetext:String);
var
  CommandUsed : String;
begin
CommandUsed :=GetCommandLineCommand(Linetext);
if CommandUsed = '' then exit;
OutPutText('>> '+linetext,false);
if UpperCase(CommandUsed) = 'HELP' then ShowHelp
else if UpperCase(CommandUsed) = 'NODES' then ShowNodes
else if UpperCase(CommandUsed) = 'BLNODES' then ShowBLNodes
else if UpperCase(CommandUsed) = 'ADDNODE' then AddNode(linetext)
else if UpperCase(CommandUsed) = 'DELETENODE' then DeleteNode(linetext)
else if UpperCase(CommandUsed) = 'DELETEBL' then DeleteBLNode(linetext)
else if UpperCase(CommandUsed) = 'STATUS' then ShowStatus()
else if UpperCase(CommandUsed) = 'CONNECT' then ConnectToServers()
else if UpperCase(CommandUsed) = 'DISCONNECT' then CloseConectionsToServers()
else if UpperCase(CommandUsed) = 'LISTENOFF' then TurnListenOff()
else if UpperCase(CommandUsed) = 'LISTENON' then TurnListenOn()
else if UpperCase(CommandUsed) = 'CONSLOTS' then ShowConexSlots()
else if UpperCase(CommandUsed) = 'OUTGOING' then ShowOutgoing() // TEMP
else if UpperCase(CommandUsed) = 'PENDING' then ShowPending() // TEMP
else if UpperCase(CommandUsed) = 'HASHCOUNTER' then OutputText(IntToStr(MINER_HashCounter)) // TEMP
else if UpperCase(CommandUsed) = 'ACCSUMHASH' then OutputText(LOCAL_MyAccsumHash)
else if UpperCase(CommandUsed) = 'MINERON' then TurnMinerOn()
else if UpperCase(CommandUsed) = 'MINEROFF' then TurnMinerOff()
else if UpperCase(CommandUsed) = 'NETTIME' then OutputText(GetNetworkTimestamp())
else if UpperCase(CommandUsed) = 'TIMESTAMP' then OutputText(GetTimestamp())
else if UpperCase(CommandUsed) = 'CLS' then ClearMemoLines()
else if UpperCase(CommandUsed) = 'ACCOUNTS' then showaccounts()
else if UpperCase(CommandUsed) = 'NEWADDRESS' then CreateNewAddress()
else if UpperCase(CommandUsed) = 'BLOCKSUM' then ShowBlockSum()
else if UpperCase(CommandUsed) = 'SENDTO' then SendFunds(linetext)
else if UpperCase(CommandUsed) = 'LBUNDONE' then UndoneLastBlock() // TEMP
else if UpperCase(CommandUsed) = 'LBDATA' then LBData()
else if UpperCase(CommandUsed) = 'LBNUM' then OutputText('Last Block on BlockSum: '+IntToStr(BlockSumLastBlock()))
else if UpperCase(CommandUsed) = 'GETDIFF' then GetCurrCiff()
else if UpperCase(CommandUsed) = 'RECON' then ReconnectOn()
else if UpperCase(CommandUsed) = 'RECOFF' then ReconnectOff()
else if UpperCase(CommandUsed) = 'SETPORT' then SetPort(linetext)
else if UpperCase(CommandUsed) = 'OPENSSL' then RunOpenSSLCommand(linetext)
else if UpperCase(CommandUsed) = 'SHA256' then ShowSha256String(linetext)
else if UpperCase(CommandUsed) = 'CHECKADDRESS' then CheckAddress(linetext)
else if UpperCase(CommandUsed) = 'MINERINFO' then Minerinfo()
else if UpperCase(CommandUsed) = 'CLIENTCOUNT' then ClientCount()
else if UpperCase(CommandUsed) = 'CPUCOUNT' then OutputText('CPUs: '+IntToStr(MAIN_CPUCOUNT))
else if UpperCase(CommandUsed) = 'CPUMINE' then SetCPUMiners(linetext)
else if UpperCase(CommandUsed) = 'MINTOTASK' then MinToTask(linetext)
else if UpperCase(CommandUsed) = 'AUTOCONN' then AutoConn(linetext)
else if UpperCase(CommandUsed) = 'FULLNODE' then FullNode(linetext)
else if UpperCase(CommandUsed) = 'UPDATENODES' then UpdateNodes(linetext)

else OutPutText('Unknown command: '+CommandUsed,false);
end;

// GETS THE REQUIRED FUNCTION ON THE COMMANDLINE
Function GetCommandLineCommand(LineText:String):String;
var
  Temp : String = '';
  ThisChar : Char;
  Contador : Integer = 1;
Begin
while contador <= Length(LineText) do
   begin
   ThisChar := Linetext[contador];
   if  ThisChar = ' ' then
      begin
      result := temp;
      exit;
      end
   else temp := temp+ ThisChar;
   contador := contador+1;
   end;
Result := Temp;
End;

// GET A DETERMINED PARAMETER FROM COMMANDLINE
Function GetParameterFromCommandLine(LineText:String;ParamNumber:Integer):String;
var
  Temp : String = '';
  ThisChar : Char;
  Contador : Integer = 1;
  WhiteSpaces : Integer = 0;
Begin
while contador <= Length(LineText) do
   begin
   ThisChar := Linetext[contador];
   if  ThisChar = ' ' then
      begin
      WhiteSpaces := WhiteSpaces +1;
      if WhiteSpaces > Paramnumber then
         begin
         result := temp;
         exit;
         end;
      end
   else if WhiteSpaces = ParamNumber then temp := temp+ ThisChar;
   contador := contador+1;
   end;
Result := Temp;
End;

// SHOWS HELP
procedure ShowHelp();
Begin
OutPutText('* All commands are case insensitive',false);
OutPutText('Help          - This Info',false);
OutPutText('Nodes         - List the existing nodes',false);
OutPutText('AddNode xx yy - Adds Node xx at port yy',false);
OutPutText('DeleteNode x  - Delete Node stored at x position',false);
OutPutText('Status        - Show The overall status',false);
OutPutText('Connect       - Try to connect to all known nodes',false);
OutPutText('Disconnect    - Disconnects all outgoing connections',false);
end;

// SHOWS NODES
procedure ShowNodes();
Var
  contador:integer=0;
Begin
while contador < length(arraynodos) do
   begin
   OutPutText(inttostr(contador+1)+' - '+arraynodos[contador].ip+':'+arraynodos[contador].port,false);
   contador := contador+1;
   end;
if Length(ArrayNodos) = 0 then OutPutText ('No nodes registered',false);
end;

// SHOWS BLACKLISTED NODES
procedure ShowBLNodes();
Var
  contador:integer=0;
Begin
while contador < length(ArrayBlacklisted) do
   begin
   OutPutText(inttostr(contador+1)+' - '+ArrayBlacklisted[contador].ip,false);
   contador := contador+1;
   end;
if Length(ArrayBlacklisted) = 0 then OutPutText ('No Blacklisted nodes',false);
end;

// ADD A NEW NODE
Procedure AddNode(linetext:String);
var
  NodeIp, NodePort : String;
Begin
NodeIp := GetParameterFromCommandLine(linetext,1);
if NodeIp ='' then
   begin
   Outputtext('Missing Parameter 1: IP',false);
   exit;
   end;
NodePort := GetParameterFromCommandLine(linetext,2);
if NodePort ='' then
   begin
   Outputtext('Missing Parameter 2: PORT',false);
   exit;
   end;
if NodeExists (NodeIp, NodePort) then OutPutText('Node already registered',false)
else
   begin
   AddNewNode(NodeIp, NodePort);
   OutPutText('Node added at position '+IntToStr(length(ArrayNodos)),false);
   end;
end;

// DELETE A NODE
Procedure DeleteNode(linetext:String);
var
  NodeNumber : Integer;
Begin
if not IsValidInt(GetParameterFromCommandLine(linetext,1)) then
   begin
   OutPutText('Invalid Parameter: '+GetParameterFromCommandLine(linetext,1),false);
   exit;
   end;
NodeNumber := StrToInt(GetParameterFromCommandLine(linetext,1));
if NodeNumber > Length(ArrayNodos) then
   begin
   OutPutText('Parameter '+IntToStr(NodeNumber)+' out of bounds',false);
   exit;
   end;
DeleteExistingNode(NodeNumber-1); // -1 Since arraynodos is 0 indexed
OutPutText('Node '+IntToStr(NodeNumber)+' deleted',false);
End;

// DELETE A BLACKLISTED NODE
Procedure DeleteBLNode(linetext:String);
var
  NodeNumber : Integer;
Begin
if not IsValidInt(GetParameterFromCommandLine(linetext,1)) then
   begin
   OutPutText('Invalid Parameter: '+GetParameterFromCommandLine(linetext,1),false);
   exit;
   end;
NodeNumber := StrToInt(GetParameterFromCommandLine(linetext,1));
DeleteBlackListedNode(NodeNumber-1); // -1 Since arraynodos is 0 indexed
OutPutText('Blacklisted Node '+IntToStr(NodeNumber)+' deleted',false);
End;

// SHOW STATUS
procedure ShowStatus();
Begin
OutPutText('To be implemented',false);
end;

// SHOW CONNECTIONS STATUS
Procedure ShowConexSlots();
var
  contador : integer;
  ShowClientNumber : String = '';
Begin
for contador := 1 to CONST_MAXConections do
   begin
   if Conexiones[contador].tipo = 'server' then ShowClientNumber := ' - CLI:'+Conexiones[contador].ClientConn
   else ShowClientNumber := '';
   OutputText(IntToStr(contador)+' - '+Conexiones[contador].tipo+' - '+Conexiones[contador].ip+ShowClientNumber,false);
   end;
end;

// CREATES A NEW WALLET IF NOT EXISTS
Procedure GetNewWallet();
begin
   AssignFile(Filawallet,CONST_ArchivoWallet);
   rewrite(FilaWallet);
   closefile(FilaWallet);
   CreateNewAddress();
   SaveAddressesToDisk();
   OutputText('Wallet Created');
end;

// CREATES A NEW ADDRESS
Procedure CreateNewAddress();
var
  PublicKey, PrivateKey : String;
  MyData: MyWalletData;
  Address: String;
Begin
CreateKeysPair();
PublicKey := GetPublicKeyFromPem();
Privatekey := GetPrivateKeyFromPem();
Address := GetAddressFromPublicKey(PublicKey);
MyData.Hash:=Address;
Mydata.PublicKey:=PublicKey;
MyData.PrivateKey:=PrivateKey;
MyData.Balance:='-1';
MyData.RegisterStatus:=0;
SetLength(ArrayMyAddresses,Length(ArrayMyAddresses)+1);
ArrayMyAddresses[length(ArrayMyAddresses)-1] := MyData;
U_SaveWallet := true;
Deletefile('DATA/private.pem');
Deletefile('DATA/public.pem');
OutputText('Address Created: '+Address);
UpdateMyAddresses();
End;

// SHOWS THE OUTGOING PROTOCOL MESSAGES
Procedure ShowOutGoing();
var
  contador : integer = 0;
Begin
if OutGoingMessages.Count> 0 then
   begin
   OutputText('OutGoing Messages: '+IntToStr(OutGoingMessages.Count));
   for contador := 0 to OutGoingMessages.count-1 do
      begin
      Outputtext(OutGoingMessages[contador],false)
      end
   end
else Outputtext('Not Outgoing Messages', false);
end;

// SHOWS THE PENDING TXS
Procedure ShowPending();
var
  contador : integer = 0;
Begin
if PendingTxs.Count> 0 then
for contador := 0 to PendingTxs.count-1 do
   begin
   Outputtext(PendingTxs[contador],false)
   end
else Outputtext('Not Pending Transactions', false);
End;

// SHOW ALL ACCOUNTS
procedure showaccounts();
var
  contador : integer = 0;
  DataRead : AccountData;
  Registered : String;
begin
assignfile (FilaAccData,CONST_ArchivoAccData);
reset(FilaAccData);
while contador < filesize(FilaAccData) do
   begin
   seek(FilaAccData,contador);
   read(FilaAccData,DataRead);
   if DataRead.PublicKey <> '' then Registered := 'REGISTERED' else Registered :='Unreg';
   outputtext(IntToStr(contador)+' - '+copy(DataRead.Hash,1,8)+' - '+Int2CurrencyStr(StrToInt(Dataread.Balance))+' '+Registered+' '+Dataread.Lastop,false);
   contador := contador +1;
   end;
closefile(FilaAccData);
end;

// TURN THE MINER ON
procedure TurnMinerOn();
Begin
if STATUS_Updated then
   begin
   OptionsData.Mining := true;
   U_SaveOptions := true;
   MINER_IsMinerOn := false;
   end
else
   begin
   OutputText('You can not mine now',false);
   OptionsData.Mining := true;
   U_SaveOptions := true;
   MINER_IsMinerOn := false;
   end;
end;

// TURN THE MINER OFF
Procedure TurnMinerOff();
Begin
CloseAllMiningThreads();
OptionsData.Mining := false;
U_SaveOptions := true;
MINER_IsMinerOn := false;
end;

// SEND FUNDS TO ACCOUNT OR ADDRESS
Procedure SendFunds(linetext:string);
var
  Destination, Amount : String;
  Monto, comision, MontoMasComision, Restante : Integer;
  Contador : integer;
  MontoFromAddress : Integer;
Begin
Destination := GetParameterFromCommandLine(linetext,1);
Amount := GetParameterFromCommandLine(linetext,2);
if IsValidInt(Destination) then
   begin
   Destination := GetAddressFromAccountNumber(StrToInt(Destination));
   if StrToInt(GetAccountNumberFromAddress(Destination)) < 0 then
      begin
      Outputtext('Account number do not exists',false);
      ShowAlert('Account number do not exists');
      exit;
      end;
   end;
if ((Destination = '') or (not IsValidAddress(Destination))) then
   begin
   Outputtext('Invalid Destination'+destination,false);
   ShowAlert('Invalid Destination'+destination);
   exit;
   end;

if IsAddressMine(Destination)>=0 then
   begin
   Outputtext('Can not send to your addresses',false);
   ShowAlert('Can not send to your addresses');
   exit;
   end;
if IsValidInt(Amount) then Monto := StrToInt(Amount)
else
   begin
   Outputtext('Invalid Amount',false);
   ShowAlert('Invalid Amount');
   exit;
   end;
if Monto < 0 then
   begin
   Outputtext('Sending negative values is a bad idea',false);
   ShowAlert('Sending negative values is a bad idea');
   exit;
   end;
comision := GetComisionValue(Monto);
MontoMasComision := Monto + Comision;
if MontoMasComision > MAIN_AccountBalance-GetTotalAccountPendingPayments() then
   begin
   Outputtext('Insufficient funds'+SlineBreak+'You need '+Int2CurrencyStr(MontoMasComision)+' KDZ',false);
   ShowAlert('Insufficient funds'+SlineBreak+'You need '+Int2CurrencyStr(MontoMasComision)+' KDZ');
   exit;
   end;
Restante := MontoMasComision;
Contador := length(ArrayMyAddresses)-1;
While Restante > 0 do
   begin
   if StrToInt(ArrayMyAddresses[contador].Balance) > 0 then
      begin
      MontoFromAddress := SendFundsFromAddress(Destination, contador, Restante);
      Restante := Restante - MontoFromAddress;
      end;
   contador := contador - 1;
   end;
ShowAlert('Transfer Sucessfull');
End;

// SHOWS THE BLOSKCUM ARRAY
Procedure ShowBlockSum();
var
  contador : integer;
  BlNumber, Trxs, TotTime, timestart, timeend,miner,blhash : String;
  BlockData : BlockSumData;
Begin
for contador := 0 to length(ArrBlockSummary)-1 do
   begin
   BlockData := ArrBlockSummary[contador];
   BlNumber := BlockData.Number;
   Trxs := BlockData.TrxTot;
   timestart := blockdata.TimeStart;
   timeend := Blockdata.TimeEnd;
   TotTime := BlockData.TimeTot;
   Miner := BlockData.AccountMiner;
   blhash := Blockdata.BlockHash;
   OutputText('Block:'+BlNumber+' Trxs:'+Trxs+' Start:'+timestart+' End: '+timeend+' Duration:'+TotTime,false);
   OutPutText('   Miner: '+miner+' Hash: '+blhash,false);
   end;
OutputText('Block Sumarry contains '+IntToStr(length(ArrBlockSummary))+' blocks',false);
End;

// SHOW DIFFICULT OF CURRENT MINING
Procedure GetCurrCiff();
Begin
if not STATUS_Updated then OutputText('Can not retrieve difficulty',false)
else
   begin
   OutputText('Block: '+IntToStr(BlockSumLastBlock()+1)+' -> '+
   IntToStr(GetCharsFromMinerDiff(MINER_MineDiff))+' Chars, '+
   IntToStr(GetStepsFromMinerDiff(MINER_MineDiff))+' Steps',false)
   end;
End;

// TURN AUTORECONNECTIONS ON
Procedure ReconnectOn();
Begin
OptionsData.Reconnect:=true;
U_SaveOptions := true;
OutPutText('Reconnections ENABLED',false);
end;

// TURN AUTORECONNECTIONS OFF
Procedure ReconnectOff();
Begin
OptionsData.Reconnect:=false;
U_SaveOptions := true;
OutPutText('Reconnections DISABLED',false);
end;

// SET LOCAL LISTENING PORT
Procedure SetPort(linetext:string);
var
  PortNumber : string;
Begin
if not IsValidInt(GetParameterFromCommandLine(linetext,1)) then
   begin
   OutPutText('ERROR: Invalid Parameter: '+GetParameterFromCommandLine(linetext,1),false);
   EditUserPort.Text := OptionsData.ListeningPort;
   exit;
   end;
PortNumber := GetParameterFromCommandLine(linetext,1);
OptionsData.ListeningPort:=PortNumber;
OutPutText('New Listening port set'+SLineBreak+'Change will be effective on next connection',false);
U_SaveOptions := true;
End;

// SHOWS THE SHA256 OF THE GIVEN PARAMETER
procedure ShowSha256String(linetext:String);
var
  TextToSha : String;
Begin
TextToSha := GetParameterFromCommandLine(linetext,1);
OutputText(HashSha256String(TextToSha),false);
End;

// SHOWS IF A GIVEN ADDRESS IS VALID
Procedure CheckAddress(linetext:String);
var
  AddToCheck : String;
Begin
AddToCheck := GetParameterFromCommandLine(linetext,1);
if IsValidAddress(AddToCheck) then
  OutputText('✔ VALID ADDRESS',false)
else OutputText('✘ INVALID ADDRESS',false)
end;

// SHOWS THE MINER VARIABLES
Procedure MinerInfo();
Begin
OutputText('IsMinerOn             : '+Booltostr(MINER_IsMinerOn, true),false);
OutputText('OptionsData.Mining    : '+Booltostr(OptionsData.Mining, true),false);
OutputText('STATUS_Updated        : '+Booltostr(STATUS_Updated, true),false);
OutputText('MINER_BlockFound      : '+Booltostr(MINER_BlockFound, true),false);
OutputText('LASTBLOCK_Duration    : '+IntToStr(LASTBLOCK_Duration),false);
OutputText('MINER_TargetHash      : '+MINER_TargetHash,false);
OutputText('NETWORK_LastBlockHash : '+NETWORK_LastBlockHash,false);
OutputText('MINER_FoundedSteps    : '+IntToStr(MINER_FoundedSteps),false);
OutputText('MINER_Steps           : '+IntToStr(MINER_Steps),false);
End;

// SHOWS THE SERVER CLIENT COUNT // DEPRECATED
Procedure ClientCount();
Begin
OutputText('TCPServer Client count: '+IntToStr(ClientsCount),false);
end;

// SET THE NUMBER OF MINING CPUS
Procedure SetCPUMiners(linetext:String);
var
  CPUsNumber : string;
Begin
if not IsValidInt(GetParameterFromCommandLine(linetext,1)) then
   begin
   OutPutText('ERROR: Invalid Parameter: '+GetParameterFromCommandLine(linetext,1),false);
   exit;
   end;
CPUsNumber := GetParameterFromCommandLine(linetext,1);
if ((StrToInt(CPUsNumber) < 1) or (StrToInt(CPUsNumber)>MAIN_CPUCOUNT)) then
   begin
   OutPutText('ERROR: Mining CPUs range: 1 to '+IntToStr(MAIN_CPUCOUNT),false);
   exit;
   end;
OutPutText('Mining CPUs set to: '+CPUsNumber,false);
OptionsData.CPUmining := StrToInt(CPUsNumber);
U_SaveOptions := true;
End;

// SET MINIMIZE TO TRAY OPTION
Procedure MinToTask(linetext:String);
var
  Option : string;
Begin
Option := GetParameterFromCommandLine(linetext,1);
if UpperCase(Option)='ON' then
   begin
   OptionsData.MinimToTray:=true;
   U_SaveOptions := true;
   end
else if UpperCase(Option)='OFF' then
   begin
   OptionsData.MinimToTray:=false;
   U_SaveOptions := true;
   end
else OutputText('Invalid Parameter. Use ON of OFF',false);
End;

// SET AUTOCONNECTION OPTION
Procedure AutoConn(linetext:String);
var
  Option : string;
Begin
Option := GetParameterFromCommandLine(linetext,1);
if UpperCase(Option)='ON' then
   begin
   OptionsData.AutoConnect:=true;
   U_SaveOptions := true;
   end
else if UpperCase(Option)='OFF' then
   begin
   OptionsData.AutoConnect:=false;
   U_SaveOptions := true;
   end
else OutputText('Invalid Parameter. Use ON of OFF',false);
End;

// SET FULLNODE OPTION
Procedure FullNode(linetext:String);
var
  Option : string;
Begin
Option := GetParameterFromCommandLine(linetext,1);
if UpperCase(Option)='ON' then
   begin
   OptionsData.FullNode:=true;
   U_SaveOptions := true;
   end
else if UpperCase(Option)='OFF' then
   begin
   OptionsData.FullNode:=false;
   U_SaveOptions := true;
   end
else OutputText('Invalid Parameter. Use ON of OFF',false);
End;

// SET GETNODES OPTION
Procedure UpdateNodes(linetext:String);
var
  Option : string;
Begin
Option := GetParameterFromCommandLine(linetext,1);
if UpperCase(Option)='ON' then
   begin
   OptionsData.GetNodes:=true;
   U_SaveOptions := true;
   ShowMensaje(true);
   end
else if UpperCase(Option)='OFF' then
   begin
   OptionsData.GetNodes:=false;
   U_SaveOptions := true;
   ShowMensaje(true);
   end
else OutputText('Invalid Parameter. Use ON of OFF',false);
End;




END. // END UNIT

