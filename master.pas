unit MASTER;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, FileUtil, Forms, Controls, ExtCtrls, Graphics, StdCtrls,
  IdContext, grids, CM_Crypto, TimeUnit;

type
  nodedata = Packed Record
     ip: string[15];
     port: string[8];
     LastAvailable : string[17];
     end;

  conectiondata = Packed Record
     Autentic: boolean;
     Connections : Integer;
     tipo: string[255];
     ip: string[255];
     lastping: string[255];
     context: TIdContext;
     ClientConn: string[255];
     Lastblock: string[255];
     LastblockHash: string[255];
     Accounts: string[255];
     AccountsHash : string[255];
     Pending: string[255];
     ListenPort: string[255];
     end;

  UserData = Packed Record
     ListeningPort: string[8];
     OpenSSLPath : String[255];
     Mining: boolean;
     Reconnect : Boolean;
     CPUmining : Integer;
     MinimToTray : Boolean;
     AutoConnect : Boolean;
     FullNode : Boolean;
     GetNodes : Boolean;
     end;

  AccountData = Packed Record
     Number: String[13];
     PublicKey: String[255];
     Hash: String[40];
     Balance: String[13];
     Lastop: String[13];
     end;

  TranxData = Packed Record
     block : String[13];
     TypeTx : String[6];
     TimeStamp : String[17];
     Sender : String[40];
     Receiver : String[40];
     Ammount : String[120];
     Signature : String[120];
     Hash : String[40];
     end;

  BlockSumData = Packed Record
     Number : String[13];
     TimeStart : String[20];
     TimeEnd : String[20];
     TimeTot : String[10];
     TrxTot : String[10];
     TargetHash : String[34];
     Difficult : String[3];
     NxtBlkDiff : String[3];
     BlockHash : String[40];
     Solution : String[255];
     AccountMiner : String[40];
     end;

  WalletData = Packed Record
     Hash : String[40];
     PublicKey : String[255];
     PrivateKey : String[255];
     end;

  MyWalletData = Packed Record
     Hash : String[40];
     PublicKey : String[255];
     PrivateKey : String[255];
     Balance : String[13];
     RegisterStatus : Integer;
     end;

  BlockHeaderData = Packed Record
     Number         : Int64;
     TimeStart      : Int64;
     TimeEnd        : Int64;
     TimeTot        : Integer;
     TrxTot         : integer;
     TargetHash     : String[34];
     Difficult      : String[3];
     NxtBlkDiff     : String[3];
     AccountMiner   : String[40];
     MinerFee       : Int64;
     Reward         : Int64;
     SolutionLength : Integer;
     end;

  // Operative functions
  function Int2CurrencyStr(Value: integer): string;
  function IsValidInt(cadena:string):boolean;
  procedure UpdateLabels();
  procedure Outputtext(TextToShow:String;showhour:boolean=true);
  procedure ClearMemoLines();
  procedure LoadWalletData();
  Procedure SaveAddressesToDisk();
  function verifyfiles():boolean;
  function IsAddressMine(address:string):integer;
  function GetTotalAccountBalance():Int64;
  function GetTotalAccountPendingPayments():Int64;
  function GetAddressAvailable(Address:String): Int64;
  function GetAccountNumberFromAddress(address:string):string;
  function GetAddressFromAccountNumber(Accnumber:integer):String;
  function GetAddressBalanceFromDisk(Account:String):Int64;
  function GetAddressPubKey(Address:String):String;
  procedure CheckIfMyAddressesNeedsRegisterFromDisk();
  procedure AddAddressIfNotExists(address:string);
  Procedure VerifyUserAddressesForAcre();
  function GetReachableNodes():Integer;
  function GetOpenSSLPath():String;
  procedure UpdatePanelStatus();
  procedure UpdatePanelServer();
  Procedure UpdateGridNodes();
  procedure UpdateBLGridNodes();
  procedure UpdateGridConxs();
  Procedure UpdateMyAddresses();
  procedure UpdatePanelMiner();
  Procedure UpdateMyPendingTxs();
  Procedure ShowMensaje(Exito:Boolean);

  // Nodes
  function NodeExists(ip,port:String):boolean;
  function BLNodeExists(ip,port:String):boolean;
  procedure LoadNodesFromDisk();
  Procedure LoadBLNodesFromDisk();
  procedure AddNewNode(Address,Port:String);
  Procedure AddNewBLNode(Address,Port:String);
  procedure DeleteExistingNode(Number:Integer);
  Procedure DeleteBlackListedNode(Number:Integer);
  Procedure SaveNodesToDisk();
  Procedure SaveBLNodesToDisk();
  Procedure DeleteNodeAddress(Address:String);
  // options
  function LoadOptionsFromDisk(): UserData;
  procedure SaveOptionsToDisk();
  // updates network values: returns the slot number with the info
  function UpdateNetworkLastBlockData():Integer;
  function UpdateNetworkAccountSumData(): Integer;
  function UpdateNetworkPendingData():Integer;
  // Updates local values
  function GetMyLastUpdatedBlock():Integer;
  function GetMyLastBLockHash(Lastblock:String):String;
  function GetLastAccountUpdated():Integer;
  function GetMyAccSumHash():String;

  function BlockSumLastBlock():Integer;
  function GetBlockData(blnumber:Integer):BlockSumData;
  function GetBlockDataFromDisk(BlockNumber:Integer):BlockSumData;
  Procedure BuildBlockSum();
  Procedure AdjustBlockSum();

  // Connections management
  function ClientsCount : Integer ;
  function GetActiveConex(tipo:string):Integer;
  function GetTotalConex():Integer;
  function SaveConection(tipo,ipuser,timestamp:String;contextdata:TIdContext;clientnumber:string):integer;
  function ClearConection(tipo,ipuser:string):boolean;
  function GetFreeConexSlot():Integer;
  function GetFreeCliente():Integer;
  function ConnectClient(Address,Port:String):integer;
  function AreWeConnectedTo(address:String):Boolean;
  function GetSlotFromIP(Ip:String):Integer;
  Procedure ConnectToServers();
  Procedure TryConnectToNode(NodeNumber:Integer);
  procedure CloseConectionsToServers();
  procedure TurnListenOff();
  procedure TurnListenOn();
  procedure CloseConnectionSlot(Slot:Integer);

Const
  CONST_ArchivoNodos = 'DATA/nodes.min';
  CONST_Blacklist = 'DATA/blnodes.min';
  CONST_ArchivoUser = 'DATA/user.min';
  CONST_ArchivoAccData = 'DATA/accsum.dat';
  CONST_DirBlocks = 'DATA/BLOCKS/';
  CONST_ArchivoWallet = 'DATA/wallet.dat';
  CONST_ArchivoBlockSum = 'DATA/blocksum.dat';

  CONST_MinimunConnectionsToWork = 1;
  CONST_PinsToStart = CONST_MinimunConnectionsToWork*2;
  CONST_BlockSummaryLength = 20;
  CONST_ExpectedBlockDuration = 180;
  CONST_DefaultServerPort = 8080;
  CONST_MaxIncomingConections = 5;
  CONST_MaxOutgoingConections = 5;
  CONST_MAXConections = CONST_MaxIncomingConections+CONST_MaxOutgoingConections;

Var
  FilaNodos : file of nodedata;  // Nodes
     U_SaveNodeFile : boolean = false;
  FilaBlackList : file of nodedata;
     U_SaveBlacklist : boolean = false;
  FilaUser : file of Userdata; // Options
     U_SaveOptions : boolean = false;
  FilaAccData: file of AccountData;
  FilaBlock : file of TranxData;
  FilaWallet : file of WalletData;
     U_SaveWallet : boolean = false;
  FilaSumary : file of BlocksumData;

  OptionsData : UserData; // Options values
  LastCommandline : String = '';
  AutoRetryConnect : boolean = false; // Auto retry connection to nodes
  OpenSSLPath : String = '';

  CONNECT_LastTime : Int64 = 0;
  CONNECT_LastNode : Integer = 0;

  STATUS_Connected : boolean = false;
  STATUS_Synzed : boolean = false;
    STATUS_LastBlockRequested : Integer = 0;
    STATUS_LastAccountRequested : boolean = false;
  STATUS_Updated: boolean = false;
  STATUS_IncomingPings : Integer = 0;
  STATUS_LastPing : Integer = 0;

  LASTBLOCK_PendingTxs : TStringlist;
  LASTBLOCK_UNDONE : Integer = 0;
  LASTBLOCK_ArrBlockSum : Array of BlockSumData;
  LASTBLOCK_ArrMyAddresses : Array of MyWalletData;
  LASTBLOCK_Duration : Integer;

  MAIN_AccountNumber : String = '';
    U_MyAccountNumber : boolean = false;
  MAIN_AccountHash: String = '';
  MAIN_AccountBalance : Integer = 0;
    U_MyBalance : boolean = false;
  MAIN_AccountPublic : String = '';
  MAIN_AccountPrivate : String = '';
  MAIN_Version : string = '0.180730';
  MAIN_USER_IP : String = '';
  MAIN_CPUCOUNT : Integer = 0;

  MINER_IsMinerOn : Boolean = false; // is the miner activated?
  MINER_HashCounter     : Integer = 0; // the longint counter for mining
  MINER_LastHashCounter : Integer = 0; // used to calculate the mining hash speed
  MINER_HashSeed : String = ''; // the prefix for mining
  MINER_BlockFound : boolean = false; // true when the user mines the block
  MINER_ResultHash : String = ''; // result when block is found
  MINER_FoundedSteps : Integer = 0;  // founded steps of this block
  MINER_TargetHash : String = ''; // TARGET HASH TO FOUND THE NEW BLOCK
  MINER_MineDiff : String = 'fe';  // string containing the block difficulty
  MINER_TargetChars : Integer = 6; // Number of consecutive operations to mine the block
  MINER_Steps : Integer = 1; // number of steps to mine the block
  MINER_BlockDiffSet : Integer = -1; // block the user is mining
  MINER_CurrStepTarget : String = '';

  LOCAL_MyLastBlock : Integer = 0; // last block the user have in disk
    U_MylastBlock : Boolean = false;
  LOCAL_LastBlockHash : String = ''; // hash of the last user block on disk
    U_MyLastBLockHash : Boolean = false;
  LOCAL_MyLastAccount : Integer = 0; // last account user have registered on disk
    U_MyLastAccount : boolean = false;
  LOCAL_MyAccsumHash : String = ''; // Hash of the local account sumary
    U_MyAccsumHash : boolean = false;

  NETWORK_LastBLock  : Integer = 0; // lastblock in the network
  NETWORK_LastBlockHash : String = ''; // target hash for next block
  NETWORK_LastAccount : Integer = 0; // acounnts registered on network
  NETWORK_AccsumHash : String = '';  // Accounts file hash
  NETWORK_Pending : Integer = 0;
    NETWORK_SendPing : boolean = false;

  ArrayNodos :  array of nodedata;
  ArrayBlacklisted : array of nodedata;
  ArrayMyAddresses : array of MyWalletData;
  ReadsFromSlots : array [1..CONST_MAXConections] of TStringList;
  ArrayThreads : Array of Integer;
  PendingTXs : TStringList;
  OutGoingMessages: TStringList;
  MainMemoStrings: TStringList;
  ProcessLines : TStringList;
  MyPendingTxs : TStringList;  // Pending Txs involving my addresses
  ArrBlockSummary : Array of BlockSumData;
  Conexiones : array [1..CONST_MaxIncomingConections+CONST_MaxOutgoingConections] of conectiondata;

    FormNotify: TForm;
      LabelNotify : TStaticText;
      ButtonCloseNotify : TButton;

  //FORM1 VISUAL COMPONENTS
    ButtonConsola : TButton;
    ButtonNodes : TButton;
    ButtonWallet : TButton;

    PanelNetwork : Tpanel;
      LabelPanNodes : TLabel;
      GridNodes : TStringGrid;
      ButtonDeleteNode : TButton;
      ButtonAddNode : TButton;
      PanelAddNode : TPanel;
        EditAddnodeIP: TLabeledEdit;
        EditAddNodePort : TLabeledEdit;
        ButAddNodeCancel : Tbutton;
        ButAddNodeAdd : TButton;
      LabelPanBLNodes : TLabel;
      GridBLNodes : TStringGrid;
      ButtonDeleteBL : TButton;
      PanelConexs : TPanel;
        LabelPanConxs : TLabel;
        GridConxs : TStringGrid;
      PanelOptions : TScrollBox;
        LabelUserPort : TLabel;
        EditUserPort: TEdit;
        LabelCpuMiner : TLabel;
        ComboCPUMiner : TComboBox;
        LabelMinimize : TLabel;
        CheckBoxMin : TCheckBox;
        LabelAutoConnect : TLabel;
        CheckBoxAutoConn : TCheckBox;
        LabelFullNode : TLabel;
        CheckBoxFullNode : TCheckBox;
        LabelUpdateNodes : TLabel;
        CheckBoxUpdateNodes : TCheckBox;

    PanelData : TPanel;
      LabelUser : TLabel;
      LabelBalance : Tlabel;
      LabelListen : TLabel;
      LabelConections : TLabel;
      LabelNodes : TLabel;
      LabelBlock : TLabel;
      LabelAccos : TLabel;
      LabelPending : TLabel;
      LabelLast20 : Tlabel;
      LabelMiner : TLabel;
      LabelTarget : TLabel;
      LabelAccsumHash : TLabel;
      LabelLastPing : Tlabel;
      LabelThisBlock : TLabel;
      LabelDifficult: TLabel;

    PanelWallet: TPanel;
      PanelSecAccTitle : TPanel;
        LabelBigAccNumber:TLabel;
        LabelBigBalance: TLabel;
      GridAddresses : TStringGrid;
        ButtonNewAddress : TButton;
        ButtonCopyAddress : TButton;
      PanelSendFunds : TPanel;
        LabelSendFunds : TLabel;
        ButtonPasteDest : TButton;
        LabelSFDestination : TLabel;
        EditSFDesti : Tedit;
        LabelSFAmount : TLabel;
        ButtonMaxAmo : TButton;
        EditSFAmount : TEdit;
        ButtonSendFunds : TButton;
        GridPending : TStringGrid;

    PanelServer : TPanel;
      CheckBoxConnect : TCheckBox;
    PanelStatus : TPanel;
    PanelMiner : TPanel;
      CheckBoxMiner : TCheckBox;
    PanelMensaje : TPanel;

implementation

Uses
  MC_Main, CommandLineParser, Protocol, Blocks;

{*******************************************************************************
                                    GENERAL
*******************************************************************************}

// SHOWS THE BALANCE AS CURRENCY
function Int2CurrencyStr(Value: integer): string;
begin
  result := Format('%.0n', [value * 1.0]);
end;

// DETECTS IF A STRING IS A VALID INTEGER
function IsValidInt(cadena:string):boolean;
begin
Result := True;
   Try
   StrToInt64(cadena);
   except
   On E : EConvertError do Result := false;
   end;
end;

// UPDATES STATUS LABELS
Procedure UpdateLabels();
Var
  HasesDone : real;
  AccNumberMsg, MinerMsg, TargetMsg, AccSumMsg, LastPingMsg, ThisBlockMsg : String;
  DifficultMsg,last20Msg: String;
begin
HasesDone := (MINER_HashCounter-MINER_LastHashCounter)/200000;
MINER_LastHashCounter := MINER_HashCounter;
// label account number
if StrToInt(MAIN_AccountNumber) > -1 then AccNumberMsg:= MAIN_AccountNumber
else AccNumberMsg:='Unknown';
LabelUser.Caption:=      'Account   : '+AccNumberMsg;
LabelBigAccNumber.caption:= 'Account Number : '+AccNumberMsg;
// label balance
LabelBalance.Caption:= 'Balance   : '+Int2CurrencyStr(GetTotalAccountBalance()-GetTotalAccountPendingPayments())+' KDZ';
LabelBigBalance.Caption:= Int2CurrencyStr(GetTotalAccountBalance()-GetTotalAccountPendingPayments())+' KDZ';;
// label listen
LabelListen.Caption:=    'Listening : '+Booltostr(form1.IdTCPServer1.Active, true)+'('+OptionsData.ListeningPort+')';
// label connections
LabelConections.Caption:='Conections: '+IntTostr(GetActiveConex('client'))+'('+')/'+IntTostr(GetActiveConex('server'));
// label reachable nodes
LabelNodes.Caption :=    'ReachNodes: '+IntToStr(GetReachableNodes);
// label block
LabelBlock.Caption :=    'Blocks    : '+IntToStr(LOCAL_MyLastBlock)+'/'+IntToStr(NETWORK_LastBLock);
// label accounts
LabelAccos.Caption :=    'Accounts  : '+IntToStr(LOCAL_MyLastAccount)+'/'+IntToStr(NETWORK_LastAccount);
// label pending txs
LabelPending.Caption:=   'Pending Tx: '+IntToStr(PendingTXs.Count)+'/'+IntToStr(NETWORK_Pending);
// label difficult
if STATUS_Updated then DifficultMsg:='Difficult : '+MINER_MineDiff+'/'+GetDiffForNextBlock(LOCAL_Mylastblock,LASTBLOCK_Duration)
else DifficultMsg:='Difficult : Unknown';
LabelDifficult.Caption:=DifficultMsg;
// Label last20
if STATUS_Updated then last20Msg :='Last 20   : '+IntToStr(GetLast20Average(LASTBLOCK_Duration))+' sec'
else last20Msg :=  'Last 20   : Unknown';
LabelLast20.Caption:=last20Msg;
// label miner speed
if HasesDone > 0.0 then MinerMsg := Formatfloat('##0.000',HasesDone)+' MH/s ('+
  IntToStr(MINER_FoundedSteps)+'/'+IntToStr(MINER_Steps)+')'
else MinerMsg := 'Off';
LabelMiner.Caption :=    'Hash MH/S : '+MinerMsg;
// label target hash
if STATUS_Connected then TargetMsg := copy(LOCAL_LastBlockHash,1,MINER_TargetChars)+'/'+copy(NETWORK_LastBlockHash,1,MINER_TargetChars)
else TargetMsg :='Offline';
LabelTarget.Caption:=    'Target    : '+TargetMsg;
// label accsumhash
if STATUS_Connected then
   begin
   if LOCAL_MyAccsumHash = NETWORK_AccsumHash then AccSumMsg := 'Correct'
   else AccSumMsg := 'Wrong';
   end
else AccSumMsg := 'Offline';
LabelAccsumHash.Caption:='AccsumHash: '+AccSumMsg;
// label last ping
if STATUS_Connected then LastPingMsg := IntToStr(StrToInt(copy(GetTimeStamp(),1,10))- STATUS_LastPing)+' segs'
else LastPingMsg :='Offline';
LabelLastPing.Caption:=  'Last Ping : '+LastPingMsg;
// LABEL LAST BLOCK DURATION
if STATUS_Updated then LASTBLOCK_Duration := (StrToInt64(GetTImeStamp())-StrToInt64(GetBlockData(BlockSumLastBlock()).TimeEnd)) div 1000;
if STATUS_UPDATED then ThisBlockMsg := IntToStr(LASTBLOCK_Duration)+' sec'
else ThisBlockMsg:='Unknown';
LabelThisBlock.Caption:='This Block: '+ThisBlockMsg;
end;

// OUTPUT TEXT TO THE MAINMEMO STRINGLIST
Procedure Outputtext(TextToShow:String;showhour:boolean=true);
Begin
if showhour then texttoshow := timetostr(now)+' '+TextToShow;
MainMemoStrings.Add(TextToShow);
End;

// CLEARS MEMO LINES
Procedure ClearMemoLines();
Begin
form1.MainMemo.Lines.Clear;
MainMemoStrings.Clear;
end;

// GETS THE PATH TO OPENSSL EXE
function GetOpenSSLPath():String;
var
  PascalFiles: TStringList;
Begin
Result := '';
PascalFiles := TStringList.Create;
   try
   FindAllFiles(PascalFiles, 'c:', 'openssl.exe', true);
   if PascalFiles.Count > 0 then
      begin
      Result := PascalFiles[0];
      exit
      end;
   finally
   PascalFiles.Free;
   end;
End;

// VERIFY THE SYSTEM FILES AT LAUNCH
Function verifyfiles():boolean;
var
  FirsTimeUser : UserData;
Begin
Result := false;
// verify if data folder exists
if directoryexists('DATA') = false then
   begin
   Application.ProcessMessages;
   CreateDir('DATA');
   outputtext('DATA folder created');
   end;
// verify if user options file exists
if not FileExists(CONST_ArchivoUser) then
   begin
   Form1.ButtonConsolaOnClick(ButtonConsola);
   assignfile (FilaUser,CONST_ArchivoUser);
   rewrite(FilaUser);
   FirsTimeUser.ListeningPort:=IntToStr(CONST_DefaultServerPort);
   if GetEnvironmentVariable('OPENSSL_CONF') = '' then
      Begin
      form1.mainmemo.Lines.Add('Finding OpennSSL');
      form1.mainmemo.Lines.Add('This can take a while, please, be patient...');
      Application.ProcessMessages;
      FirsTimeUser.OpenSSLPath:=GetOpenSSLPath();
      if FirsTimeUser.OpenSSLPath = '' then
         begin
         ShowMessage('Unable to find OpennSSL. Install it and try again.');
         Application.Terminate;
         end
      else
         begin
         form1.mainmemo.Lines.Add('OpenSSL found at: '+FirsTimeUser.OpenSSLPath);
         end;
      end
   else
      begin
      FirsTimeUser.OpenSSLPath := GetEnvironmentVariable('OPENSSL_CONF');
      FirsTimeUser.OpenSSLPath :=StringReplace(FirsTimeUser.OpenSSLPath,'cfg','exe',[rfReplaceAll, rfIgnoreCase]);
      form1.mainmemo.Lines.Add('OpenSSL Installed at: '+FirsTimeUser.OpenSSLPath);
      end;
   FirsTimeUser.Mining:=false;
   FirsTimeUser.Reconnect:=true;
   FirsTimeUser.CPUmining:=1;
   FirsTimeUser.MinimToTray:=false;
   FirsTimeUser.AutoConnect:=false;
   FirsTimeUser.FullNode := true;
   FirsTimeUser.GetNodes := false;
   write(FilaUser,FirsTimeUser);
   OptionsData := FirsTimeUser;
   closefile(FilaUser);
   end
else
   begin
   OptionsData := LoadOptionsFromDisk();
   end;
// Verify if accounts balance summary exist
if not FileExists(CONST_ArchivoAccData) then
   begin
   assignfile (FilaAccData,CONST_ArchivoAccData);
   rewrite(FilaAccData);
   closefile(FilaAccData);
   end;
// verify if block folder exists
if directoryexists(CONST_DIRBLOCKS) = false then
   begin
   CreateDir(CONST_DIRBLOCKS);
   outputtext('BLOCKS folder created');
   CreateBlockZero();
   end;
InicializeTime();
CheckBoxMiner.Checked := OptionsData.Mining;
Form1.IdTCPServer1.DefaultPort:=StrToInt(OptionsData.ListeningPort);
ComboCPUMiner.ItemIndex:=OptionsData.CPUmining - 1;
EditUserPort.Text:=OptionsData.ListeningPort;
CheckBoxMin.Checked := OptionsData.MinimToTray;
CheckBoxAutoConn.Checked := OptionsData.AutoConnect;
CheckBoxFullNode.Checked := OptionsData.FullNode;
CheckBoxUpdateNodes.Checked := OptionsData.GetNodes;
// Verify if blocksummary file exists {to be implemented}
if not FileExists(CONST_ArchivoBlockSum) then
   begin
   assignfile (FilaSumary,CONST_ArchivoBlockSum);
   rewrite(FilaSumary);
   closefile(FilaSumary);
   end;
// verify if nodes file exists.
if not FileExists(CONST_ArchivoNodos) then
   begin
   assignfile (FilaNodos,CONST_ArchivoNodos);
   rewrite(FilaNodos);
   // add seed nodes here
   closefile(FilaNodos);
   AddNewnode('190.39.22.202','8080');
   AddNewNode('190.79.192.44','8080');
   SaveNodesToDisk();
   outputtext('Nodes not found. You will need add then manually.');
   end;
// verify if blacklisted nodes file exists.
if not FileExists(CONST_Blacklist) then
   begin
   assignfile (FilaBlackList,CONST_Blacklist);
   rewrite(FilaBlackList);
   closefile(FilaBlackList);
   outputtext('Blacklisted nodes file created.');
   end;
// in not wallet.dat found, then create a new one
if not FileExists(CONST_ArchivoWallet) then
   begin
   GetNewWallet()
   end;
LoadWalletData();
Result := true;
End;

{*******************************************************************************
                                       NODES
*******************************************************************************}

// RETURNS IF A NODE EXISTS OR NOT
function NodeExists(ip,port:String):boolean;
Var
  contador: integer = 0;
Begin
result := false;
while contador < length(arraynodos) do
   begin
   if ((arraynodos[contador].ip = ip) and (arraynodos[contador].port = port)) then result := true;
   contador := contador+1;
   end;
end;

// RETURNS IF A BLACKLISTED NODE EXISTS OR NOT
function BLNodeExists(ip,port:String):boolean;
Var
  contador: integer = 0;
Begin
result := false;
while contador < length(ArrayBlacklisted) do
   begin
   if ((ArrayBlacklisted[contador].ip = ip) and (ArrayBlacklisted[contador].port = port)) then result := true;
   contador := contador+1;
   end;
end;

// LOAD NODES FROM DISK
Procedure LoadNodesFromDisk();
Var
  DataLeida : nodedata;
  contador: integer;
Begin
assignfile (FilaNodos,CONST_ArchivoNodos);
contador := 0;
reset (FilaNodos);
SetLength(ArrayNodos,0);
SetLength(ArrayNodos, filesize(FilaNodos));
while contador < (filesize(FilaNodos)) do
   begin
   seek (FilaNodos, contador);
   read (FilaNodos, DataLeida);
   Arraynodos[contador] := dataleida;
   contador := contador + 1;
   end;
closefile(FilaNodos);
End;

// LOAD BLACKLISTED NODES FROM DISK
Procedure LoadBLNodesFromDisk();
Var
  DataLeida : nodedata;
  contador: integer;
Begin
assignfile (FilaBlacklist,CONST_Blacklist);
contador := 0;
reset (FilaBlacklist);
SetLength(ArrayBlacklisted,0);
SetLength(ArrayBlacklisted, filesize(FilaBlacklist));
while contador < (filesize(FilaBlacklist)) do
   begin
   seek (FilaBlacklist, contador);
   read (FilaBlacklist, DataLeida);
   ArrayBlacklisted[contador] := dataleida;
   contador := contador + 1;
   end;
closefile(FilaBlacklist);
End;

// ADD A NEW NODE
Procedure AddNewNode(Address,Port:String);
Var
  DataLeida : nodedata;
Begin
Dataleida.ip:=Address;
Dataleida.port:=Port;
dataleida.LastAvailable:=GetTimeStamp();
SetLength(arraynodos,Length(arraynodos)+1);
arraynodos[length(arraynodos)-1] := Dataleida;
U_SaveNodeFile := true;
UpdateGridNodes();
End;

// ADD A NEW BLACKLISTED NODE
Procedure AddNewBLNode(Address,Port:String);
Var
  DataLeida : nodedata;
Begin
Dataleida.ip:=Address;
Dataleida.port:=Port;
dataleida.LastAvailable:=GetTimeStamp();
SetLength(ArrayBlacklisted,Length(ArrayBlacklisted)+1);
ArrayBlacklisted[length(ArrayBlacklisted)-1] := Dataleida;
U_SaveBlacklist := true;
UpdateBLGridNodes();
End;

// DELETE AN EXISTING NODE
Procedure DeleteExistingNode(Number:Integer);
Begin
while Number < length(ArrayNodos) do
   begin
   arraynodos[Number] := arraynodos[Number+1];
   Number := Number+1;
   end;
SetLength(arraynodos,Length(arraynodos)-1);
U_SaveNodeFile := true;
UpdateGridNodes();
End;

// DELETE AN EXISTING BLACKLISTED NODE
Procedure DeleteBlackListedNode(Number:Integer);
Begin
while Number < length(ArrayBlackListed) do
   begin
   ArrayBlackListed[Number] := ArrayBlackListed[Number+1];
   Number := Number+1;
   end;
SetLength(ArrayBlackListed,Length(ArrayBlackListed)-1);
U_SaveBlacklist := true;
UpdateBLGridNodes();
End;

// SAVE NODES ARRAY TO DISK
Procedure SaveNodesToDisk();
Var
  contador : integer = 0;
Begin
assignfile (FilaNodos,CONST_ArchivoNodos);
rewrite (FilaNodos);
while contador < length(arraynodos) do
   begin
   write(FilaNodos,arraynodos[contador]);
   contador := contador+1;
   end;
closefile(FilaNodos);
U_SaveNodeFile := false;
end;

// SAVE BLACKLISTED NODES ARRAY TO DISK
Procedure SaveBLNodesToDisk();
Var
  contador : integer = 0;
Begin
assignfile (FilaBlacklist,CONST_Blacklist);
rewrite (FilaBlacklist);
while contador < length(ArrayBlacklisted) do
   begin
   write(FilaBlacklist,ArrayBlacklisted[contador]);
   contador := contador+1;
   end;
closefile(FilaBlacklist);
U_SaveBlacklist := false;
end;

// DELETE A NODE FROM ITS ADDRESS
Procedure DeleteNodeAddress(Address:String);
var
  Resultado : Integer = -1;
  Contador : Integer;
Begin
For contador := 0 to length(ArrayNodos)-1 do
   If ArrayNodos[contador].ip = Address then Resultado := Contador;
If Resultado > -1 then DeleteExistingNode(Resultado);
end;

{*******************************************************************************
                                   USER OPTIONS
*******************************************************************************}

// LOAD DEFAULT OPTIONS DATA
Function LoadOptionsFromDisk(): UserData;
var
  DataFromFile:UserData;
begin
assignfile (FilaUser,CONST_ArchivoUser);
reset(FilaUser);
read(filauser,DataFromFile);
closefile(FilaUser);
result := DataFromFile;
end;

// SAVE TO DISK THE CURRENT OPTIONS DATA
procedure SaveOptionsToDisk();
Begin
assignfile (FilaUser,CONST_ArchivoUser);
rewrite(FilaUser);
write(FilaUser,OptionsData);
closefile(FilaUser);
U_SaveOptions := false;
end;

{*******************************************************************************
                                   NETWORK DATA
*******************************************************************************}

// UPDATE THE NETWORK BLOCK DATA: LAST AND HASH
function UpdateNetworkLastBlockData():Integer;
var
  contador : Integer = 1;
   Higher, slot : Integer;
Begin
Higher := -1;
for contador := 1 to CONST_MAXConections do
   begin
   if StrToIntDef(Conexiones[contador].Lastblock,0) > Higher then
      begin
      Higher := StrToIntDef(Conexiones[contador].Lastblock,0);
      slot := contador;
      end;
   end;
NETWORK_LastBLock := Higher;
NETWORK_LastBlockHash := Conexiones[slot].LastblockHash;
result := slot;
End;

// UPDATES THE NETWORKS ACCSUM DATA: LAST AND HASH
function UpdateNetworkAccountSumData():Integer;
var
  contador : Integer = 1;
  Higher, slot : Integer;
Begin
Higher := -1;
for contador := 1 to CONST_MAXConections do
   begin
   if StrToIntDef(Conexiones[contador].Accounts,0) > Higher then
      begin
      Higher := StrToIntDef(Conexiones[contador].Accounts,0);
      slot := contador;
      end;
   end;
NETWORK_LastAccount := Higher;
NETWORK_AccsumHash := Conexiones[slot].AccountsHash;
result := slot;
End;

// UPDATES THE NETWORK PENDING TRXS
function UpdateNetworkPendingData():Integer;
var
  contador : Integer = 1;
  Higher, slot : Integer;
Begin
Higher := -1;
for contador := 1 to CONST_MAXConections do
   begin
   if StrToIntDef(Conexiones[contador].Pending,0) > Higher then
      begin
      Higher := StrToIntDef(Conexiones[contador].Pending,0);
      slot := contador;
      end;
   end;
NETWORK_Pending := Higher;
result := slot;
End;

{*******************************************************************************
                              ACCOUNT AND ADDRESSES
*******************************************************************************}

// IF the USER OWNS A SPECIFIED ADDRESS RETURNS INDEX OF ArrayMyAddresses, ELSE RETURNS -1
function IsAddressMine(address:string):integer;
var
  Contador : Integer = 0;
Begin
result := -1;
for contador := 0 to length(ArrayMyAddresses) - 1 do
   begin
   if ArrayMyAddresses[contador].Hash = address then
      begin
      result := contador;
      exit;
      end;
   end;
End;

// LOAD MY ADDRESSES DATA FROM DISK
Procedure LoadWalletData();
var
  ReadData: WalletData;
  WriteData : MyWalletData;
  contador: integer = 0;
Begin
assignfile (FilaWallet,CONST_ArchivoWallet);
reset(FilaWallet);
SetLength(ArrayMyAddresses,0);
SetLength(ArrayMyAddresses, filesize(FilaWallet));
while contador < (filesize(FilaWallet)) do
   begin
   seek (FilaWallet, contador);
   read(FilaWallet,ReadData);
   WriteData.Hash:= ReadData.Hash;
   WriteData.PublicKey:=ReadData.PublicKey;
   WriteData.PrivateKey:=ReadData.PrivateKey;
   WriteData.Balance:=IntToStr(GetAddressBalanceFromDisk(ReadData.Hash));
   WriteData.RegisterStatus:=0;
   ArrayMyAddresses[contador] := WriteData;
   if contador = 0 then MAIN_AccountNumber := GetAccountNumberFromAddress(WriteData.Hash);
   contador := contador + 1;
   end;
closefile(FilaWallet);
MAIN_AccountHash:= ArrayMyAddresses[0].Hash;
MAIN_AccountPublic := ArrayMyAddresses[0].PublicKey;
MAIN_AccountPrivate := ArrayMyAddresses[0].PrivateKey;
MAIN_AccountBalance := GetTotalAccountBalance();
end;

// SAVE THE ADDRESSES ARRAY TO DISK (Wallet.dat)
Procedure SaveAddressesToDisk();
var
  ReadData: MyWalletData;
  WriteData : WalletData;
  contador: integer = 0;
Begin
assignfile (FilaWallet,CONST_ArchivoWallet);
rewrite(FilaWallet);
while contador < length(ArrayMyAddresses) do
   begin
   ReadData := ArrayMyAddresses[contador];
   WriteData.Hash:=ReadData.Hash;
   WriteData.PublicKey:=ReadData.PublicKey;
   WriteData.PrivateKey:=ReadData.PrivateKey;
   write(FilaWallet,WriteData);
   contador := contador + 1;
   end;
closefile(FilaWallet);
U_SaveWallet := false;
End;

// CALCULATES THE TOTAL ACCOUNT BALANCE
function GetTotalAccountBalance():Int64;
var
  Contador : Integer = 0;
  Total : int64 = 0;
Begin
for contador := 0 to length(ArrayMyAddresses) - 1 do
   begin
   if StrToInt64(ArrayMyAddresses[contador].Balance)>-1 then Total := Total + StrToInt64(ArrayMyAddresses[contador].Balance);
   end;
result := total;
end;

// RETURNS THE TOTAL PENDING PAYMENTS FROM THE ACCOUNT
function GetTotalAccountPendingPayments():Int64;
var
  Contador : Integer = 0;
  Total : int64 = 0;
  ThisAddress  : int64 = 0;
begin
for contador := 0 to length(ArrayMyAddresses) - 1 do
   begin
   ThisAddress := 0;
   ThisAddress := GetAddressPaymentsOnPending(ArrayMyAddresses[Contador].Hash);
   Total := Total+ThisAddress;
   end;
result := total;
end;

// RETURNS THE AVAILABLE BALANCE IN A ADDRESS
function GetAddressAvailable(Address:String): Int64;
var
  Balance : Int64;
  Pending : Int64;
Begin
if IsAddressMine(Address)>=0 then Balance := StrToInt64(ArrayMyAddresses[IsAddressMine(Address)].Balance)
else Balance := GetAddressBalanceFromDisk(Address);
Pending := GetAddressPaymentsOnPending(Address);
Result := Balance-Pending;
End;

// RETURNS THE ACCOUNT NUMBER OF A SPECIFIED ADDRESS
function GetAccountNumberFromAddress(address:string):string;
var
  DataRead : AccountData;
  contador : integer = 0;
Begin
Result := '-1';
assignfile (FilaAccData,CONST_ArchivoAccData);
Reset(FilaAccData);
While contador < filesize(FilaAccData) do
   begin
   seek(FilaAccData,contador);
   read(FilaAccData,DataRead);
   if Dataread.Hash = address then
      begin
      result := IntToStr(contador);
      Closefile(FilaAccData);
      exit;
      end;
   contador := contador +1;
   end;
Closefile(FilaAccData);
End;

// RETURNS THE ADDRESS  OF A SPECIFIED ACCOUNT NUMBER
function GetAddressFromAccountNumber(Accnumber:integer):String;
var
  DataRead : AccountData;
Begin
Result := '';
assignfile (FilaAccData,CONST_ArchivoAccData);
Reset(FilaAccData);
   try
   seek(FilaAccData,Accnumber);
   read(FilaAccData,DataRead);
   result := DataRead.Hash;
   Except
      on E:Exception do
      begin
      end;
   end;
Closefile(FilaAccData);
End;

// RETURNS THE ADDRESS BALANCE FROM SUMARY; -1 if account is not registered yet
function GetAddressBalanceFromDisk(Account:String):Int64;
var
  DataRead : AccountData;
  contador : integer = 0;
Begin
Result := -1;
assignfile (FilaAccData,CONST_ArchivoAccData);
Reset(FilaAccData);
While contador < filesize(FilaAccData) do
   begin
   seek(FilaAccData,contador);
   read(FilaAccData,DataRead);
   if Dataread.Hash = Account then
      begin
      result := StrToInt64(dataread.Balance);
      Closefile(FilaAccData);
      exit;
      end;
   contador := contador +1;
   end;
Closefile(FilaAccData);
end;

// RETURNS THE ADDRESS PUBLIC KEY
function GetAddressPubKey(Address:String):String;
var
  DataRead : AccountData;
  contador : integer = 0;
Begin
Result :='FAIL';
assignfile (FilaAccData,CONST_ArchivoAccData);
Reset(FilaAccData);
While contador < filesize(FilaAccData) do
   begin
   seek(FilaAccData,contador);
   read(FilaAccData,DataRead);
   if Dataread.Hash = Address then
      begin
      result := DataRead.PublicKey;
      Closefile(FilaAccData);
      exit;
      end;
   contador := contador +1;
   end;
Closefile(FilaAccData);
End;

//CHECK IF MY ADDRESSES ARE REGISTERED; IF SO, AND NO PUBKEY, SEND THE ACRE REQUEST
procedure CheckIfMyAddressesNeedsRegisterFromDisk();
var
  DataRead : AccountData;
  Contador : Integer = 0;
Begin
assignfile (FilaAccData,CONST_ArchivoAccData);
Reset(FilaAccData);
While contador < filesize(FilaAccData) do
   begin
   seek(FilaAccData,contador);
   read(FilaAccData,DataRead);
   if ((IsAddressMine(Dataread.Hash) > -1) and (Dataread.PublicKey='')) then
      begin
      ArrayMyAddresses[IsAddressMine(Dataread.Hash)].RegisterStatus := 1;
      OutGoingMessages.Add('{MIC ACRE '+
      '$timestamp$'+
      ' 0.0.0.0 '+
      ArrayMyAddresses[IsAddressMine(Dataread.Hash)].PublicKey+' '+
      ArrayMyAddresses[IsAddressMine(Dataread.Hash)].Hash+' '+
      HashMD5String(GetTimeStamp()+'0.0.0.0'+ArrayMyAddresses[IsAddressMine(Dataread.Hash)].PublicKey+ArrayMyAddresses[IsAddressMine(Dataread.Hash)].Hash)+' '+
      GetStringSigned('MY ADDRESS',ArrayMyAddresses[IsAddressMine(Dataread.Hash)].PrivateKey)+
      ' END}');
      end;
   if ((IsAddressMine(Dataread.Hash) > -1) and (Dataread.PublicKey<>'')) then
      ArrayMyAddresses[IsAddressMine(Dataread.Hash)].RegisterStatus := 2;
   contador := contador +1;
   end;
Closefile(FilaAccData);
UpdateMyAddresses();
End;

// CHECK IF ADDRESS EXISTS IN ACCSUM; IF NOT, ADD IT AT THE END
procedure AddAddressIfNotExists(address:string);
var
  DataRead : AccountData;
  contador : integer = 0;
  NewAccount : AccountData;
Begin
assignfile (FilaAccData,CONST_ArchivoAccData);
Reset(FilaAccData);
While contador < filesize(FilaAccData) do
   begin
   seek(FilaAccData,contador);
   read(FilaAccData,DataRead);
   if Dataread.Hash = address then
      begin
      Closefile(FilaAccData);
      exit;
      end;
   contador := contador +1;
   end;
// add the new address
seek(FilaAccData,filesize(FilaAccData));
NewAccount := Default(AccountData);
NewAccount.Number := IntToStr(filesize(FilaAccData));
NewAccount.PublicKey:='';
NewAccount.Hash:=address;
NewAccount.Balance:='0';
NewAccount.Lastop:='0';
Write(FilaAccData,NewAccount);
Closefile(FilaAccData);
if IsAddressMine(NewAccount.Hash) > -1 then
   begin
   ArrayMyAddresses[IsAddressMine(NewAccount.Hash)].RegisterStatus:=1;
   UpdateMyAddresses();
   end;
if length(ArrayMyAddresses) > 0 then
   If NewAccount.Hash = ArrayMyAddresses[0].Hash then U_MyAccountNumber := True;
End;

// VERIFY IF ANY USER ADDRESS HAVE REGISTER STATUS = 1 AND SEND ACRE
Procedure VerifyUserAddressesForAcre();
var
  counter:Integer;
Begin
for counter := 0 to length(ArrayMyAddresses)-1 do
   begin
   if ArrayMyAddresses[counter].RegisterStatus = 1 then
      OutGoingMessages.Add('{MIC ACRE '+
      '$timestamp$'+
      ' 0.0.0.0 '+
      ArrayMyAddresses[counter].PublicKey+' '+
      ArrayMyAddresses[counter].Hash+' '+
      HashMD5String(GetTimeStamp()+'0.0.0.0'+ArrayMyAddresses[counter].PublicKey+ArrayMyAddresses[counter].Hash)+' '+
      GetStringSigned('MY ADDRESS',ArrayMyAddresses[counter].PrivateKey)+
      ' END}');
   end;
End;

// RETURNS THE NUMBER OF ACCOUNTS IN THE ACCOUNT SUMMARY
function GetLastAccountUpdated():Integer;
Begin
assignfile (FilaAccData,CONST_ArchivoAccData);
reset(FilaAccData);
result := filesize(FilaAccData);
LOCAL_MyLastAccount :=  filesize(FilaAccData);
closefile(FilaAccData);
U_MyLastAccount := false;
End;

// RETURN THE HASH OF MY ACCOUNT SUMMARY
function GetMyAccSumHash():String;
begin
LOCAL_MyAccsumHash := HashMD5File(CONST_ArchivoAccData);
Result := LOCAL_MyAccsumHash;
U_MyAccsumHash := false;
End;

{*******************************************************************************
                             BLOCKS AND BLOCKSUM
*******************************************************************************}

// RETURNS THE LAST DOWNLOADED BLOCK
function GetMyLastUpdatedBlock():Integer;
Var
  BlockFiles : TStringList;
  contador : Integer = 0;
  LastBlock : Integer = 0;
  OnlyNumbers : String;
Begin
BlockFiles := TStringList.Create;
FindAllFiles(BlockFiles, CONST_DIRBLOCKS, '*.blk', true);
while contador < BlockFiles.Count do
   begin
   OnlyNumbers := copy(BlockFiles[contador], 13, length(BlockFiles[contador])-16);
   if StrToInt(OnlyNumbers) > Lastblock then LastBlock := StrToInt(OnlyNumbers);
   contador := contador+1;
   end;
BlockFiles.Free;
LOCAL_MyLastBlock := LastBlock;
Result := LastBlock;
U_MylastBlock := false;
end;

// RETURNS THE LAST BLOCK HASH
function GetMyLastBLockHash(Lastblock:String):String;
begin
LOCAL_LastBlockHash := HashMD5File(CONST_DirBlocks+lastblock+'.blk');
Result := LOCAL_LastBlockHash;
U_MyLastBLockHash := false;
end;

// RETURNS THE LAST BLOCK BUILD FROM SUMMARY
function BlockSumLastBlock():Integer;
Begin
if length(ArrBlockSummary) > 0 then Result := StrToInt(ArrBlockSummary[length(ArrBlockSummary)-1].Number)
else result := 0;
end;

// RETURNS THE BLOCK DATA FROM BLOCKSUM
function GetBlockData(blnumber:Integer):BlockSumData;
var
  contador : Integer = 0;
Begin
for contador := 0 to length(ArrBlockSummary)-1 do
   begin
   if StrToInt(ArrBlockSummary[contador].Number) = blnumber then
      begin
      Result := ArrBlockSummary[contador];
      exit;
      end;
   end;
end;

// RETURNS THE BLOCK DATA FROM DISK FOR SUMMARY
function GetBlockDataFromDisk(BlockNumber:Integer):BlockSumData;
var
  Resultado : BlockSumData;
  MemStr: TMemoryStream;
  Header : BlockHeaderData;
  ArchData : String;
Begin
ArchData := CONST_DIRBLOCKS+IntToStr(BlockNumber)+'.blk';
MemStr := TMemoryStream.Create;
   try
   MemStr.LoadFromFile(ArchData);
   MemStr.Position := 0;
   MemStr.Read(Header, SizeOf(Header));
   finally
   MemStr.Free;
   end;
Resultado := Default(BlockSumData);
Resultado.Number      := IntToStr(BlockNumber);
Resultado.TimeStart   := IntToStr(Header.TimeStart);
Resultado.TimeEnd     := IntToStr(Header.TimeEnd);
Resultado.TimeTot     := IntToStr(Header.TimeTot);
Resultado.TrxTot      := IntToStr(Header.TrxTot);
Resultado.TargetHash  := Header.TargetHash;
Resultado.Difficult   := Header.Difficult;
Resultado.NxtBlkDiff  := Header.NxtBlkDiff;
Resultado.BlockHash   := HashMD5File(ArchData);
Resultado.AccountMiner:= Header.AccountMiner;
Result := resultado;
End;

// BUILD THE BLOCK SUMMARY
Procedure BuildBlockSum();
var
  FirstBlock, Arrlen : Integer;
  Contador : Integer;
Begin
if LOCAL_MyLastBlock > -1 then
   begin
   SetLength(ArrBlockSummary,0);
   FirstBlock := LOCAL_MyLastBlock - CONST_BlockSummaryLength+1;
   if FirstBlock < 0 then FirstBlock := 0;
   ArrLen := LOCAL_MyLastBlock-FirstBlock+1;
   SetLength(ArrBlockSummary,ArrLen);
   For Contador := 0 to Arrlen-1 do
      begin
      ArrBlockSummary[contador] := GetBlockDataFromDisk(FirstBlock+Contador);
      end;
   end;
End;

// ADJUST BLOCKSUM TO MAXIMUN SIZE
Procedure AdjustBlockSum();
var
  Number : integer = 0;
Begin
if length(ArrBlockSummary) > CONST_BlockSummaryLength then
   begin
   while Number < length(ArrBlockSummary) do
      begin
      ArrBlockSummary[Number] := ArrBlockSummary[Number+1];
      Number := Number+1;
   end;
   SetLength(ArrBlockSummary,Length(ArrBlockSummary)-1);
   end;
End;

{*******************************************************************************
                                   CONNECTIONS
*******************************************************************************}

// RETURNS THE NUMBER OF CONTEXTS OF SERVER
function ClientsCount : Integer ;
var
  Clients : TList;
begin
Clients := TList.Create;
Clients:=form1.IdTCPServer1.Contexts.LockList;
   try
   Result := Clients.Count ;
   finally
   form1.IdTCPServer1.Contexts.UnlockList;
   Clients.Free;
   end;
end ;

// RETURN THE NUMBER OF ACTIVE CONNECTIONS (CLIENT OR SERVER)
function GetActiveConex(tipo:string):Integer;
var
  contador : Integer = 1;
Begin
result := 0;
for contador := 1 to CONST_MAXConections do
   begin
   if Conexiones[contador].tipo = tipo then result := result + 1;
   end;
End;

// RETURN THE NUMBER OF TOTAL ACTIVE CONEX
function GetTotalConex():Integer;
var
  contador : Integer = 1;
Begin
result := 0;
for contador := 1 to CONST_MAXConections do
   begin
   if Conexiones[contador].tipo <> '' then result := result + 1;
   end;
End;

// SAVE CONECTION TO SLOT
function SaveConection(tipo,ipuser,timestamp:String;contextdata:TIdContext;clientnumber:string):integer;
var
  contador : integer = 1;
  Slot : integer = 0;
begin
For contador := 1 to CONST_MAXConections do
   begin
   if Conexiones[contador].tipo = '' then
      begin
      Conexiones[contador].Autentic:=false;
      Conexiones[contador].Connections:=0;
      Conexiones[contador].tipo := tipo;
      Conexiones[contador].ip:= ipuser;
      Conexiones[contador].lastping:=Gettimestamp();
      Conexiones[contador].context:=contextdata;
      Conexiones[contador].ClientConn:=clientnumber;
      Conexiones[contador].Lastblock:='0';
      Conexiones[contador].Accounts:='0';
      Conexiones[contador].Pending:='0';
      Conexiones[contador].ListenPort:='0';
      slot := contador;
      ReadsFromSlots[slot].Clear;
      break;
      end;
   end;
result := slot;
end;

// CLEAR A CONECTION SLOT
function ClearConection(tipo,ipuser:string):boolean;
var
  contador : integer = 1;
begin
result := false;
while contador < CONST_MAXConections+1 do
   begin
   if ((Conexiones[contador].tipo=tipo) and (Conexiones[contador].ip=ipuser)) then
      begin
      Conexiones[contador].Autentic:=false;
      Conexiones[contador].Connections:=0;
      Conexiones[contador].tipo := '';
      Conexiones[contador].ip:= '';
      Conexiones[contador].lastping:='';
      Conexiones[contador].ClientConn:='0';
      Conexiones[contador].Lastblock:='0';
      Conexiones[contador].LastblockHash:='';
      Conexiones[contador].Accounts:='0';
      Conexiones[contador].AccountsHash:='';
      Conexiones[contador].Pending:='0';
      Conexiones[contador].ListenPort:='0';
      result := true;
      break;
      end;
   contador := contador+1;
   end;
end;

// RETURNS A FREE CONECTION SLOT OR 0 IF NONE
function GetFreeConexSlot():Integer;
var
  contador : Integer = 1;
begin
for contador := 1 to CONST_MAXConections do
   begin
   if Conexiones[contador].tipo = '' then
      begin
      result := contador;
      exit;
      end;
   end;
result := 0;
end;

// RETURNS THE REACHABLE NODES
function GetReachableNodes():Integer;
var
  contador : Integer = 1;
begin
result := 0;
for contador := 1 to CONST_MAXConections do
   begin
   if Conexiones[contador].tipo <> '' then
      begin
      result := result+Conexiones[contador].Connections;
      end;
   end;
end;

// RETURNS A FREE CLIENT OR 0 IF NONE
function GetFreeCliente():Integer;
var
  contador : integer = 1;
begin
for contador := 1 to CONST_MaxOutgoingConections do
   begin
   if not ConexionesCliente[contador].connected() then
      begin
      Result := contador;
      exit;
      end;
   end;
Result := 0;
end;

// CONNECT AS CLIENT
function ConnectClient(Address,Port:String):integer;
var
  freeconex, freecliente : integer;
  ConContext : TIdContext; // EMPTY
Begin
ConContext := Default(TIdContext);
freeconex := GetFreeConexSlot();
if freeconex = 0 then
   begin
   result := 0;
   exit;
   end;
FreeCliente := GetFreeCliente();
if FreeCliente = 0 then
   begin
   result := 0;
   exit;
   end;
ConexionesCliente[FreeCliente].Host:=Address;
ConexionesCliente[FreeCliente].Port:=StrToInt(Port);
   try
   ConexionesCliente[FreeCliente].ConnectTimeout:= 200;
   ConexionesCliente[FreeCliente].Connect;
   SaveConection('server',Address,GetTimestamp(),ConContext, IntToStr(FreeCliente));
   OutputText('Connected TO: '+Address);
   ConexionesCliente[FreeCliente].IOHandler.WriteLn('{MIC KREDITZ '+Address+' END}');
   ConexionesCliente[FreeCliente].IOHandler.WriteLn('{MIC JOIN '+
   IntTostr(GetTotalConex)+' '+
   GetTimeStamp()+' '+
   IntToStr(LOCAL_MyLastBlock)+' '+
   LOCAL_LastBlockHash+' '+
   IntToStr(LOCAL_MyLastAccount)+' '+
   IntToStr(PendingTXs.Count)+' '+
   OptionsData.ListeningPort+' '+
   LOCAL_MyAccsumHash+
   ' END}');
   If OptionsData.GetNodes then
     ConexionesCliente[FreeCliente].IOHandler.WriteLn('{MIC GETNODES END}');
   result := FreeConex;
   Except
   on E:Exception do
      begin
      //OutputText(Address+': '+E.Message);
      result := 0;
      exit;
      end;
   end;
End;

// RETURNS IF WE ARE CONNECTED TO A GIVEN IP
Function AreWeConnectedTo(address:String):Boolean;
var
  contador : Integer = 1;
Begin
for contador := 1 to CONST_MAXConections do
   begin
   if Conexiones[contador].ip = address then
      begin
      result := true;
      exit;
      end;
   end;
Result := false;
End;

// RETURNS THE SLOT OF THE GIVEN IP
function GetSlotFromIP(Ip:String):Integer;
var
  contador : integer;
Begin
for contador := 1 to CONST_MAXConections do
   begin
   if conexiones[contador].ip=ip then
      begin
      result := contador;
      exit;
      end;
   end;
Result := 0;
end;

// CONNECT TO SERVERS
Procedure ConnectToServers();
var
  contador : integer = 0;
begin
while contador < length(ArrayNodos) do
   begin
   if ((not AreWeConnectedTo(Arraynodos[contador].ip)) AND (GetActiveConex('server')<CONST_MaxOutgoingConections)) then
      begin
      ConnectClient(Arraynodos[contador].ip,Arraynodos[contador].port);
      end;
   contador := contador +1;
   end;
CONNECT_LastTime := StrToInt64(GetTimeStamp());
end;

// CONNECT TO A SPECIFIED NODE
Procedure TryConnectToNode(NodeNumber:Integer);
Begin
if ((not AreWeConnectedTo(Arraynodos[NodeNumber].ip)) AND
   (GetActiveConex('server')<CONST_MaxOutgoingConections) AND
   (Nodenumber<Length(ArrayNodos))) then
   begin
   ConnectClient(Arraynodos[NodeNumber].ip,Arraynodos[NodeNumber].port);
   end;
CONNECT_LastTime := StrToInt64(GetTimeStamp());
CONNECT_LastNode := NodeNumber;
End;

// CLOSE ALL OUTGOING (tipo:server) CONECTIONS
Procedure CloseConectionsToServers();
var
  Contador: Integer;
Begin
for contador := 1 to CONST_MAXConections do
   begin
   if conexiones[contador].tipo='server' then
      begin
      ConexionesCliente[contador].Disconnect;
      OutputText('Disconnected outgoing: '+conexiones[contador].ip);
      ClearConection('server',conexiones[contador].ip);
      end;
   end;
End;

// ENABLE SERVER
procedure TurnListenOn();
Begin
   try
   Form1.IdTCPServer1.Bindings.Clear;
   Form1.IdTCPServer1.DefaultPort:=StrToInt(OptionsData.ListeningPort);
   Form1.IdTCPServer1.Active:=true;
   OutputText('Server ENABLED. Listening on port '+OptionsData.ListeningPort);
   except
   on E : Exception do
   OutputText('Unable to start Server');
   end;
end;

// DISABLE SERVER (DISCONNECT ALL INCOMING FIRST)
procedure TurnListenOff();
var
  Contador: Integer;
Begin
for contador := 1 to CONST_MAXConections do
   begin
   if conexiones[contador].tipo='client' then  CloseConnectionSlot(contador);
   end;
Form1.IdTCPServer1.Active:=false;
OutputText('Incoming connections disabled');
end;

// CLOSE A GIVEN CONNECTION SLOT AND SENDS A MESSAGE
procedure CloseConnectionSlot(Slot:Integer);
begin
if conexiones[Slot].tipo='client' then
   begin
   ReadsFromSlots[slot].Clear;
   Conexiones[Slot].context.Connection.IOHandler.InputBuffer.Clear;
   Conexiones[Slot].context.Connection.Disconnect;
   ClearConection('client',conexiones[Slot].ip);
   end;
if conexiones[Slot].tipo='server' then
   begin
   ReadsFromSlots[slot].Clear;
   ConexionesCliente[Slot].IOHandler.InputBuffer.Clear;
   ConexionesCliente[Slot].Disconnect;
   ClearConection('server',conexiones[Slot].ip);
   end;
end;

{*******************************************************************************
                                  UPDATES GUI
*******************************************************************************}

// UPDATES THE PANEL FOR STATUS
procedure UpdatePanelStatus();
var
  SimbolOk : String;
Begin
if LOCAL_MyAccsumHash = NETWORK_AccsumHash then SimbolOk:='✔'
else SimbolOk :='✘';
PanelStatus.Color:=clRed;PanelStatus.Caption:='Disconnected';PanelStatus.Hint:='Disconnected';
if STATUS_Connected then
   begin
   PanelStatus.Color:=clYellow;
   PanelStatus.Caption:=IntToStr(LOCAL_MyLastBlock)+'/'+IntToStr(NETWORK_LastBLock)+SimbolOk;
   PanelStatus.Hint:='Connected to Network';
   end;
if STATUS_Synzed then
   begin
   PanelStatus.Color:=clBackground;
   PanelStatus.Caption:=IntToStr(LOCAL_MyLastBlock)+'/'+IntToStr(NETWORK_LastBLock)+SimbolOk;
   PanelStatus.Hint:='Synchronizing from Network';
   end;
if STATUS_Updated then
   begin
   PanelStatus.Color:=clGreen;
   PanelStatus.Caption:=IntToStr(LOCAL_MyLastBlock)+'/'+IntToStr(NETWORK_LastBLock)+SimbolOk;
   PanelStatus.Hint:='Updated and Ready';
   end;
End;

// UPDATES PANEL SERVER
procedure UpdatePanelServer();
Begin
if form1.IdTCPServer1.Active then PanelServer.Color:=clGreen
else PanelServer.Color:=clRed;
PanelServer.Caption:=IntToStr(GetActiveConex('client'))+'/'+IntToStr(GetActiveConex('server'))+' ';
end;

// UPDATES THE GRID CONTAINING THE NODES
Procedure UpdateGridNodes();
var
  contador : integer = 0;
Begin
GridNodes.ColCount:=2;
GridNodes.RowCount:=length(arraynodos)+1;
if length(arraynodos)>0 then
   begin
   GridNodes.ColWidths[0]:= 100;GridNodes.ColWidths[1]:= 50;
   while contador < length(arraynodos) do
      begin
      GridNodes.Cells[0,contador+1] := arraynodos[contador].ip;
      GridNodes.Cells[1,contador+1] := arraynodos[contador].port;
      contador := contador+1;
      end;
   end;
End;

// UPDATES THE GRID CONTAINING THE BLACKLISTED NODES // FIX
procedure UpdateBLGridNodes();
var
  contador : integer = 0;
Begin
GridBLNodes.ColCount:=1;
GridBLNodes.RowCount:=length(ArrayBlacklisted)+1;
if length(ArrayBlacklisted) > 0 then
   begin
   GridBLNodes.ColWidths[0]:= 100;
   while contador < length(ArrayBlacklisted) do
      begin
      GridBLNodes.Cells[0,contador+1] := ArrayBlacklisted[contador].ip;
      contador := contador+1;
      end;
   end;
End;

// UPDATES THE GRID CONTAINING THE CONNECTION SLOTS INFO
procedure UpdateGridConxs();
var
  contador : integer;
  ShowClientTipe, ShowLB, ShowLBHash : String;
Begin
GridConxs.ColWidths[0]:= 20;GridConxs.ColWidths[1]:= 100;GridConxs.ColWidths[2]:= 60;
GridConxs.ColWidths[3]:= 60;GridConxs.ColWidths[4]:= 60;
for contador := 1 to CONST_MAXConections do
   begin
   if Conexiones[contador].tipo = '' then
      begin
      ShowClientTipe := 'EMPTY';
      ShowLB := '';
      ShowLBHash := '';
      end
   else
      begin
      ShowClientTipe := Uppercase(Conexiones[contador].tipo);
      if StrToIntDef(Conexiones[contador].Lastblock,0) = LOCAL_MyLastBlock then ShowLB := '✔'
      else ShowLB := '✘';
      if Conexiones[contador].LastblockHash = LOCAL_LastBlockHash then ShowLBHash := '✔'
      else ShowLBHash := '✘';
      end;
   GridConxs.Cells[0,contador]:=IntToStr(contador);
   GridConxs.Cells[1,contador]:=Conexiones[contador].ip;
   GridConxs.Cells[2,contador]:=ShowClientTipe;
   GridConxs.Cells[3,contador]:=ShowLB;
   GridConxs.Cells[4,contador]:=ShowLBHash;
   end;
End;

// UPDATES THE GRID CONTAINING THE WALLET ADDRESSES
Procedure UpdateMyAddresses();
var
  contador : integer = 0;
  BalanceToShow : String;
  RegisterStatus : String = '';
Begin
GridAddresses.RowCount:=1;GridAddresses.ColCount:=2;
GridAddresses.RowCount:=length(ArrayMyAddresses)+1;
if length(ArrayMyAddresses)>0 then
   begin
   GridAddresses.ColWidths[0]:= 250;GridAddresses.ColWidths[1]:= 100;
   while contador < length(ArrayMyAddresses) do
      begin
      if length(ArrayMyAddresses) > 0 then
         begin
         if ArrayMyAddresses[contador].RegisterStatus = 0 then RegisterStatus:='<U>'
         else if ArrayMyAddresses[contador].RegisterStatus = 1 then RegisterStatus:='<P>'
         else RegisterStatus:='<R>';
         end;
      GridAddresses.Cells[0,contador+1] := ArrayMyAddresses[contador].Hash+RegisterStatus;
         if GetAddressAvailable(ArrayMyAddresses[contador].Hash) = -1 then BalanceToShow := '0'
         else BalanceToShow := Int2CurrencyStr(GetAddressAvailable(ArrayMyAddresses[contador].Hash));
      GridAddresses.Cells[1,contador+1] := BalanceToShow;
      contador := contador+1;
      end;
   end;
End;

// UPDATES THE PANEL SHOWING THE MINER INFO
procedure UpdatePanelMiner();
var
  minerondata: string;
Begin
if ((MINER_IsMinerOn) and (copy(LabelMiner.Caption,13,10)='Off')) then minerondata := PanelMiner.Caption
else minerondata := copy(LabelMiner.Caption,13,10);
if MINER_IsMinerOn then
   begin
   PanelMiner.Caption:=minerondata;
   PanelMiner.Color:=clGreen;
   PanelMiner.Hint:='Miner Working';
   end
else
   begin
   PanelMiner.Caption:='MINER: OFF';
   PanelMiner.Color:=clred;
   PanelMiner.Hint:='Miner Stopped';
   end;
End;

// UPDATES THE USER WALLET PENDING TXS
Procedure UpdateMyPendingTxs();
var
  contador : Integer;
  Tipo, Sender, Receiver, Monto : String;
  Ammount : Int64;
begin
MyPendingTxs.Clear;
For contador := 0 to PendingTxs.Count-1 do
   begin
   Tipo := GetParameterFromCommandLine(PendingTxs[contador],1);
   if tipo = 'TRFR' then
      begin
      Sender := GetParameterFromCommandLine(PendingTxs[contador],3);
      Receiver := GetParameterFromCommandLine(PendingTxs[contador],4);
      Monto := GetParameterFromCommandLine(PendingTxs[contador],5);
      if IsAddressMine(sender) >= 0 then MyPendingTxs.Add('-'+monto);
      if IsAddressMine(Receiver) >= 0 then MyPendingTxs.Add('+'+monto);
      end;
   end;
GridPending.RowCount:=1;GridPending.ColCount:=1;
GridPending.RowCount:=MyPendingTxs.Count+1;
contador := 0;
if MyPendingTxs.Count>0 then
   begin
   GridPending.ColWidths[0]:= 210;
   while contador < MyPendingTxs.Count do
      begin
      Ammount := StrToInt64(MyPendingTxs[contador]);
      if Ammount > 0 then Ammount := Ammount - GetComisionValue(StrToInt64(MyPendingTxs[contador]));
      GridPending.Cells[0,contador+1] := Int2CurrencyStr(Ammount);
      contador := contador+1;
      end;
   end;
end;

// SHOWS THE MENSAJE PANEL
Procedure ShowMensaje(Exito:Boolean);
Begin
if Exito then
   begin
   PanelMensaje.Color:=clgreen;PanelMensaje.Caption:='Success';PanelMensaje.Visible:=true;
   PanelMensaje.BringToFront;
   end
else
   begin
   PanelMensaje.Color:=clRed;PanelMensaje.Caption:='Failed';PanelMensaje.Visible:=true;
   PanelMensaje.BringToFront;
   end;
Form1.TimerMensaje.Enabled:=true;
End;

END. // END UNIT

