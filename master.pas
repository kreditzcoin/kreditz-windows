unit MASTER;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, FileUtil, Forms, Controls, ExtCtrls, Graphics, StdCtrls,
  IdContext, grids, fphttpclient,IdSNTP, dateutils, MD5,DCPsha256, DCPripemd160,
  StrUtils, Process, IdGlobal, Zipper;

type
  nodedata = Packed Record
     ip: string[15];
     port: string[8];
     LastAvailable : string[17];
     end;

  conectiondata = Packed Record
     Autentic: boolean;
     Connections : int64;
     tipo: string[8];
     ip: string[20];
     lastping: string[255];
     context: TIdContext;
     ClientConn: string[255];
     Lastblock: string[255];
     LastblockHash: string[255];
     Accounts: string[255];
     AccountsHash : string[255];
     Pending: string[255];
     ListenPort: string[255];
     Version : String[13];
     Listening : boolean;
     offset : string[4];
     end;

  UserData = Packed Record
     ListeningPort: string[8];
     OpenSSLPath : String[255];
     Mining: boolean;
     Reconnect : Boolean;
     CPUmining : int64;
     MinimToTray : Boolean;
     AutoConnect : Boolean;
     FullNode : Boolean;
     GetNodes : Boolean;
     ShowMinned : Boolean;
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
     Concept : String[40];
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
     AccountMiner : String[40];
     MinerFee : String[10];
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
     RegisterStatus : int64;
     end;

  BlockHeaderData = Packed Record
     Number         : Int64;
     TimeStart      : Int64;
     TimeEnd        : Int64;
     TimeTot        : int64;
     TrxTot         : int64;
     TargetHash     : String[34];
     Difficult      : String[3];
     NxtBlkDiff     : String[3];
     AccountMiner   : String[40];
     MinerFee       : Int64;
     Reward         : Int64;
     SolutionLength : int64;
     end;

  BlockArrTrxsData = array of TranxData;

  // General
  function IsValidInt(cadena:string):boolean;
  function IsValidFloat(cadena:String):Boolean;
  function GetOpenSSLPath():String;
  function verifyfiles():boolean;

  // GUI relatives
  function Int2CurrencyStr(Value: int64): string;
  procedure UpdateLabels();
  procedure Outputtext(TextToShow:String;showhour:boolean=true);
  procedure ClearMemoLines();

  // Nodes
  function NodeExists(ip,port:String):boolean;
  function BLNodeExists(ip,port:String):boolean;
  procedure LoadNodesFromDisk();
  procedure LoadBLNodesFromDisk();
  procedure AddNewNode(Address,Port:String);
  procedure AddNewBLNode(Address,Port:String);
  procedure DeleteExistingNode(Number:int64);
  procedure DeleteBlackListedNode(Number:int64);
  procedure SaveNodesToDisk();
  procedure SaveBLNodesToDisk();
  procedure DeleteNodeAddress(Address:String);
  function GetReachableNodes():int64;

  // User options
  function LoadOptionsFromDisk(): UserData;
  procedure SaveOptionsToDisk();

  // Network data
  function UpdateNetworkLastBlockData():int64;
  function UpdateNetworkAccountSumData(): int64;
  function UpdateNetworkPendingData():int64;

  // Account and addresses
  function IsAddressMine(address:string):int64;
  procedure LoadWalletData();
  procedure SaveAddressesToDisk();
  function GetTotalAccountBalance():Int64;
  function GetTotalAccountPendingPayments():Int64;
  function GetAddressAvailable(Address:String): Int64;
  function GetAccountNumberFromAddress(address:string):string;
  function GetAddressFromAccountNumber(Accnumber:int64):String;
  function GetAddressBalanceFromDisk(Account:String):Int64;
  function GetAddressPubKey(Address:String):String;
  procedure CheckIfMyAddressesNeedsRegisterFromDisk();
  procedure AddAddressIfNotExists(address:string);
  procedure VerifyUserAddressesForAcre();
  function GetLastAccountUpdated():int64;
  function GetMyAccSumHash():String;

  // Blocks and blocksum
  function GetMyLastUpdatedBlock():int64;
  function GetMyLastBLockHash(Lastblock:String):String;
  function BlockSumLastBlock():int64;
  function GetBlockData(blnumber:int64):BlockSumData;
  function GetBlockDataFromDisk(BlockNumber:int64):BlockSumData;
  procedure BuildBlockSum();
  procedure AdjustBlockSum();
  function GetMyLastUpdatedBlockTrxs():integer;
  procedure SetMyLastUpdatedBlockTrxs(Number:integer);
  function GetBlockTrxsFromDisk(BlockNumber:integer):BlockArrTrxsData;
  procedure SaveToMyTrxs(transaction : TranxData);
  procedure BuiltMyTrxs();

  // Connections
  function GetActiveConex(tipo:string):int64;
  function GetTotalConex():int64;
  function SaveConection(tipo,ipuser,timestamp:String;contextdata:TIdContext;clientnumber:string):int64;
  function ClearConection(tipo,ipuser:string):boolean;
  function GetFreeConexSlot():int64;
  function GetFreeCliente():int64;
  function ConnectClient(Address,Port:String):int64;
  function AreWeConnectedTo(address:String):Boolean;
  function GetSlotFromIP(Ip:String):int64;
  procedure ConnectToServers();
  procedure TryConnectToNode(NodeNumber:int64);
  procedure CloseConectionsToServers();
  procedure TurnListenOff();
  procedure TurnListenOn();
  procedure CloseConnectionSlot(Slot:int64);

  // Updates GUI
  procedure UpdatePanelStatus();
  procedure UpdatePanelServer();
  Procedure UpdateGridNodes();
  procedure UpdateBLGridNodes();
  procedure UpdateGridConxs();
  Procedure UpdateMyAddresses();
  procedure UpdatePanelMiner();
  Procedure UpdateMyPendingTxs();
  Procedure ShowMensaje(Exito:Boolean);
  Procedure UpdateUserTrxs();

  // TimeUnit
  function GetNetworkTimestamp(hostname:string):String;
  function GetLocalTimestamp():string;
  function GetTimestamp():string;
  Procedure InicializeTime();
  function getSNTPStringTime(): String;
  function TimestampToDate(timestamp:string):String;

  // Blocks unit
  function BuildNewBlock(blNumber:int64;TimeStamp,Account,Solution,NewBlHash, TargetHash,difficulty:String): boolean;
  procedure SaveNewBlockToDisk(BlockHeader:BlockHeaderData;ArrBlockTxs: Array of TranxData;Solution, filename:string);
  function GetDiffForNextBlock(block,ThisBlockTime:int64):String;
  function DecreaseDiff(minediff:String;variation:int64):String;
  function IncreaseDiff(minediff:String;variation:int64):String;
  function GetLast20Average(ThisBlockTime:int64):int64;
  function GetBlockReward(BlNumber:int64):Int64;
  procedure AddFundsToAddress(Address:String;Amount:Int64;block:int64);
  procedure SetAddressPubKey(Address,PubKey:String;block:int64);
  function GetNegativeValue(number:int64):int64;
  procedure CreateBlockZero();
  function UndoneLastBlock(BlNumber:int64 = -1):Boolean;

  // CommandLineParser
  Procedure ParseCommandLine(Linetext:String);
  Function GetCommandLineCommand(LineText:String):String;
  Function GetParameterFromCommandLine(LineText:String;ParamNumber:int64):String;
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
  Procedure SetCPUMiners(linetext:String);
  Procedure MinToTask(linetext:String);
  Procedure AutoConn(linetext:String);
  Procedure FullNode(linetext:String);
  Procedure UpdateNodes(linetext:String);
  Procedure ShowMinned(linetext:String);

  // Crypto
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

  // Protocol
  function IsValidProtocol(line:String):Boolean;
  function JoinString():String;
  procedure ParseProtocolConnection(Lines:TStringList;Slot:int64);
  procedure PTC_Join(TextLine:String;slot:int64;Answer:boolean);
  procedure PTC_ServerClose(slot:int64);
  procedure SendOutGoingMessages();
  procedure SendPTCMessage(Slot:int64;Message:String);
  procedure PTC_ACRE(Textline:String;slot:int64);
  function TranxAlreadyPending(TypeOfTrx, ValTrx:String;ParamNum:int64):boolean;
  procedure PTC_SendPending(Slot:int64);
  function ReplaceTimestamp(TextLine:String):String;
  procedure AddPendingByTimestamp(Pending:String);
  procedure PTC_Newblock(TextLine:String);
  procedure SendAccDataFile(slot:int64);
  procedure PTC_TRFR(Textline:string;slot:int64);
  function TrxExistsInLastBlock(hash:String):boolean;
  function GetNodesString():string;
  procedure PTC_GetNodes(Textline:string;slot:int64);
  procedure PTC_SaveNodes(Textline:string;slot:int64);
  function GetComisionValue(Monto:int64):int64;
  function SendFundsFromAddress(Destination:String; AddressIndex,Restante:int64;concepto:string):int64;
  function GetAddressPaymentsOnPending(Address:String):Int64;
  procedure PTC_SendZipedBlocks(textline:string;slot:int64);
  procedure UnzipBlockFile(filename:String);
  function GetCharsFromMinerDiff(minerdiff:string):int64;
  function GetStepsFromMinerDiff(minerdiff:string):int64;
  function GetNodeFromString(DataString:String):NodeData;
  procedure PTC_InBlRe(Textline:string;slot:int64);

Const
  CONST_ArchivoNodos = 'DATA/nodes.min';
  CONST_Blacklist = 'DATA/blnodes.min';
  CONST_ArchivoUser = 'DATA/user.min';
  CONST_ArchivoAccData = 'DATA/accsum.dat';
  CONST_DirBlocks = 'DATA/BLOCKS/';
  CONST_ArchivoWallet = 'DATA/wallet.dat';
  CONST_ArchivoBlockSum = 'DATA/blocksum.dat';
  CONST_ArchivoMyTxs = 'DATA/mytxs.kdz';

  CONST_MinimunConnectionsToWork = 1;
  CONST_PinsToStart = CONST_MinimunConnectionsToWork*2;
  CONST_BlockSummaryLength = 20;
  CONST_ExpectedBlockDuration = 300;
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
  FilaMytxs : file of TranxData;
    U_RebuildMyTxs : Boolean = false;

  OptionsData : UserData; // Options values
  LastCommandline : String = '';
  AutoRetryConnect : boolean = false; // Auto retry connection to nodes
  OpenSSLPath : String = '';
  GLOBAL_TimeDiff : Integer = 0;

  CONNECT_LastTime : Int64 = 0;
  CONNECT_LastNode : int64 = 0;

  STATUS_Connected : boolean = false;
  STATUS_Synzed : boolean = false;
    STATUS_LastBlockRequested : int64 = 0;
    STATUS_LastAccountRequested : boolean = false;
  STATUS_Updated: boolean = false;
  STATUS_IncomingPings : int64 = 0;
  STATUS_LastPing : int64 = 0;

  LASTBLOCK_PendingTxs : TStringlist;
  LASTBLOCK_UNDONE : int64 = 0;
  LASTBLOCK_ArrBlockSum : Array of BlockSumData;
  LASTBLOCK_ArrMyAddresses : Array of MyWalletData;
  LASTBLOCK_Duration : int64;
  LASTBLOCK_TrxsIDs : TStringlist;

  MAIN_FirstFormShow : boolean = true;
  MAIN_MinOnTray : boolean = false;
  MAIN_AccountNumber : String = '';
    U_MyAccountNumber : boolean = false;
  MAIN_AccountHash: String = '';
  MAIN_AccountBalance : int64 = 0;
    U_MyBalance : boolean = false;
  MAIN_AccountPublic : String = '';
  MAIN_AccountPrivate : String = '';
  MAIN_Version : string = '0.180811';
  MAIN_USER_IP : String = '';
  MAIN_CPUCOUNT : int64 = 0;
  MAIN_ProgramStarted : boolean = false;

  MINER_IsMinerOn : Boolean = false; // is the miner activated?
  MINER_HashCounter     : int64 = 0; // the longint counter for mining
  MINER_LastHashCounter : int64 = 0; // used to calculate the mining hash speed
  MINER_HashSeed : String = ''; // the prefix for mining
  MINER_BlockFound : boolean = false; // true when the user mines the block
  MINER_ResultHash : String = ''; // result when block is found
  MINER_FoundedSteps : int64 = 0;  // founded steps of this block
  MINER_TargetHash : String = ''; // TARGET HASH TO FOUND THE NEW BLOCK
  MINER_MineDiff : String = 'fe';  // string containing the block difficulty
  MINER_TargetChars : int64 = 6; // Number of consecutive operations to mine the block
  MINER_Steps : int64 = 1; // number of steps to mine the block
  MINER_BlockDiffSet : int64 = -1; // block the user is mining
  MINER_CurrStepTarget : String = '';

  LOCAL_MyLastBlock : int64 = 0; // last block the user have in disk
    U_MylastBlock : Boolean = false;
  LOCAL_LastBlockHash : String = ''; // hash of the last user block on disk
    U_MyLastBLockHash : Boolean = false;
  LOCAL_MyLastAccount : int64 = 0; // last account user have registered on disk
    U_MyLastAccount : boolean = false;
  LOCAL_MyAccsumHash : String = ''; // Hash of the local account sumary
    U_MyAccsumHash : boolean = false;

  NETWORK_LastBLock  : int64 = 0; // lastblock in the network
  NETWORK_LastBlockHash : String = ''; // target hash for next block
  NETWORK_LastAccount : int64 = 0; // acounnts registered on network
  NETWORK_AccsumHash : String = '';  // Accounts file hash
  NETWORK_Pending : int64 = 0;
    NETWORK_SendPing : boolean = false;

  ArrayNodos :  array of nodedata;
  ArrayBlacklisted : array of nodedata;
  ArrayMyAddresses : array of MyWalletData;
  ReadsFromSlots : array [1..CONST_MAXConections] of TStringList;
  ArrayThreads : Array of int64;
  PendingTXs : TStringList;
  OutGoingMessages: TStringList;
  MainMemoStrings: TStringList;
  ProcessLines : TStringList;
  MyPendingTxs : TStringList;  // Pending Txs involving my addresses
  ArrBlockSummary : Array of BlockSumData;
  Conexiones : array [1..CONST_MaxIncomingConections+CONST_MaxOutgoingConections] of conectiondata;

    FormNotify: TForm;
      MemoNotify : TMemo;
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
      LabelOptions : TLabel;
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
        LabelShowMinned : Tlabel;
        CheckBoxMinned :  TCheckBox;

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
        LabelConcept : TLabel;
        EditConcept : TEdit;
        LabelCorAmo : TLabel;
        ButtonSendFunds : TButton;
      GridPending : TStringGrid;
      GrisUserTrxs : TStringGrid;
        ButtonDetails : TButton;

    PanelServer : TPanel;
      CheckBoxConnect : TCheckBox;
    PanelStatus : TPanel;
    PanelMiner : TPanel;
      CheckBoxMiner : TCheckBox;
    PanelMensaje : TPanel;

implementation

Uses
  MC_Main;

{*******************************************************************************
                                    GENERAL
*******************************************************************************}

// DETECTS IF A STRING IS A VALID int64
function IsValidInt(cadena:string):boolean;
begin
Result := True;
   Try
   StrToInt64(cadena);
   except
   On E : EConvertError do Result := false;
   end;
end;

// DETECTS IF A STRING IS A VALID FLOAT
function IsValidFloat(cadena:String):Boolean;
begin
Result := True;
   Try
   StrtoFloat(cadena);
   except
   On E : EConvertError do Result := false;
   end;
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
  MiFirstTrx : TranxData;
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
   FirsTimeUser.ShowMinned:=true;
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
CheckBoxMinned.Checked := OptionsData.ShowMinned;
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
   closefile(FilaNodos);
   // add seed nodes here
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
// verify file with my txs
if not FileExists(CONST_ArchivoMyTxs) then
   begin
   MiFirstTrx := default(TranxData);
   assignfile (FilaMytxs,CONST_ArchivoMyTxs);
   rewrite(FilaMytxs);
   MiFirstTrx.block:='0';
   write(FilaMytxs,MiFirstTrx);
   closefile(FilaMytxs);
   outputtext('User transactions file created.');
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
                                    GUI RELATIVES
*******************************************************************************}

 // SHOWS THE BALANCE AS CURRENCY
function Int2CurrencyStr(Value: int64): string;
begin
  result := Format('%.2n', [value * 0.01]);
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
LabelConections.Caption:='Conections: '+IntTostr(GetActiveConex('client'))+'/'+IntTostr(GetActiveConex('server'));
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
if STATUS_Connected then LastPingMsg := IntToStr(StrToInt64(copy(GetTimeStamp(),1,10))- STATUS_LastPing)+' segs'
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

{*******************************************************************************
                                       NODES
*******************************************************************************}

// RETURNS IF A NODE EXISTS OR NOT
function NodeExists(ip,port:String):boolean;
Var
  contador: int64 = 0;
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
  contador: int64 = 0;
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
  contador: int64;
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
  contador: int64;
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
Procedure DeleteExistingNode(Number:int64);
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
Procedure DeleteBlackListedNode(Number:int64);
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
  contador : int64 = 0;
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
  contador : int64 = 0;
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
  Resultado : int64 = -1;
  Contador : integer;
Begin
For contador := 0 to length(ArrayNodos)-1 do
   If ArrayNodos[contador].ip = Address then Resultado := Contador;
If Resultado > -1 then DeleteExistingNode(Resultado);
end;

// RETURNS THE REACHABLE NODES
function GetReachableNodes():int64;
var
  contador : integer = 1;
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
function UpdateNetworkLastBlockData():int64;
var
  contador : integer = 1;
   Higher, slot : int64;
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
function UpdateNetworkAccountSumData():int64;
var
  contador : integer = 1;
  Higher, slot : int64;
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
function UpdateNetworkPendingData():int64;
var
  contador : integer = 1;
  Higher, slot : int64;
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
function IsAddressMine(address:string):int64;
var
  Contador : integer = 0;
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
  contador: int64 = 0;
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
  contador: int64 = 0;
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
  Contador : integer = 0;
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
  Contador : integer = 0;
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
  contador : int64 = 0;
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
function GetAddressFromAccountNumber(Accnumber:int64):String;
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
  contador : int64 = 0;
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
  contador : int64 = 0;
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

// CHECK IF MY ADDRESSES ARE REGISTERED; IF SO, AND NO PUBKEY, SEND THE ACRE REQUEST
procedure CheckIfMyAddressesNeedsRegisterFromDisk();
var
  DataRead : AccountData;
  Contador : int64 = 0;
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
  contador : int64 = 0;
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
  counter:integer;
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
function GetLastAccountUpdated():int64;
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
function GetMyLastUpdatedBlock():int64;
Var
  BlockFiles : TStringList;
  contador : int64 = 0;
  LastBlock : int64 = 0;
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
function BlockSumLastBlock():int64;
Begin
if length(ArrBlockSummary) > 0 then Result := StrToInt(ArrBlockSummary[length(ArrBlockSummary)-1].Number)
else result := 0;
end;

// RETURNS THE BLOCK DATA FROM BLOCKSUM
function GetBlockData(blnumber:int64):BlockSumData;
var
  contador : integer = 0;
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

// RETURNS THE BLOCK DATA FROM DISK FOR SUMMARY (Headers)
function GetBlockDataFromDisk(BlockNumber:int64):BlockSumData;
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
Resultado.MinerFee    := IntToStr(Header.MinerFee);
Result := resultado;
End;

// BUILD THE BLOCK SUMMARY
Procedure BuildBlockSum();
var
  FirstBlock, Arrlen : int64;
  Contador : integer;
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
  Number : int64 = 0;
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

// returns the last block from where user trxs was extracted
function GetMyLastUpdatedBlockTrxs():integer;
var
  FirstTrx : TranxData;
Begin
assignfile (FilaMytxs,CONST_ArchivoMyTxs);
reset(FilaMytxs);
seek(FilaMytxs,0);
read(FilaMytxs,FirstTrx);
Closefile(FilaMytxs);
Result := StrToInt(FirstTrx.block);
end;

// SET THE LAST CHECKED BLOCK FOR USER TRXS
Procedure SetMyLastUpdatedBlockTrxs(Number:integer);
var
  FirstTrx : TranxData;
Begin
FirstTrx := Default(TranxData);
FirstTrx.block:=IntToStr(Number);
assignfile (FilaMytxs,CONST_ArchivoMyTxs);
reset(FilaMytxs);
seek(FilaMytxs,0);
write(FilaMytxs,FirstTrx);
Closefile(FilaMytxs);
End;

// returns an array with all the trxs from a specified block
function GetBlockTrxsFromDisk(BlockNumber:integer):BlockArrTrxsData;
var
  ArrTrxs : BlockArrTrxsData;
  MemStr: TMemoryStream;
  Header : BlockHeaderData;
  ArchData : String;
  counter : integer;
  TotalTrxs : integer;
Begin
Setlength(ArrTrxs,0);
ArchData := CONST_DIRBLOCKS+IntToStr(BlockNumber)+'.blk';
MemStr := TMemoryStream.Create;
   try
   MemStr.LoadFromFile(ArchData);
   MemStr.Position := 0;
   MemStr.Read(Header, SizeOf(Header));
   TotalTrxs := header.TrxTot;
   SetLength(ArrTrxs,TotalTrxs);
   For Counter := 0 to TotalTrxs-1 do
      MemStr.Read(ArrTrxs[Counter],Sizeof(ArrTrxs[Counter])); // read each record
   Except on E: Exception do // nothing, the block is not founded
   end;
MemStr.Free;
Result := ArrTrxs;
End;

// SAVE TRANSACTION TO MY TRXS
procedure SaveToMyTrxs(transaction : TranxData);
Begin
assignfile (FilaMytxs,CONST_ArchivoMyTxs);
reset(FilaMytxs);
seek(FilaMytxs,filesize(FilaMytxs));
write(FilaMytxs,transaction);
Closefile(FilaMytxs);
end;

// BUILT USER TRXS
Procedure BuiltMyTrxs();
var
  LastUpdatedBlock : integer;
  counter,counter2 : integer;
  BlockArrTrxs : array of TranxData;
  SaveMsj : String;
  SavedTrxs : integer = 0;
  Header : BlockSumData;
  TrxMinned: TranxData;
Begin
SaveMsj := PanelStatus.Caption;
LastUpdatedBlock := GetMyLastUpdatedBlockTrxs();
for counter := LastUpdatedBlock+1 to LOCAL_MyLastBlock do
   begin
      try
      BlockArrTrxs := Copy(GetBlockTrxsFromDisk(counter));
      for counter2 := 0 to length(BlockArrTrxs)-1 do
         begin
         if BlockArrTrxs[counter2].TypeTx = 'TRFR' then
            begin
            if ((IsAddressMine(BlockArrTrxs[counter2].Sender)>-1) or (IsAddressmine(BlockArrTrxs[counter2].Receiver)>-1)) then
               begin
               SaveToMyTrxs(BlockArrTrxs[counter2]);
               SavedTrxs := SavedTrxs+1;
               end;
            end;
         end;
      PanelStatus.Caption:= IntToStr(counter);
      application.ProcessMessages;
      finally
      end;
   Header := GetBlockDataFromDisk(counter);
   if IsAddressMine(header.AccountMiner)>-1 then
      begin
      TrxMinned := default(Tranxdata);
      TrxMinned.block := header.Number;
      TrxMinned.TypeTx:= 'MINE';
      TrxMinned.TimeStamp:= header.TimeEnd;
      TrxMinned.Sender:= header.NxtBlkDiff;
      TrxMinned.Receiver:= header.AccountMiner;
      TrxMinned.Ammount:= IntToStr(StrToInt64(header.MinerFee)+GetBlockReward(StrToInt64(header.Number)));
      TrxMinned.Signature:= '';
      TrxMinned.Hash:= '';
      TrxMinned.Concept:= '';
      SaveToMyTrxs(TrxMinned);
      SavedTrxs := SavedTrxs+1;
      end;
   end;
if LOCAL_MyLastBlock > LastUpdatedBlock+1 then
  OutputText('Blocks Rebuilded: '+IntToStr(LastUpdatedBlock+1)+' to '+IntToStr(LOCAL_MyLastBlock));
PanelStatus.Caption := SaveMsj;
SetMyLastUpdatedBlockTrxs(LOCAL_MyLastBlock);// set last block checked
U_RebuildMyTxs := false;
if SavedTrxs > 0 then UpdateUserTrxs();
End;

{*******************************************************************************
                                   CONNECTIONS
*******************************************************************************}

// RETURN THE NUMBER OF ACTIVE CONNECTIONS (CLIENT OR SERVER)
function GetActiveConex(tipo:string):int64;
var
  contador : integer = 1;
Begin
result := 0;
for contador := 1 to CONST_MAXConections do
   begin
   if Conexiones[contador].tipo = tipo then result := result + 1;
   end;
End;

// RETURN THE NUMBER OF TOTAL ACTIVE CONEX
function GetTotalConex():int64;
var
  contador : integer = 1;
Begin
result := 0;
for contador := 1 to CONST_MAXConections do
   begin
   if Conexiones[contador].tipo <> '' then result := result + 1;
   end;
End;

// SAVE CONECTION TO SLOT
function SaveConection(tipo,ipuser,timestamp:String;contextdata:TIdContext;clientnumber:string):int64;
var
  contador : integer = 1;
  Slot : int64 = 0;
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
  contador : int64 = 1;
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
      Conexiones[contador].Version:='';
      result := true;
      break;
      end;
   contador := contador+1;
   end;
end;

// RETURNS A FREE CONECTION SLOT OR 0 IF NONE
function GetFreeConexSlot():int64;
var
  contador : integer = 1;
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

// RETURNS A FREE CLIENT OR 0 IF NONE
function GetFreeCliente():int64;
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
function ConnectClient(Address,Port:String):int64;
var
  freeconex, freecliente : int64;
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
   ConexionesCliente[FreeCliente].IOHandler.WriteLn(JoinString());
   If OptionsData.GetNodes then
     ConexionesCliente[FreeCliente].IOHandler.WriteLn('{MIC GETNODES END}');
   result := FreeConex;
   Except
   on E:Exception do
      begin
      OutputText(Address+': '+E.Message);
      result := 0;
      exit;
      end;
   end;
End;

// RETURNS IF WE ARE CONNECTED TO A GIVEN IP
Function AreWeConnectedTo(address:String):Boolean;
var
  contador : integer = 1;
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
function GetSlotFromIP(Ip:String):int64;
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
  contador : int64 = 0;
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
Procedure TryConnectToNode(NodeNumber:int64);
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
  Contador: integer;
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
  Contador: integer;
Begin
for contador := 1 to CONST_MAXConections do
   begin
   if conexiones[contador].tipo='client' then  CloseConnectionSlot(contador);
   end;
Form1.IdTCPServer1.Active:=false;
OutputText('Incoming connections disabled');
end;

// CLOSE A GIVEN CONNECTION SLOT AND SENDS A MESSAGE
procedure CloseConnectionSlot(Slot:int64);
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
  contador : int64 = 0;
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
  contador : int64 = 0;
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
  ShowClientTipe, ShowLB, ShowLBHash, ShowAccSumHash, ShowVersion, ShowServer, ShowOffset : String;
Begin
GridConxs.ColWidths[0]:= 20;GridConxs.ColWidths[1]:= 100;GridConxs.ColWidths[2]:= 50;
GridConxs.ColWidths[3]:= 30;GridConxs.ColWidths[4]:= 30;GridConxs.ColWidths[5]:= 30;
GridConxs.ColWidths[6]:= 60;GridConxs.ColWidths[7]:= 30;GridConxs.ColWidths[8]:= 30;
for contador := 1 to CONST_MAXConections do
   begin
   if Conexiones[contador].tipo = '' then
      begin
      ShowClientTipe := 'EMPTY';
      ShowLB := '';
      ShowLBHash := '';
      ShowAccSumHash := '';
      ShowVersion := '';
      ShowServer := '';
      ShowOffset := '';
      end
   else
      begin
      ShowClientTipe := Uppercase(Conexiones[contador].tipo);
      if StrToIntDef(Conexiones[contador].Lastblock,0) = LOCAL_MyLastBlock then ShowLB := '✔'
      else ShowLB := '✘';
      if Conexiones[contador].LastblockHash = LOCAL_LastBlockHash then ShowLBHash := '✔'
      else ShowLBHash := '✘';
      if Conexiones[contador].AccountsHash = LOCAL_MyAccsumHash then ShowAccSumHash := '✔'
      else ShowAccSumHash := '✘';
      ShowVersion := Conexiones[contador].Version;
      if Conexiones[contador].listening then ShowServer := '✔'
      else ShowServer := '✘' ;
      ShowOffset := Conexiones[contador].offset;
      end;
   GridConxs.Cells[0,contador]:=IntToStr(contador);
   GridConxs.Cells[1,contador]:=Conexiones[contador].ip;
   GridConxs.Cells[2,contador]:=ShowClientTipe;
   GridConxs.Cells[3,contador]:=ShowLB;
   GridConxs.Cells[4,contador]:=ShowLBHash;
   GridConxs.Cells[5,contador]:=ShowAccSumHash;
   GridConxs.Cells[6,contador]:=ShowVersion;
   GridConxs.Cells[7,contador]:=ShowServer;
   GridConxs.Cells[8,contador]:=ShowOffset;
   end;
End;

// UPDATES THE GRID CONTAINING THE WALLET ADDRESSES
Procedure UpdateMyAddresses();
var
  contador : int64 = 0;
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

// UPDATES GRID CONTAINING THE USER WALLET PENDING TXS
Procedure UpdateMyPendingTxs();
var
  contador : integer;
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
if not MAIN_ProgramStarted then exit;
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

// SHOW USER TRXS
Procedure UpdateUserTrxs();
var
  contador : integer;
  Registros : integer;
  Transaccion : tranxdata;
  Ammount : Int64;
Begin
GrisUserTrxs.RowCount:=1;GrisUserTrxs.ColCount:=8;
assignfile (FilaMytxs,CONST_ArchivoMyTxs);
reset(FilaMytxs);
registros := filesize(FilaMytxs);
for contador := registros-1 downto 0 do
   begin
   seek(FilaMytxs,contador);
   read(FilaMytxs,Transaccion);
   if transaccion.TypeTx = 'TRFR' then
      begin
      if IsAddressMine(transaccion.sender) >= 0 then transaccion.Ammount := '-'+transaccion.Ammount;
      if IsAddressMine(transaccion.Receiver) >= 0 then transaccion.Ammount := '+'+transaccion.Ammount;
      if ((IsAddressMine(transaccion.sender)>-1) or (IsAddressMine(transaccion.Receiver)>-1)) then
         begin
         GrisUserTrxs.RowCount:=GrisUserTrxs.RowCount+1;
         GrisUserTrxs.ColWidths[0]:= 50;GrisUserTrxs.ColWidths[1]:= 160;GrisUserTrxs.ColWidths[2]:= 1;
         GrisUserTrxs.ColWidths[3]:= 1;
         Ammount := StrToInt64(transaccion.Ammount);
         if Ammount < 0 then Ammount := Ammount - GetComisionValue(abs(ammount));
         GrisUserTrxs.Cells[0,GrisUserTrxs.RowCount-1] := transaccion.block;
         GrisUserTrxs.Cells[1,GrisUserTrxs.RowCount-1] := Int2CurrencyStr(Ammount);
         GrisUserTrxs.Cells[2,GrisUserTrxs.RowCount-1] := transaccion.hash;
         GrisUserTrxs.Cells[3,GrisUserTrxs.RowCount-1] := transaccion.TypeTx;
         GrisUserTrxs.Cells[4,GrisUserTrxs.RowCount-1] := transaccion.Sender;
         GrisUserTrxs.Cells[5,GrisUserTrxs.RowCount-1] := transaccion.receiver;
         GrisUserTrxs.Cells[6,GrisUserTrxs.RowCount-1] := transaccion.TimeStamp;
         GrisUserTrxs.Cells[7,GrisUserTrxs.RowCount-1] := transaccion.Concept;
         end;
      end;
   if ((transaccion.TypeTx = 'MINE') and (OptionsData.ShowMinned)) then
      begin
      GrisUserTrxs.RowCount:=GrisUserTrxs.RowCount+1;
      GrisUserTrxs.ColWidths[0]:= 50;GrisUserTrxs.ColWidths[1]:= 160;
      Ammount := StrToInt64(transaccion.Ammount);
      GrisUserTrxs.Cells[0,GrisUserTrxs.RowCount-1] := transaccion.block;
      GrisUserTrxs.Cells[1,GrisUserTrxs.RowCount-1] := Int2CurrencyStr(Ammount);
      GrisUserTrxs.Cells[2,GrisUserTrxs.RowCount-1] := transaccion.hash;
      GrisUserTrxs.Cells[3,GrisUserTrxs.RowCount-1] := transaccion.TypeTx;
      GrisUserTrxs.Cells[4,GrisUserTrxs.RowCount-1] := transaccion.Sender;
      GrisUserTrxs.Cells[5,GrisUserTrxs.RowCount-1] := transaccion.receiver;
      GrisUserTrxs.Cells[6,GrisUserTrxs.RowCount-1] := transaccion.TimeStamp;
      GrisUserTrxs.Cells[7,GrisUserTrxs.RowCount-1] := transaccion.Concept;
      end;
   end;
closefile(FilaMytxs);
End;

{*******************************************************************************
                                  TIME UNIT
*******************************************************************************}

// INITIALIZE TIME
Procedure InicializeTime();
Begin
GLOBAL_TimeDiff := StrToInt64(getSNTPStringTime())-StrToInt64(GetLocalTimestamp());
end;

// LOOPS THE SERVER UNTIL IT GET A VALID ONE
function getSNTPStringTime(): String;
var
  ArraData : array[0..9] of string;
  counter : integer = 0;
Begin
result := '';
Arradata[0] := 'ntp.amnic.net';
Arradata[1] := 'ts2.aco.net';
Arradata[2] := 'hora.roa.es';
Arradata[3] := 'ntp.atomki.mta.hu';
Arradata[4] := 'time.esa.int';
Arradata[5] := 'time.stdtime.gov.tw';
Arradata[6] := 'stratum-1.sjc02.svwh.net';
Arradata[7] := 'ntp3.indypl.org';
Arradata[8] := 'ntp1.sp.se';
Arradata[9] := 'ntp.ntp-servers.com';
while result = '' do
   begin
   result := GetNetworkTimestamp(arradata[counter]);
   Inc(counter);
   if counter > 9 then counter := 0;
   end;
End;

// RETURNS GLOBAL TIMESTAMP
function GetNetworkTimestamp(hostname:string):String;
var
  NTPClient: TIdSNTP;
  OffSet : Integer;
begin
OffSet := GetLocalTimeOffset * 60;
NTPClient := TIdSNTP.Create(nil);
   try
   NTPClient.Host := hostname;
   NTPClient.Active := True;
   result := IntToStr(DateTimeToUnix(NTPClient.DateTime)+Offset)+IntToStr(100+Random(899));
   Except on E:Exception do
   result := '';
   end;
NTPClient.Free;
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

// RETURNS A TIMESTAMP AS A HUMAN READABLE DATE
function TimestampToDate(timestamp:string):String;
var
  AsInteger: integer;
  Fecha : TDateTime;
begin
timestamp := copy(timestamp,1,10);
AsInteger := StrToInt64(timestamp);
fecha := UnixToDateTime(AsInteger);
result := DateTimeToStr(fecha);
end;

{*******************************************************************************
                                 BLOCKS UNIT
*******************************************************************************}

// BUILDS A NEW BLOCK
function BuildNewBlock(blNumber:int64;TimeStamp,Account,Solution,NewBlHash,TargetHash,difficulty:String): boolean;
var
  TrxData : TranxData;
  PendingType : string;
  textLine : String;
  StartBlockTime, ThisBlockHash : String;
  PendingCounter : integer;
  TotalOps : int64 = 0;
  MinerFee : int64 = 0;
  ThisTrxFee : int64 = 0;
  TLAddress, TLAmount: String;
  TransfersList : TStringlist;
  AcresList : TStringlist;
  IgnoredTrxs : TStringlist;
  TrxTimeStamp : String;
  BlockHeader : BlockHeaderData;
  ArrBlockTxs : Array of TranxData;
  filename : String;
Begin
Result := false;
If Solution = '' then Solution := MINER_ResultHash;
If TargetHash = '' then TargetHash := MINER_TargetHash;
if blNumber = 0 then StartBlockTime := '1531896783344'
else StartBlockTime:= GetBlockData(BlockSumLastBlock).TimeEnd;
TotalOps := 0;
TransfersList := TStringlist.Create;
IgnoredTrxs := TStringlist.Create;
AcresList := TStringlist.Create;
LASTBLOCK_PendingTxs.Clear;
LASTBLOCK_TrxsIDs.Clear;
filename := CONST_DIRBLOCKS+IntToStr(BlNumber)+'.blk';
// Create array with Txs *****
if PendingTXs.Count > 0 then
   begin
   SetLength(ArrBlockTxs,0);
   for PendingCounter := 0 to PendingTXs.Count-1 do
      begin
      TextLine := copy(PendingTXs[PendingCounter],6,length(PendingTXs[PendingCounter])-10);
      {If trx is recent, do not include it in the block *****}
      TrxTimeStamp := GetParameterFromCommandLine(TextLine,1);
      if StrToInt64(TrxTimeStamp)+5000 > StrToInt64(TimeStamp) then // TRX is recent so not include in block
         begin
         IgnoredTrxs.Add('{MIC '+TextLine+' END}');
         Continue;
         end;
      {-----}
      PendingType := GetCommandLineCommand(TextLine);
      if PendingType = 'ACRE' then
         begin
         TotalOps := TotalOps + 1;
         TrxData := Default(TranxData);
         TrxData.block:=IntToStr(BlNumber);
         TrxData.TypeTx:='ACRE';
         TrxData.TimeStamp:=GetParameterFromCommandLine(TextLine,1);
         TrxData.Sender:=GetParameterFromCommandLine(TextLine,2);
         TrxData.Receiver:='null';
         TrxData.Ammount:= GetParameterFromCommandLine(TextLine,4);
         TrxData.Signature:= GetParameterFromCommandLine(TextLine,3);
         TrxData.hash:=GetParameterFromCommandLine(TextLine,5);
         SetLength(ArrBlockTxs,Length(ArrBlockTxs)+1);
         ArrBlockTxs[Length(ArrBlockTxs)-1] := TrxData;
         AcresList.Add('ACRE '+TrxData.Ammount+' '+TrxData.Signature);
         end;
      if PendingType = 'TRFR' then
         begin
         TotalOps := TotalOps + 1;
         TrxData := Default(TranxData);
         TrxData.block:=IntToStr(BlNumber);
         TrxData.TypeTx:='TRFR';
         TrxData.TimeStamp:=GetParameterFromCommandLine(TextLine,1);
         TrxData.Sender:=GetParameterFromCommandLine(TextLine,2);
         TrxData.Receiver:=GetParameterFromCommandLine(TextLine,3);
         TrxData.Ammount:= GetParameterFromCommandLine(TextLine,4);
         TrxData.Signature:= GetParameterFromCommandLine(TextLine,5);
         TrxData.hash:=GetParameterFromCommandLine(TextLine,6);
         TrxData.Concept:=GetParameterFromCommandLine(TextLine,7);
           ThisTrxFee := GetComisionValue(StrToInt64(TrxData.Ammount));
           TrxData.Ammount := IntToStr(StrToInt64(TrxData.Ammount)-ThisTrxFee);
         MinerFee := MinerFee + ThisTrxFee;
         TransfersList.Add('TRFR '+TrxData.Receiver+' '+TrxData.Ammount);
         TransfersList.Add('TRFR '+TrxData.Sender+' '+IntToStr(GetNegativeValue(StrToInt64(TrxData.Ammount)+ThisTrxFee)));
         SetLength(ArrBlockTxs,Length(ArrBlockTxs)+1);
         ArrBlockTxs[Length(ArrBlockTxs)-1] := TrxData;
         LASTBLOCK_TrxsIDs.Add(TrxData.hash);
         end;
      end;
   end;
// End of the array with Txs -----
// Set the block Header *****
BlockHeader := Default(BlockHeaderData);
BlockHeader.Number := blNumber;
BlockHeader.TimeStart:= StrToInt64(StartBlockTime);
BlockHeader.TimeEnd:= StrToInt64(TimeStamp);
BlockHeader.TimeTot:= (StrToInt64(TimeStamp)-StrToInt64(StartBlockTime)) div 1000;
BlockHeader.TrxTot:=TotalOps;
BlockHeader.TargetHash:=TargetHash;
BlockHeader.Difficult:=difficulty;
BlockHeader.NxtBlkDiff:=GetDiffForNextBlock(BlNumber,BlockHeader.TimeTot);
BlockHeader.AccountMiner:=Account;
BlockHeader.MinerFee:=MinerFee;
BlockHeader.Reward:=GetBlockReward(BlNumber);
BlockHeader.SolutionLength:=length(Solution);
// End of the block header -----

SaveNewBlockToDisk(BlockHeader,ArrBlockTxs,Solution, filename);

ThisBlockHash := HashMD5File(CONST_DirBlocks+IntToStr(BlNumber)+'.blk');
if NewBlHash = '' then
   begin
   OutputText('You found block:'+IntToStr(BlNumber)+' - Hash:'+ThisBlockHash);
   if MAIN_MinOnTray then
      begin
      Form1.SystrayIcon.BalloonHint:='You found block: '+IntToStr(BlNumber);
      form1.SystrayIcon.BalloonTitle:='Kreditz Miner';
      form1.SystrayIcon.ShowBalloonHint;
      end;
   end
else if ((NewBlHash <> '') and (NewBlHash = ThisBlockHash)) then OutputText('Good Block Hash')
else if ((NewBlHash <> '') and (NewBlHash <> ThisBlockHash)) then
   begin
   // the block hash is wrong so discard it
   OutputText('Wrong Block: '+IntToStr(BlNumber));
   DeleteFile(filename);
   exit;
   end;

outputtext(IntToStr(AcresList.Count)+' Registers - '+IntToStr(TransfersList.Count div 2)+' Trxs - '+IntToStr(IgnoredTrxs.Count)+' Ignored');
// SAVE VALUES TO REBUILD THE BLOCK IF NECESSARY
CopyFile(CONST_ArchivoAccData,CONST_ArchivoAccData+'.bak',true);
CopyFile(CONST_ArchivoMyTxs,CONST_ArchivoMyTxs+'.bak',true);
LASTBLOCK_ArrBlockSum := copy(ArrBlockSummary);
LASTBLOCK_ArrMyAddresses := copy(ArrayMyAddresses);
LASTBLOCK_PendingTxs.AddStrings(PendingTxs);

// PROCESS ACRES {modify accsum}
While AcresList.Count > 0 do
   Begin
   TLAddress := GetParameterFromCommandLine(AcresList[0],1);
   TLAmount := GetParameterFromCommandLine(AcresList[0],2);
   If GetAddressPubKey(TLAddress) = '' then SetAddressPubKey(TLAddress,TLAmount,blNumber);
   if IsAddressMine(TLAddress) >-1 then
      ArrayMyAddresses[IsAddressMine(TLAddress)].RegisterStatus:=2;
   if  AcresList.Count > 0 then AcresList.Delete(0);
   end;

// PROCESS TRANSFERS {modify accsum & ArrayMyAddresses}
While TransfersList.Count > 0 do
   begin
   TLAddress := GetParameterFromCommandLine(TransfersList[0],1);
   TLAmount := GetParameterFromCommandLine(TransfersList[0],2);
   AddAddressIfNotExists(TLAddress);
   AddFundsToAddress(TLAddress,StrToInt64(TLAmount),BlNumber);
     if IsAddressMine(TLAddress) > -1 then
       ArrayMyAddresses[IsAddressMine(TLAddress)].Balance:=
       IntToStr(StrToInt64(ArrayMyAddresses[IsAddressMine(TLAddress)].Balance)+
       StrToInt64(TLAmount));
   if  TransfersList.Count > 0 then TransfersList.Delete(0);
   end;
// MINER PAYMENT {modify accsum & ArrayMyAddresses}
AddAddressIfNotExists(Account);
AddFundsToAddress(Account,GetBlockReward(BlNumber)+MinerFee,blNumber);
if IsAddressMine(Account) > -1 then
   ArrayMyAddresses[IsAddressMine(Account)].Balance:=
   IntToStr(StrToInt64(ArrayMyAddresses[IsAddressMine(Account)].Balance)+GetBlockReward(BlNumber)+MinerFee);
// SET UPDATES
U_MylastBlock := true;
U_MyLastBLockHash := true;
U_MyLastAccount := true;
U_MyAccsumHash := true;
U_MyBalance := true;
U_RebuildMyTxs := true;
PendingTXs.Clear;
While IgnoredTrxs.Count> 0 do
   begin
   PendingTXs.Add(IgnoredTrxs[0]);
   if IgnoredTrxs.Count > 0 then IgnoredTrxs.Delete(0);
   end;
if BlNumber > 0 then OutGoingMessages.Add('{MIC NWBL '+
   TimeStamp+' '+
   IntToStr(BlNumber)+' '+
   Account+' '+
   Solution+' '+
   Hashmd5file(CONST_DIRBLOCKS+IntToStr(BlNumber)+'.blk')+' '+
   TargetHash+' '+
   MINER_MineDiff+
   ' END}');
// ADD BLOCKSUMMARY RECORD {modify blocksum}
SetLength(ArrBlockSummary,length(ArrBlockSummary)+1);
ArrBlockSummary[length(ArrBlockSummary)-1] := GetBlockDataFromDisk(BlNumber);
AdjustBlockSum();
// -----
NETWORK_SendPing := true;
UpdateMyPendingTxs();
TransfersList.Free;
IgnoredTrxs.Free;
AcresList.Free;
if LASTBLOCK_UNDONE = blNumber then OutPutText('BLOCK '+IntToStr(blNumber)+' Rebuilded');
Result := true;
MINER_IsMinerOn := false;
End;

// SAVE BLOCK TO DISK BINARY
Procedure SaveNewBlockToDisk(BlockHeader:BlockHeaderData;ArrBlockTxs: Array of TranxData;Solution, filename:string);
var
  MemStr: TMemoryStream;
  ArrRecords, SolLen : int64;
  counter : integer;
Begin
ArrRecords := BlockHeader.TrxTot;
SolLen := BlockHeader.SolutionLength;
MemStr := TMemoryStream.Create;
   try
   MemStr.Write(BlockHeader,Sizeof(BlockHeader));
   for counter := 0 to ArrRecords-1 do
       MemStr.Write(ArrBlockTxs[counter],Sizeof(ArrBlockTxs[Counter]));
   MemStr.Write(Solution[1],SolLen*sizeof(Solution[1]));
   MemStr.SaveToFile(FileName);
   finally
   MemStr.Free;
   end;
End;

// RETURNS THE DIFFICULT FOR NEXT BLOCK
function GetDiffForNextBlock(block,ThisBlockTime:int64):String;
var
  Last20Ave : int64;
  LastDiff : String;
Begin
if block < 20 then Result := 'fe'
else
   begin
   Last20Ave := GetLast20Average(ThisBlockTime);
   LastDiff := GetBlockData(BlockSumLastBlock()).NxtBlkDiff;
   if ThisBlockTime > CONST_ExpectedBlockDuration*1.5 then
      begin
      Result := DecreaseDiff(LastDiff,2);
      exit;
      end;
   if ThisBlockTime < CONST_ExpectedBlockDuration*0.5 then
      begin
      Result := IncreaseDiff(LastDiff,2);
      exit;
      end;
   if ThisBlockTime > CONST_ExpectedBlockDuration*1.1 then
      begin
      Result := DecreaseDiff(LastDiff,1);
      exit;
      end;
   if ThisBlockTime < CONST_ExpectedBlockDuration*0.9 then
      begin
      Result := IncreaseDiff(LastDiff,1);
      exit;
      end;
   if ((ThisBlockTime<CONST_ExpectedBlockDuration*1.1) and (ThisBlockTime>CONST_ExpectedBlockDuration*0.9)) then
      begin
      Result := LastDiff;
      exit;
      end;
   if Last20Ave > CONST_ExpectedBlockDuration*1.1 then // HIGH, DECREASE DIFFICULT
      begin
      Result := DecreaseDiff(LastDiff,1);
      end
   else if Last20Ave < CONST_ExpectedBlockDuration*0.9 then // LOW, INCREASE DIFFICULT
      begin
      Result := IncreaseDiff(LastDiff,1);
      end
   else Result := LastDiff;
   end;
End;

// DECREASES THE MINER DIFF
function DecreaseDiff(minediff:String;variation:int64):String;
var
  Lettra, Numero : int64;
  Resultado : String = '';
Begin
Lettra := GetCharsFromMinerDiff(minediff);
Numero := GetStepsFromMinerDiff(minediff);
Numero := Numero-variation;
if Numero < 5 then
   begin
   Numero := 21+numero;
   Lettra := lettra-1;
   end;
Resultado := Resultado+chr(Lettra+96);
Resultado := Resultado+chr(numero+96);
Result := Resultado;
End;

// INCREASES THE MINER DIFF
function IncreaseDiff(minediff:String;variation:int64):String;
var
  Lettra, Numero : int64;
  Resultado : String = '';
Begin
Lettra := GetCharsFromMinerDiff(minediff);
Numero := GetStepsFromMinerDiff(minediff);
Numero := Numero+variation;
if Numero > 25 then
   begin
   Numero := 4+(numero-25);
   Lettra := lettra+1;
   end;
Resultado := Resultado+chr(Lettra+96);
Resultado := Resultado+chr(numero+96);
Result := Resultado;
End;

// RETURNS THE AVE DURATION OF THE LAST 20 BLOCKS
Function GetLast20Average(ThisBlockTime:int64):int64;
var
  contador : integer;
  Duration : int64 = 0;
  Divisor : int64;
Begin
if ThisBlockTime > 0 then Divisor := length(ArrBlockSummary) + 1
else Divisor := length(ArrBlockSummary);
for contador := 0 to length(ArrBlockSummary)-1 do
   begin
   Duration := Duration+StrToInt64(ArrBlockSummary[contador].TimeTot);
   end;
Result := (Duration+ThisBlockTime) div (divisor);
end;

// RETURNS THE MINING REWARD FOR A BLOCK
function GetBlockReward(BlNumber:int64):Int64;
Begin
if BlNumber = 0 then result := 142054400000
else if ((BlNumber > 0) and (blnumber <= 2016)) then result := 102400000
else if ((BlNumber > 2017) and (BlNumber <= 6048)) then result := 51200000
else if ((BlNumber > 6049) and (BlNumber <= 14112)) then result := 25600000
else if ((BlNumber > 14113) and (BlNumber <= 30240)) then result := 12800000
else if ((BlNumber > 30241) and (BlNumber <= 62496)) then result := 6400000
else if ((BlNumber > 62497) and (BlNumber <= 127008)) then result := 3200000
else if ((BlNumber > 127009) and (BlNumber <= 256032)) then result := 1600000
else if ((BlNumber > 256033) and (BlNumber <= 514080)) then result := 800000
else if ((BlNumber > 514081) and (BlNumber <= 1030176)) then result := 400000
else result := 0;
End;

// ADD FUNDS TO A SPECIFIED ADDRESS
Procedure AddFundsToAddress(Address:String;Amount:Int64;block:int64);
var
  contador : integer = 0;
  Dataread, DataWrite : AccountData;
Begin
assignfile (FilaAccData,CONST_ArchivoAccData);
reset(FilaAccData);
for contador := 0 to filesize(FilaAccData)-1 do
   begin
   seek(FilaAccData,contador);
   read(FilaAccData,Dataread);
   if DataRead.Hash = Address then
      begin
      Datawrite := Default(AccountData);
      Datawrite.Number := Dataread.Number;
      Datawrite.PublicKey:= Dataread.PublicKey;
      Datawrite.Hash:= DataRead.Hash;
      Datawrite.Balance:= IntToStr(StrToInt64(dataread.Balance)+amount);
      Datawrite.Lastop:= IntToStr(block);
      seek(FilaAccData,contador);
      write(FilaAccData,datawrite);
      end;
   end;
Closefile(FilaAccData);
End;

// SET ADDRESS PUBLIC KEY (REGISTER)
Procedure SetAddressPubKey(Address,PubKey:String;block:int64);
var
  contador : integer = 0;
  Dataread, DataWrite : AccountData;
Begin
assignfile (FilaAccData,CONST_ArchivoAccData);
reset(FilaAccData);
for contador := 0 to filesize(FilaAccData)-1 do
   begin
   seek(FilaAccData,contador);
   read(FilaAccData,Dataread);
   if DataRead.Hash = Address then
      begin
      Datawrite := Default(AccountData);
      Datawrite.Number := Dataread.Number;
      Datawrite.PublicKey:= PubKey;
      Datawrite.Hash:= DataRead.Hash;
      Datawrite.Balance:= DataRead.Balance;
      Datawrite.Lastop:= IntToStr(block);
      seek(FilaAccData,contador);
      write(FilaAccData,datawrite);
      Closefile(FilaAccData);
      Exit;
      end;
   end;
Closefile(FilaAccData);
End;

// RETURNS THE NEGATIVE VALUE OF AN int64
function GetNegativeValue(number:int64):int64;
Begin
if number > 0 then Result := number-(Number*2)
else Result := number;
End;

// CREATES THE BLOCK ZERO
Procedure CreateBlockZero();
Begin
BuildNewBlock(0,'1531896783344','KB589714930B2EAC5296BD2BB887A8AF9E9','NONE','','NONE','fe');
End;

// UNDONES THE LASTBLOCK
function UndoneLastBlock(BlNumber:int64 = -1):Boolean;
Begin
Result := False;
If not STATUS_Updated then
   begin
   OutPutText('Can Not Undone Last Block if not Updated',false);
   exit;
   end;
// RESTORE SAVED VALUES
CopyFile(CONST_ArchivoAccData+'.bak',CONST_ArchivoAccData,true);
CopyFile(CONST_ArchivoMyTxs+'.bak',CONST_ArchivoMyTxs,true);
SetMyLastUpdatedBlockTrxs(blnumber-1);
ArrBlockSummary := copy(LASTBLOCK_ArrBlockSum);
ArrayMyAddresses := copy(LASTBLOCK_ArrMyAddresses);
LASTBLOCK_PendingTxs.AddStrings(PendingTxs);
LASTBLOCK_PendingTxs.AddStrings(PendingTxs);
PendingTxs.Clear;
PendingTxs.AddStrings(LASTBLOCK_PendingTxs);
Deletefile(CONST_DIRBLOCKS+IntToStr(BlNumber)+'.blk');

LASTBLOCK_UNDONE := BlNumber;
OutPutText('BLOCK '+IntToStr(BlNumber)+' Undone');
Result := True;
End;

{*******************************************************************************
                              COMMAND LINE PARSER
*******************************************************************************}

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
else if UpperCase(CommandUsed) = 'NETTIME' then OutputText(TimestampToDate(getSNTPStringTime()))
else if UpperCase(CommandUsed) = 'CLS' then ClearMemoLines()
else if UpperCase(CommandUsed) = 'ACCOUNTS' then showaccounts()
else if UpperCase(CommandUsed) = 'NEWADDRESS' then CreateNewAddress()
else if UpperCase(CommandUsed) = 'BLOCKSUM' then ShowBlockSum()
else if UpperCase(CommandUsed) = 'SENDTO' then SendFunds(linetext)
else if UpperCase(CommandUsed) = 'LBNUM' then OutputText('Last Block on BlockSum: '+IntToStr(BlockSumLastBlock()))
else if UpperCase(CommandUsed) = 'GETDIFF' then GetCurrCiff()
else if UpperCase(CommandUsed) = 'RECON' then ReconnectOn()
else if UpperCase(CommandUsed) = 'RECOFF' then ReconnectOff()
else if UpperCase(CommandUsed) = 'SETPORT' then SetPort(linetext)
else if UpperCase(CommandUsed) = 'OPENSSL' then RunOpenSSLCommand(linetext)
else if UpperCase(CommandUsed) = 'SHA256' then ShowSha256String(linetext)
else if UpperCase(CommandUsed) = 'CHECKADDRESS' then CheckAddress(linetext)
else if UpperCase(CommandUsed) = 'MINERINFO' then Minerinfo()
else if UpperCase(CommandUsed) = 'CPUCOUNT' then OutputText('CPUs: '+IntToStr(MAIN_CPUCOUNT))
else if UpperCase(CommandUsed) = 'CPUMINE' then SetCPUMiners(linetext)
else if UpperCase(CommandUsed) = 'MINTOTASK' then MinToTask(linetext)
else if UpperCase(CommandUsed) = 'AUTOCONN' then AutoConn(linetext)
else if UpperCase(CommandUsed) = 'FULLNODE' then FullNode(linetext)
else if UpperCase(CommandUsed) = 'UPDATENODES' then UpdateNodes(linetext)
else if UpperCase(CommandUsed) = 'SHOWMINNED' then ShowMinned(linetext)
else if UpperCase(CommandUsed) = 'LBSTARTIME' then OutputText(TimestampToDate(GetBlockData(BlockSumLastBlock()).TimeStart))
else OutPutText('Unknown command: '+CommandUsed,false);
end;

// GETS THE REQUIRED FUNCTION ON THE COMMANDLINE
Function GetCommandLineCommand(LineText:String):String;
var
  Temp : String = '';
  ThisChar : Char;
  Contador : int64 = 1;
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
Function GetParameterFromCommandLine(LineText:String;ParamNumber:int64):String;
var
  Temp : String = '';
  ThisChar : Char;
  Contador : int64 = 1;
  WhiteSpaces : int64 = 0;
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
  contador:int64=0;
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
  contador:int64=0;
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
  NodeNumber : int64;
Begin
if not IsValidInt(GetParameterFromCommandLine(linetext,1)) then
   begin
   OutPutText('Invalid Parameter: '+GetParameterFromCommandLine(linetext,1),false);
   exit;
   end;
NodeNumber := StrToInt64(GetParameterFromCommandLine(linetext,1));
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
  NodeNumber : int64;
Begin
if not IsValidInt(GetParameterFromCommandLine(linetext,1)) then
   begin
   OutPutText('Invalid Parameter: '+GetParameterFromCommandLine(linetext,1),false);
   exit;
   end;
NodeNumber := StrToInt64(GetParameterFromCommandLine(linetext,1));
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
  contador : int64 = 0;
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
   outputtext(IntToStr(contador)+' - '+copy(DataRead.Hash,1,8)+' - '+Int2CurrencyStr(StrToInt64(Dataread.Balance))+' '+Registered+' '+Dataread.Lastop,false);
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
  Monto, comision, MontoMasComision, Restante : int64;
  Contador : int64;
  MontoFromAddress : int64;
  Concepto : String;
Begin
if not STATUS_Updated then
   begin
   Outputtext('You need connect and update to the network',false);
   ShowAlert('Your wallet is not updated');
   exit;
   end;
Destination := GetParameterFromCommandLine(linetext,1);
Amount := GetParameterFromCommandLine(linetext,2);
Concepto := GetParameterFromCommandLine(linetext,3);
if IsValidInt(Destination) then
   begin
   Destination := GetAddressFromAccountNumber(StrToInt64(Destination));
   if StrToInt64(GetAccountNumberFromAddress(Destination)) < 0 then
      begin
      Outputtext('Account number do not exists',false);
      ShowAlert('Account number do not exists');
      exit;
      end;
   end;
if ((Destination = '') or (not IsValidAddress(Destination))) then
   begin
   Outputtext('Invalid Destination: '+destination,false);
   ShowAlert('Invalid Destination '+destination);
   exit;
   end;
if IsAddressMine(Destination)>=0 then
   begin
   Outputtext('Can not send to your addresses',false);
   ShowAlert('Can not send to your addresses');
   exit;
   end;
if IsValidFLoat(Amount) then
   begin
   Monto := Round(StrToFloat(Amount)*100);
   end
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
   if StrToInt64(ArrayMyAddresses[contador].Balance) > 0 then
      begin
      MontoFromAddress := SendFundsFromAddress(Destination, contador, Restante, concepto);
      Restante := Restante - MontoFromAddress;
      end;
   contador := contador - 1;
   end;
ShowAlert('Transfer Sucessfull'+SLINEBREAK+
          'Sent   : '+Int2CurrencyStr(Monto)+SLINEBREAK+
          'Fee    : '+Int2CurrencyStr(comision)+SLINEBREAK+
          'Total  : '+Int2CurrencyStr(MontoMasComision)+SLINEBREAK+
          'To     : '+Destination+SLINEBREAK+
          'Concept: '+Concepto);
EditSFDesti.Text := '';EditSFAmount.Text:='';EditConcept.Text:='';
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
   ShowMensaje(false);
   exit;
   end;
PortNumber := GetParameterFromCommandLine(linetext,1);
if StrToInt(PortNumber) < 1 then
   begin
   OutPutText('ERROR: Invalid Parameter: '+GetParameterFromCommandLine(linetext,1),false);
   EditUserPort.Text := OptionsData.ListeningPort;
   ShowMensaje(false);
   exit;
   end;
OptionsData.ListeningPort:=PortNumber;
OutPutText('New Listening port set'+SLineBreak+'Change will be effective on next connection',false);
U_SaveOptions := true;
ShowMensaje(true);
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

// SET THE NUMBER OF MINING CPUS
Procedure SetCPUMiners(linetext:String);
var
  CPUsNumber : string;
Begin
if not IsValidInt(GetParameterFromCommandLine(linetext,1)) then
   begin
   OutPutText('ERROR: Invalid Parameter: '+GetParameterFromCommandLine(linetext,1),false);
   ShowMensaje(false);
   exit;
   end;
CPUsNumber := GetParameterFromCommandLine(linetext,1);
if ((StrToInt64(CPUsNumber) < 1) or (StrToInt64(CPUsNumber)>MAIN_CPUCOUNT)) then
   begin
   OutPutText('ERROR: Mining CPUs range: 1 to '+IntToStr(MAIN_CPUCOUNT),false);
   ShowMensaje(false);
   exit;
   end;
OutPutText('Mining CPUs set to: '+CPUsNumber,false);
OptionsData.CPUmining := StrToInt64(CPUsNumber);
U_SaveOptions := true;
ShowMensaje(true);
CloseAllMiningThreads();
Miner_IsMineron := false;
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
   CheckBoxMin.Checked:=true;
   ShowMensaje(true);
   end
else if UpperCase(Option)='OFF' then
   begin
   OptionsData.MinimToTray:=false;
   U_SaveOptions := true;
   CheckBoxMin.Checked:=false;
   ShowMensaje(true);
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
   CheckBoxAutoConn.Checked:=true;
   ShowMensaje(true);
   end
else if UpperCase(Option)='OFF' then
   begin
   OptionsData.AutoConnect:=false;
   U_SaveOptions := true;
   CheckBoxAutoConn.Checked:=false;
   ShowMensaje(true);
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
   CheckBoxFullNode.Checked:=true;
   ShowMensaje(true);
   end
else if UpperCase(Option)='OFF' then
   begin
   OptionsData.FullNode:=false;
   U_SaveOptions := true;
   CheckBoxFullNode.Checked:=false;
   ShowMensaje(true);
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
   CheckBoxUpdateNodes.Checked:=true;
   ShowMensaje(true);
   end
else if UpperCase(Option)='OFF' then
   begin
   OptionsData.GetNodes:=false;
   U_SaveOptions := true;
   CheckBoxUpdateNodes.Checked:=false;
   ShowMensaje(true);
   end
else OutputText('Invalid Parameter. Use ON of OFF',false);
End;

// SET SHOW MINNED TRXS
Procedure ShowMinned(linetext:String);
var
  Option : string;
Begin
Option := GetParameterFromCommandLine(linetext,1);
if UpperCase(Option)='ON' then
   begin
   OptionsData.ShowMinned:=true;
   U_SaveOptions := true;
   CheckBoxMinned.Checked := true;
   ShowMensaje(true);
   UpdateUserTrxs();
   end
else if UpperCase(Option)='OFF' then
   begin
   OptionsData.ShowMinned:=false;
   U_SaveOptions := true;
   CheckBoxMinned.Checked := false;
   ShowMensaje(true);
   UpdateUserTrxs();
   end
else OutputText('Invalid Parameter. Use ON of OFF',false);
End;

{*******************************************************************************
                                       CRYPTO
*******************************************************************************}

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

{*******************************************************************************
                                      PROTOCOL
*******************************************************************************}

// RETURNS IF A STRING IS A VALID PROTOCOL MESSAGE
function IsValidProtocol(line:String):Boolean;
Var
  Start, Finish:String;
Begin
Start := copy(line,1,4);finish:= copy(line,length(line)-3,4);
if ((start='{MIC') and (finish='END}')) then result := true
else result := false;
End;

// RETURNS THE STRING FOR JOIN MESSAGES
function JoinString():String;
var
  PortData : String;
Begin
if Form1.IdTCPServer1.Active=false then PortData := '-'+OptionsData.ListeningPort
else PortData := OptionsData.ListeningPort;
result :='{MIC JOIN '+
   IntTostr(GetTotalConex)+' '+
   GetTimeStamp()+' '+
   IntToStr(LOCAL_MyLastBlock)+' '+
   LOCAL_LastBlockHash+' '+
   IntToStr(LOCAL_MyLastAccount)+' '+
   IntToStr(PendingTXs.Count)+' '+
   PortData+' '+
   LOCAL_MyAccsumHash+' '+
   MAIN_Version+
   ' END}'
End;

// PARSES A PROTOCOL LINE
Procedure ParseProtocolConnection(Lines:TStringList;Slot:int64);
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
Procedure PTC_Join(TextLine:String;slot:int64;Answer:boolean);
var
  conections,block,lastblockhash,account,pending,port,accsumhash,version:String;
  peertime : string;
  Listening : boolean;
Begin
STATUS_IncomingPings := STATUS_IncomingPings+1;
conections := GetParameterFromCommandLine(TextLine,1);
peertime := GetParameterFromCommandLine(TextLine,2);
block := GetParameterFromCommandLine(TextLine,3);
lastblockhash := GetParameterFromCommandLine(TextLine,4);
account := GetParameterFromCommandLine(TextLine,5);
pending := GetParameterFromCommandLine(TextLine,6);
port := GetParameterFromCommandLine(TextLine,7);
if AnsiContainsStr(Port,'-') then listening := false else listening := true;
port := StringReplace(port,'-','',[rfReplaceAll, rfIgnoreCase]);
accsumhash := GetParameterFromCommandLine(TextLine,8);
version := GetParameterFromCommandLine(TextLine,9);
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
conexiones[slot].Version:=version;
conexiones[slot].Listening:=listening;
conexiones[slot].offset:= IntToStr(abs(StrToInt64(peertime)-StrToInt64(GetTimeStamp())));
if not NodeExists(conexiones[slot].ip,conexiones[slot].ListenPort) then
   AddNewNode(conexiones[slot].ip,conexiones[slot].ListenPort);
if Answer then
   Begin
   SendPTCMessage(slot,StringReplace(JoinString(),'JOIN','JOIR',[rfReplaceAll, rfIgnoreCase]));
   end;
STATUS_LastPing := StrToInt64(copy(conexiones[slot].lastping,1,10));
end;

// SEND A PROTOCOL MESSAGE TO A SPECIFIC SLOT
Procedure SendPTCMessage(Slot:int64;Message:String);
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
procedure PTC_ServerClose(slot:int64);
Begin
ReadsFromSlots[slot].Clear;
ConexionesCliente[Slot].Disconnect;
OutputText('Outgoing connection closed: '+conexiones[slot].ip+' -> Server disconnected');
ClearConection('server',conexiones[Slot].ip);
End;

// SEND ALL OUTGOING MESSAGES TO ALL AVAILABLE PEERS
Procedure SendOutGoingMessages();
Var
  Slot :integer = 1;
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
Procedure PTC_ACRE(Textline:String;slot:int64);
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
  contador : int64 = 0;
  Timevalue, TimePending : Int64;
  resultado : int64 = 0;
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
      if StrToInt64(timestamp) >= StrToInt64(GetBlockData(StrToInt64(blockNumber)).TimeEnd) then
         begin
         OutputText('Block '+(blockNumber)+' older received: Omitted');
         exit;
         end
      else if StrToInt64(timestamp) < StrToInt64(GetBlockData(StrToInt64(blockNumber)).TimeEnd) then // the new is the good one
         begin
         OutputText('*************************************');
         OutputText('Better block '+blockNumber+' received');
         OutputText('*************************************');
         UndoneLastBlock(StrToInt64(blockNumber));
         BuildNewBlock(StrToInt64(blockNumber),TimeStamp,Account,Solution,NewBlHash,TargetHash,Difficulty);
         exit;
         end;
      end;
   end;
if StrToInt64(blockNumber) = BlockSumLastBlock()+1 then
   begin
   if VerifyMinerResult(Solution,Difficulty,TargetHash,StrToInt64(blockNumber)) > 0 then
      begin
      outputtext('Wrong Solution Block '+blockNumber+' : '+solution+' for '+MINER_TargetHash);
      DoIt := false;
      end;
   if DoIT then
      begin
      OutputText('Solution Ok for block: '+blockNumber+ '. Building block');
      BuildNewBlock(StrToInt64(blockNumber),TimeStamp,Account,Solution,NewBlHash,TargetHash,Difficulty);
      end;
   end;
End;

// RETURNS IF A TRANSACTION IS ALREADY PENDING / PARAMNUM IS +1 SINCE COMMAND=1
function TranxAlreadyPending(TypeOfTrx, ValTrx:String;ParamNum:int64):boolean;
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
procedure PTC_SendPending(Slot:int64);
var
  contador : integer;
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
procedure SendAccDataFile(slot:int64);
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
procedure PTC_TRFR(Textline:string;slot:int64);
var
  TimeStamp, Sender, Destination, Monto, SigHash, OpHash, concepto : String;
  Proceder : Boolean = true;
Begin
TimeStamp := GetParameterFromCommandLine(Textline,1);
Sender := GetParameterFromCommandLine(Textline,2);
Destination := GetParameterFromCommandLine(Textline,3);
Monto := GetParameterFromCommandLine(Textline,4);
SigHash := GetParameterFromCommandLine(Textline,5);
OpHash := GetParameterFromCommandLine(Textline,6);
concepto := GetParameterFromCommandLine(Textline,7);
if GetAddressBalanceFromDisk(Sender)-GetAddressPaymentsOnPending(sender) < StrToInt64(Monto) then Proceder := false;
if TranxAlreadyPending('TRFR',OpHash,7) then Proceder := false;
if StrToInt64(TimeStamp) < StrToInt64(GetBlockData(BlockSumLastBlock()).TimeStart) then exit;
if TrxExistsInLastBlock(OpHash) then exit;
if not VerifySignedString(TimeStamp+Sender+Destination+Monto,SigHash,GetAddressPubKey(Sender)) then exit;
if proceder then
   begin
   Textline := '{MIC '+Textline+' END}';
   AddPendingByTimestamp(Textline); // ORDERED BY TIMESTAMP
   OutGoingMessages.Add(Textline);
   end;
End;

// RETURNS IF A TRXID WAS ADDED IN THE LAST BLOCK
function TrxExistsInLastBlock(hash:String):boolean;
var
  counter : integer;
Begin
result := false;
for counter := 0 to LASTBLOCK_TrxsIDs.Count-1 do
   begin
   if LASTBLOCK_TrxsIDs[counter] = hash then
      begin
      result := true;
      exit;
      end;
   end;
end;

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
function SendFundsFromAddress(Destination:string; AddressIndex,Restante:int64;concepto:string):int64;
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
Concepto+' '+
'END}');
Result := MontoFinal;
End;

// RETURNS THE TOTAL PAYMENTS PENDING FOR AN ADDRESS
function GetAddressPaymentsOnPending(Address:String):Int64;
var
  contador : integer;
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
Procedure PTC_SendZipedBlocks(textline:string;slot:int64);
var
  FirstBlock, LastBlock : int64;
  MyZipFile: TZipper;
  contador : integer;
  AFileStream : TFileStream;
begin
if not IsValidInt(GetParameterFromCommandLine(textline,1)) then
   begin
   SendPTCMessage(slot,'{MIC INVALIDBLOCKREQUEST END}');
   exit;
   end;
FirstBlock := StrToInt64(GetParameterFromCommandLine(textline,1))+1;
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
function GetCharsFromMinerDiff(minerdiff:string):int64;
var
  Lettra : Char;
Begin
Lettra := minerdiff[1];
Result := Ord(Lettra)-96;
End;

// OBTAINS THE NUMBER OF STEPS FROM A DIFFICUL STRING
Function GetStepsFromMinerDiff(minerdiff:string):int64;
var
  Lettra : Char;
Begin
Lettra := minerdiff[2];
Result := Ord(Lettra)-96;
End;

// RETURNS THE STRING WITH THE FIRST 50 NODES
function GetNodesString():string;
var
  NodesString : String = '';
  NodesAdded : integer = 0;
  Counter : integer;
Begin
for counter := 0 to length(ArrayNodos)-1 do
   begin
   NodesString := NodesString+' '+ArrayNodos[counter].ip+':'+ArrayNodos[counter].port+':';
   NodesAdded := NodesAdded+1;
   if NodesAdded>50 then break;
   end;
NodesString := '{MIC NODES'+NodesString+' END}';
result := NodesString;
End;

// SEND THE NODES TO PEER
procedure PTC_GetNodes(Textline:string;slot:int64);
var
  NodesString : String = '';
Begin
NodesString := GetNodesString();
SendPTCMessage(slot,NodesString);
End;

// SAVE NODES RECEIVED FROM PEER
procedure PTC_SaveNodes(Textline:string;slot:int64);
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
   if thisnode.ip = '127.0.0.1' then continue;
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
procedure PTC_InBlRe(Textline:string;slot:int64);
Begin
if LOCAL_MyLastBlock < STATUS_LastBlockRequested then
   STATUS_LastBlockRequested := LOCAL_MyLastBlock;
End;

END. // END UNIT

