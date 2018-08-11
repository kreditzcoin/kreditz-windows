unit MC_Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Menus, ComCtrls, grids, IdTCPServer, IdContext, IdTCPClient, IdTime,
  master, LCLType, CommandLineParser, CM_Crypto, Protocol, strutils,
  Blocks, Clipbrd, dateutils,
  IdComponent, IdTCPConnection, IdBaseComponent, IdGlobal, IdCustomTCPServer;

type

  { TForm1 }

  TForm1 = class(TForm)
    IdTCPServer1: TIdTCPServer;
    ImageList1: TImageList;
    TimerStart: TTimer;
    TimerLoop : TTimer;
    TimerMensaje : TTimer;
    CommandLine : TEdit;
    MainMemo : TMemo;
    SystrayIcon: TTrayIcon;

    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure IdTCPServer1Exception(AContext: TIdContext; AException: Exception);
    procedure TimerLoopTimer(Sender: TObject);
    procedure TimerStartTimer(Sender: TObject);
    procedure TimerMensajeTimer(Sender: TObject);
    procedure CommandLineKeyup(Sender: TObject; var Key: Word; Shift: TShiftState);
    Procedure EditSFAmountOnChange(Sender: TObject);
    procedure IdTCPServer1Connect(AContext: TIdContext);
    procedure IdTCPServer1Disconnect(AContext: TIdContext);
    procedure ReadClientLines(Number, Slot:int64);
    procedure IdTCPServer1Execute(AContext: TIdContext);
    procedure InitializeForm1();
    procedure ButtonConsolaOnClick(Sender: TObject);
    procedure ButtonNodesOnClick(Sender: TObject);
    procedure ButtonWalletOnClick(Sender: TObject);
    procedure ButtonDeleteNodeOnClick(Sender: TObject);
    procedure ButtonAddnodeOnClick(Sender: TObject);
    Procedure ButAddNodeCancelOnClick(Sender: TObject);
    Procedure ButAddNodeAddOnClick(Sender: TObject);
    procedure ButtonDeleteBLOnClick(Sender: TObject);
    procedure ButtonNewAddressOnClick(Sender: TObject);
    procedure ButtonCopyAddressOnClick(Sender: TObject);
    Procedure ButtonPasteDestOnClick(Sender: TObject);

    Procedure ButtonMaxAmoOnClick(Sender: TObject);
    Procedure ButtonSendFundsOnClick(Sender: TObject);
    Procedure ButtonDetailsOnClick(Sender: TObject);
    Procedure CheckBoxConnectOnChange(Sender: TObject);
    Procedure CheckBoxMinerOnChange(Sender: TObject);
    procedure GridAddressesPrepareCanvas(sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
    Procedure GridPendingPrepareCanvas(sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
    Procedure GrisUserTrxsPrepareCanvas(sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
    Procedure GridConxsPrepareCanvas(sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
    Procedure DoubleClickSysTray(Sender: TObject);
    Procedure EditUserPortOnEditingDone(Sender: TObject);
    Procedure ComboCPUMinerOnChange(Sender: TObject);
    Procedure CheckBoxMinOnChange(Sender: TObject);
    Procedure CheckBoxAutoConnOnChange(Sender: TObject);
    Procedure CheckBoxFullNodeOnChange(Sender: TObject);
    Procedure CheckBoxUpdateNodesOnChange(Sender: TObject);
    Procedure CheckBoxMinnedOnChange(Sender: TObject);

    private
    Procedure ButtonCloseNotifyOnClick(Sender: TObject);
    public
  end;

  Procedure ShowAlert(Message:String);

var
  Form1: TForm1;  // MUST BE REMOVED
  ConexionesCliente : array [1..CONST_MaxOutgoingConections] of TIdTCPClient;

implementation

{$R *.lfm}

{ TForm1 }

// FORM SHOW
procedure TForm1.FormShow(Sender: TObject);
begin
if MAIN_FirstFormShow then
   begin
   // DETECT CPU CORES
   if GetEnvironmentVariable('NUMBER_OF_PROCESSORS') = '' then
      MAIN_CPUCOUNT := 1
   else MAIN_CPUCOUNT := StrToInt(GetEnvironmentVariable('NUMBER_OF_PROCESSORS'));
   Initializeform1();
   MainMemo.Lines.Add('Kreditz By PJOR, 2018');
   MainMemo.Lines.Add('Version: '+MAIN_Version);
   MainMemo.Lines.Add('-----------------------------------------------------------------------------------');
   Application.ProcessMessages;
   TimerStart.Enabled:=true;
   MAIN_FirstFormShow := false;
   end;
end;

// HIDE FORM1 ON TASK BAR WHEN MINIMIZE
procedure TForm1.FormWindowStateChange(Sender: TObject);
begin
if OptionsData.MinimToTray then
   begin
   if Form1.WindowState = wsMinimized then
      begin
      SysTrayIcon.visible:=true;
      form1.hide;
      MAIN_MinOnTray:= true;
      end;
   end;
end;

// EXCEPTION ON SERVER
procedure TForm1.IdTCPServer1Exception(AContext: TIdContext;AException: Exception);
begin
OutputText('Server Excepcion: '+AException.Message);
end;

// TIMER TO HIDE MESSAGE PANEL
procedure TForm1.TimerMensajeTimer(Sender: TObject);
Begin
TimerMensaje.Enabled:=false;
PanelMensaje.Visible:=false;
End;

// EXECUTES TIMER LOOP
procedure TForm1.TimerLoopTimer(Sender: TObject);
var
  contador : integer;
Begin
TimerLoop.Enabled:=false;
{updates the UI}
UpdateLabels();
UpdatePanelServer();
UpdateGridConxs();
UpdatePanelMiner();
While MainMemoStrings.Count > 0 do
   begin
   mainmemo.Lines.Add(MainMemoStrings[0]);
   MainMemoStrings.Delete(0);
   end;
{save to disk updated files}
if U_SaveNodeFile then SaveNodesToDisk();
if U_SaveBlacklist then SaveBLNodesToDisk();
if U_SaveWallet then SaveAddressesToDisk();
if U_SaveOptions then SaveOptionsToDisk();
  if U_MylastBlock then GetMyLastUpdatedBlock();
  if U_MyLastAccount then  GetLastAccountUpdated();
  if U_MyLastBLockHash then GetMyLastBlockHash(IntToStr(LOCAL_MyLastBlock));
  if U_MyAccsumHash then GetMyAccSumHash();
  if U_MyBalance then
     begin
     MAIN_AccountBalance := GetTotalAccountBalance();
     UpdateMyAddresses();
     U_MyBalance := false;
     end;
  if U_MyAccountNumber then
     begin
     MAIN_AccountNumber := GetAccountNumberFromAddress(ArrayMyAddresses[0].Hash);
     U_MyAccountNumber := false;
     end;
if U_RebuildMyTxs then
   begin
   BuiltMyTrxs();
   end;
if NETWORK_SendPing then
   begin
   OutGoingMessages.Add(JoinString());
   NETWORK_SendPing := false;
   end;
{Process the command lines}
While ProcessLines.Count > 0 do
   begin
   ParseCommandLine(ProcessLines[0]);
   if ProcessLines.Count>0 then ProcessLines.Delete(0);
   end;
{detect if we are connected or disconnected}
if ((GetTotalConex >= CONST_MinimunConnectionsToWork) and (not STATUS_Connected)) then
   begin
   OutPutText('CONNECTED');
   STATUS_Connected := true;
   UpdatePanelStatus();
   end
else if ((GetTotalConex >= CONST_MinimunConnectionsToWork) and (STATUS_Connected)) then
   begin
   UpdateNetworkLastBlockData(); // SET NETWORK_LastBLock and NETWORK_LastBlockHash
   UpdateNetworkAccountSumData(); // SET NETWORK_LastAccount and NETWORK_AccsumHash
   UpdateNetworkPendingData(); // SET NETWORK_Pending
   UpdatePanelStatus();
   end
else if ((GetTotalConex < CONST_MinimunConnectionsToWork) and (STATUS_Connected)) then
   Begin
   OutPutText('DISCONNECTED');
   STATUS_Connected := false;
   STATUS_Synzed := false;
   STATUS_Updated := false;
   STATUS_IncomingPings := 0;
   PendingTXs.Clear;
   NETWORK_PENDING := 0;
   NETWORK_LastBLock := 0;
   NETWORK_LastBlockHash := '';
   NETWORK_LastAccount := 0;
   NETWORK_AccsumHash := '';
   MINER_IsMinerOn := false;
   CloseAllMiningThreads();
   UpdateMyPendingTxs();
   UpdatePanelStatus();
   end;
{detect if we are synzed or not}
if ((STATUS_IncomingPings >= CONST_PinsToStart) and (not STATUS_Synzed))then
   begin
   Outputtext('SYNCHRONIZED');
   STATUS_Synzed := true;
   UpdatePanelStatus();
   end;
if STATUS_Synzed then
   begin
   SendOutGoingMessages();
   if LOCAL_MyLastBlock < NETWORK_LastBLock then
      begin
      if STATUS_LastBlockRequested < LOCAL_MyLastBlock+1 then
         begin
         if OptionsData.FullNode = true then contador := LOCAL_MyLastBlock
         else contador := NETWORK_LastBLock - 20;
         if contador < LOCAL_MyLastBlock then contador := LOCAL_MyLastBlock;
         SendPTCMessage(UpdateNetworkLastBlockData(),'{MIC LASTBLOCK '+IntToStr(contador)+' END}');
         STATUS_LastBlockRequested := LOCAL_MyLastBlock+1;
         end;
      end;
   if LOCAL_MyAccsumHash <> NETWORK_AccsumHash then
      begin
      if ((not STATUS_LastAccountRequested) and (not STATUS_Updated) and (LOCAL_MyLastBlock<NETWORK_LastBLock))  then
         begin
         SendPTCMessage(UpdateNetworkAccountSumData(),'{MIC LASTACC '+IntToStr(LOCAL_MyLastAccount)+' END}');
         STATUS_LastAccountRequested := true;
         OutputText('Requesting accs data');
         end;
      end;
   end;
if ((STATUS_Synzed) and (not STATUS_Updated) and (LOCAL_MylastBlock = NETWORK_LastBLock) and (LOCAL_MyAccsumHash=NETWORK_AccsumHash)) then
   begin
   Outputtext('UPDATED');
   STATUS_Updated := true;
   UpdatePanelStatus();
   LoadWalletData();
   BuildBlockSum();
   CheckIfMyAddressesNeedsRegisterFromDisk();
   end;
if STATUS_Updated then
   begin
   if NETWORK_PENDING > PendingTXs.Count then SendPTCMessage(UpdateNetworkPendingData(),'{MIC GETPENDING END}');
   if MINER_BlockDiffSet <> BlockSumLastBlock()+1 then
      begin
      VerifyUserAddressesForAcre();
      CloseAllMiningThreads();
      MINER_IsMinerOn := false;
      MINER_BlockDiffSet := BlockSumLastBlock()+1;
      MINER_MineDiff := GetBlockData(BlockSumLastBlock()).NxtBlkDiff;
        MINER_TargetChars := GetCharsFromMinerDiff(MINER_MineDiff);
        MINER_Steps := GetStepsFromMinerDiff(MINER_MineDiff);
      MINER_TargetHash := copy(LOCAL_LastBlockHash,1,MINER_TargetChars); // FOR THE MINER
        MINER_CurrStepTarget := MINER_TargetHash;
        MINER_FoundedSteps := 0;
      MINER_ResultHash :='';
      MINER_HashCounter := 10000000;
      MINER_HashSeed := GetHashSeed(MINER_TargetChars);
      end;
   end;
// MINER VERIFICATION
if ((OptionsData.Mining) and (not MINER_IsMinerOn) and (STATUS_Updated) and
  (not MINER_BlockFound) and (LASTBLOCK_Duration > 5)
  and (MINER_TargetHash = copy(NETWORK_LastBlockHash,1,MINER_TargetChars))) then
   begin
   OutputText('Mining. Target: '+MINER_TargetHash+' (Block: '+IntToStr(NETWORK_LastBLock+1)+') with '+IntToStr(OptionsData.CPUmining)+' CPUs');
   while Length(ArrayThreads) < OptionsData.CPUmining do
      begin
      MINER_IsMinerOn := true;
      SetLength(ArrayThreads,Length(ArrayThreads)+1);
      ArrayThreads[Length(ArrayThreads)-1] := Beginthread(tthreadfunc(@RunMiner));
      end;
   end
else if MINER_BlockFound then // BLOCK FOUND, BUILD BLOCK
   begin
   if LASTBLOCK_Duration > 5 then
      begin
      if VerifyMinerResult(MINER_ResultHash,MINER_MineDiff,MINER_TargetHash,MINER_BlockDiffSet) > 0 then
         begin
         OutputText('FALSE POSITIVE for '+MINER_TargetHash+' : '+MINER_ResultHash);
         MINER_IsMinerOn := false;
         MINER_BlockFound := false;
         MINER_CurrStepTarget := MINER_TargetHash;
         MINER_FoundedSteps := 0;
         MINER_ResultHash :='';
         MINER_HashCounter := 10000000;
         MINER_HashSeed := GetHashSeed(MINER_TargetChars);
         end
      else
         begin
         OutputText('Block Found '+IntToStr(NETWORK_LastBLock+1)+': '+MINER_TargetHash+' -> '+MINER_ResultHash);
         BuildNewBlock(MINER_BlockDiffSet,GetTimestamp(),MAIN_AccountHash,'','','',MINER_MineDiff);
         MINER_IsMinerOn := false;
         MINER_BlockFound := false;
         MINER_CurrStepTarget := MINER_TargetHash;
         MINER_FoundedSteps := 0;
         MINER_ResultHash :='';
         MINER_HashCounter := 10000000;
         MINER_HashSeed := GetHashSeed(MINER_TargetChars);
         end;
      end
   else // Less than 5 second from last block
      begin
      OutputText('Too early to post a block');
      MINER_IsMinerOn := false;
      MINER_BlockFound := false;
      OptionsData.Mining := true;
      end;
   end;
// RUN PERIODICALLY A CONNECTION TO SERVERS
if strtoint64(GetTimeStamp()) > CONNECT_LastTime+5000 then
   begin
   CONNECT_LastNode := CONNECT_LastNode+1;
   if CONNECT_LastNode > length(ArrayNodos)-1 then CONNECT_LastNode := 0;
   if ((length(ArrayNodos) > 0)  and (CheckBoxConnect.Checked) and (Optionsdata.Reconnect)) then
      begin
      TryConnectToNode(CONNECT_LastNode);
      end;
   CONNECT_LastTime := strtoint64(GetTimeStamp);
   end;
// CHECK ALL CONNECTIONS
for contador := 1 to CONST_MAXConections do
   begin
   if conexiones[contador].tipo='server' then
      begin
      ReadClientLines(strtoint64(conexiones[contador].ClientConn),contador);
      if strtoint64(GetTimeStamp()) > strtoint64(conexiones[contador].lastping)+5000 then
         begin
         SendPTCMessage(contador,JoinString());
         end;
      end;
   if conexiones[contador].tipo <> '' then
      begin
      {close inactive connections after x time}
      if ((strtoint64(GetTimeStamp()) > strtoint64(conexiones[contador].lastping)+15000)) then
         begin
         Outputtext('Conection closed: '+conexiones[contador].ip+' -> Time Out Auth');
         CloseConnectionSlot(contador);
         end;
      end;
   if ReadsFromSlots[contador].count > 0 then ParseProtocolConnection(ReadsFromSlots[contador],contador);
   end;
TimerLoop.Enabled:=true;
end;

// WHEN PROGRAM STARTS
procedure TForm1.TimerStartTimer(Sender: TObject);
begin
Randomize;
TimerStart.Enabled:=false;
verifyfiles();
GetMyLastUpdatedBlock(); // Set LOCAL_MyLastBlock
GetLastAccountUpdated(); // Set LOCAL_MyLastAccount
GetMyLastBLockHash(IntToStr(LOCAL_MyLastBlock));    // Set LOCAL_LastBlockHash
GetMyAccSumHash();      // Set LOCAL_MyAccsumHash
LoadNodesFromDisk();
LoadBLNodesFromDisk();
UpdateLabels();
UpdateMyAddresses();
if OptionsData.AutoConnect then CheckBoxConnect.Checked:=true;
BuiltMyTrxs();
UpdateUserTrxs();
MAIN_ProgramStarted := true;
TimerLoop.Enabled:=true;
end;

// SERVER: GET LINE
procedure TForm1.IdTCPServer1Execute(AContext: TIdContext);
var
  LLine : String;
  IPUser : String;
  AFileStream : TFileStream;
  BlockZipName : String;
begin
IPUser := AContext.Connection.Socket.Binding.PeerIP;
LLine := AContext.Connection.IOHandler.ReadLn(IndyTextEncoding_UTF8);
if GetSlotFromIP(IPUser) = 0 then exit;
if LLine = 'FILEACCSUM' then
   begin
   if FileExists(CONST_ArchivoAccData) then DeleteFile(CONST_ArchivoAccData);
   AFileStream := TFileStream.Create(CONST_ArchivoAccData, fmCreate);
   AContext.Connection.IOHandler.ReadStream(AFileStream);
   AFileStream.Free;
   U_MyLastAccount := true;
   U_MyAccsumHash := true;
   U_MyBalance := true;
   STATUS_LastAccountRequested := false;
   end
else if LLine = 'BLOCKZIP' then
   begin
   BlockZipName := CONST_DirBlocks+'blocks.zip';
   if FileExists(BlockZipName) then DeleteFile(BlockZipName);
   AFileStream := TFileStream.Create(BlockZipName, fmCreate);
   AContext.Connection.IOHandler.ReadStream(AFileStream);
   AFileStream.Free;
   U_MyLastBlock := true;
   U_MyLastBLockHash := true;
   UnzipBlockFile(CONST_DirBlocks+'blocks.zip');
   BuildBlockSum();
   U_RebuildMyTxs := true;
   end
else
   try
   ReadsFromSlots[GetSlotFromIP(IPUser)].Add(LLine);
   Except
   On E :Exception do outputtext('Error receiving line: '+LLine);
   end;
end;

// SERVER: CLIENT JOINS
procedure TForm1.IdTCPServer1Connect(AContext: TIdContext);
var
  IPUser : string;
  LLine : String;
begin
IPUser := AContext.Connection.Socket.Binding.PeerIP;
LLine := AContext.Connection.IOHandler.ReadLn('',200,-1,IndyTextEncoding_UTF8);
if Copy(LLine,1,13) <> '{MIC KREDITZ ' then
   begin
   OutputText('INVALID CLIENT : '+IPUser);
   AContext.Connection.Disconnect;
   Acontext.Connection.IOHandler.InputBuffer.Clear;
   if not BLNodeExists(IPUser,'') then AddNewBLNode(IPUser,'');
   exit;
   end
else
   begin
   MAIN_USER_IP := GetParameterFromCommandLine(LLine,2);
   if IPUser = MAIN_USER_IP then
      begin
      OutputText('INCOMING CLOSED: OWN CONNECTION');
      AContext.Connection.Disconnect;
      Acontext.Connection.IOHandler.InputBuffer.Clear;
      if not BLNodeExists(IPUser,'') then AddNewBLNode(IPUser,'');
      DeleteNodeAddress(IPUser);
      exit;
      end;
   end;
if BLNodeExists(IPUser,'') then
   begin
   OutputText('BLACKLISTED FROM: '+IPUser);
   AContext.Connection.Disconnect;
   Acontext.Connection.IOHandler.InputBuffer.Clear;
   exit;
   end;
if AreWeConnectedTo(IPUser) then
   begin
   OutputText('DUPLICATE REJECTED: '+IPUser);
   AContext.Connection.Disconnect;
   Acontext.Connection.IOHandler.InputBuffer.Clear;
   exit;
   end;
if SaveConection('client',IPUser,GetTimeStamp(),Acontext,'0') = 0 then // ZERO BECAUSE INCOMING
   begin                                                               // NOT USE CLIENT CHANNEL
   AContext.Connection.IOHandler.WriteLn(GetNodesString);
   AContext.Connection.Disconnect;
   OutputText('Unable to keep conection: '+IPUser);
   Acontext.Connection.IOHandler.InputBuffer.Clear;
   end
else
   begin
   OutputText('Connection FROM: '+IPUser);
   If OptionsData.GetNodes then
      Acontext.Connection.IOHandler.WriteLn('{MIC GETNODES END}');
   end;
end;

// SERVER: CLIENT LEAVE
procedure TForm1.IdTCPServer1Disconnect(AContext: TIdContext);
var
  IPUser : string;
begin
IPUser := AContext.Connection.Socket.Binding.PeerIP;
Acontext.Connection.IOHandler.InputBuffer.Clear;
ClearConection('client',ipuser);
end;

// CLIENTS: GET LINE
procedure TForm1.ReadClientLines(Number,Slot:int64);
var
  LLine: String;
  AFileStream : TFileStream;
  BlockZipName : String;
begin
if ConexionesCliente[Number].IOHandler.InputBufferIsEmpty then
   begin
   ConexionesCliente[Number].IOHandler.CheckForDataOnSource(10);
   if ConexionesCliente[Number].IOHandler.InputBufferIsEmpty then Exit;
   end;
While not ConexionesCliente[Number].IOHandler.InputBufferIsEmpty do
   begin
   LLine := ConexionesCliente[Number].IOHandler.ReadLn(IndyTextEncoding_UTF8);
   if LLine = 'FILEACCSUM' then
      begin
      if FileExists(CONST_ArchivoAccData) then DeleteFile(CONST_ArchivoAccData);
      AFileStream := TFileStream.Create(CONST_ArchivoAccData, fmCreate);
      ConexionesCliente[Number].IOHandler.ReadStream(AFileStream);
      AFileStream.Free;
      U_MyLastAccount := true;
      U_MyAccsumHash := true;
      U_MyBalance := true;
      STATUS_LastAccountRequested := false;
      end
   else if LLine = 'BLOCKZIP' then
      begin
      BlockZipName := CONST_DirBlocks+'blocks.zip';
      if FileExists(BlockZipName) then DeleteFile(BlockZipName);
      AFileStream := TFileStream.Create(BlockZipName, fmCreate);
      ConexionesCliente[Number].IOHandler.ReadStream(AFileStream);
      AFileStream.Free;
      U_MyLastBlock := true;
      U_MyLastBLockHash := true;
      UnzipBlockFile(CONST_DirBlocks+'blocks.zip');
      BuildBlockSum();
      U_RebuildMyTxs := true;
      end
   else
      ReadsFromSlots[Slot].Add(LLine);
   end;
end;

// CHECKS KEYPRESS ON COMMANDLINE
Procedure Tform1.CommandLineKeyup(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  LineText : String;
begin
LineText := commandline.Text;
if Key=VK_RETURN then
   begin
   CommandLine.Text := '';
   LastCommandline := LineText;
   ProcessLines.add(LineText);
   end;
if Key=VK_F3 then
   Begin
   commandline.Text := LastCommandline;
   CommandLine.SelStart := Length(CommandLine.Text);
   end;
end;

// CHECK KEYPRESS ON AMMOUNT EDIT
Procedure Tform1.EditSFAmountOnChange(Sender: TObject);
var
  LineText : String;
  amount : real;
  Available, IntAmount, fee, total : int64;
Begin
Available := GetTotalAccountBalance()-GetTotalAccountPendingPayments();
LineText := EditSFAmount.Text;
if linetext = '' then
   begin
   LabelCorAmo.font.Color := clBlack;
   LabelCorAmo.Caption:= '0,00';
   exit;
   end;
if not IsValidfloat(LineText) then
   begin
   LabelCorAmo.Caption:='Err';
   exit;
   end;
Amount := StrToFloat(linetext)*100;
IntAmount := Round(Amount);
fee := GetComisionValue(IntAmount);
Total := IntAmount+fee;
if total > Available then LabelCorAmo.Font.Color :=clRed else LabelCorAmo.font.Color := clBlack;
LabelCorAmo.Caption := Int2CurrencyStr(IntAmount)+' + '+Int2CurrencyStr(fee)+' (Fee) = '+
  Int2CurrencyStr(Total);
End;

// CREATES COMPONENTS FOR MAIN FORM
Procedure Tform1.InitializeForm1();
var
  contador : integer;
  ABitmap: TBitmap;
Begin
ABitmap := TBitmap.Create;
// NON-VISUAL COMPONENTS -> FORM
PendingTXs := TStringlist.Create;
LASTBLOCK_PendingTxs := TStringlist.Create;
LASTBLOCK_TrxsIDs := TStringlist.Create;
MainMemoStrings := TStringlist.Create;
OutGoingMessages := TStringlist.Create;
ProcessLines := TStringlist.Create;
MyPendingTxs := TStringlist.Create;

for contador := 1 to CONST_MaxOutgoingConections do
   ConexionesCliente[contador] := TIdTCPClient.Create(form1);
for contador := 1 to CONST_MAXConections do
   ReadsFromSlots[contador] := TStringList.Create;

IdTCPServer1 := TIdTCPServer.Create(Form1);
IdTCPServer1.DefaultPort:=CONST_DefaultServerPort;
IdTCPServer1.Active:=false;
IdTCPServer1.UseNagle:=true;
IdTCPServer1.TerminateWaitTime:=5000;
IdTCPServer1.OnExecute:=@IdTCPServer1Execute;
IdTCPServer1.OnConnect:=@IdTCPServer1Connect;
IdTCPServer1.OnDisconnect:=@IdTCPServer1Disconnect;
IdTCPServer1.OnException:=@IdTCPServer1Exception;

SystrayIcon := TTrayIcon.Create(form1);
SystrayIcon.BalloonTimeout:=3000;
SystrayIcon.BalloonTitle:='Kreditz Wallet';
SystrayIcon.Hint:='Kreditz Ver. '+MAIN_Version;
SysTrayIcon.OnDblClick:=@DoubleClickSysTray;
ImageList1.GetBitmap(0,ABitmap);
SysTrayIcon.Icon.Assign(ABitmap);

TimerLoop:= TTimer.Create(Form1);
Timerloop.Enabled:=false;TimerLoop.Interval:=200;
TimerLoop.OnTimer:=@TimerLoopTimer;

TimerStart:= TTimer.Create(Form1);
TimerStart.Enabled:=false;TimerStart.Interval:=1;
TimerStart.OnTimer:=@TimerStartTimer;

TimerMensaje:= TTimer.Create(Form1);
TimerMensaje.Enabled:=false;TimerMensaje.Interval:=1000;
TimerMensaje.OnTimer:=@TimerMensajeTimer;

// END NON-VISUAL COMPONENTS -> FORM

// VISUAL COMPONENTS -> FORM
PanelMensaje := TPanel.Create(Form1);PanelMensaje.Parent:=form1;
PanelMensaje.Left:=270;PanelMensaje.Top:=190;PanelMensaje.Height:=20;PanelMensaje.Width:=100;
PanelMensaje.BevelColor:=clBlack;PanelMensaje.Visible:=false;
PanelMensaje.font.Name:='consolas';PanelMensaje.Font.Size:=14;

ButtonWallet := TButton.Create(Form1);ButtonWallet.Parent:=form1;
ButtonWallet.Left:=8;ButtonWallet.Top:=2;ButtonWallet.Height:=18;ButtonWallet.Width:=65;
ButtonWallet.Caption:='WALLET';ButtonWallet.Font.Style:=[fsBOLD];
ButtonWallet.Visible:=true;ButtonWallet.OnClick:=@ButtonWalletOnClick;

ButtonNodes := TButton.Create(Form1);ButtonNodes.Parent:=form1;
ButtonNodes.Left:=78;ButtonNodes.Top:=2;ButtonNodes.Height:=18;ButtonNodes.Width:=65;
ButtonNodes.Caption:='NETWORK';ButtonNodes.Font.Style:=[];
ButtonNodes.Visible:=true;ButtonNodes.OnClick:=@ButtonNodesOnClick;

ButtonConsola := TButton.Create(Form1);ButtonConsola.Parent:=form1;
ButtonConsola.Left:=148;ButtonConsola.Top:=2;ButtonConsola.Height:=18;ButtonConsola.Width:=65;
ButtonConsola.Caption:='CONSOLE';ButtonConsola.Font.Style:=[];
ButtonConsola.Visible:=true;ButtonConsola.OnClick:=@ButtonConsolaOnClick;

PanelServer := TPanel.Create(Form1);PanelServer.Parent:=form1;
PanelServer.Left:=8;PanelServer.Top:=378;PanelServer.Alignment:=taRightJustify;
PanelServer.Height:=20;PanelServer.Width:=40;PanelServer.Caption:='0/0 ';;
PanelServer.Color:=clRed;PanelServer.BevelColor:=clBlack;
PanelServer.ShowHint:=true;PanelServer.Hint:='Incoming/Outgoing connections';
PanelServer.Visible:=true;

   CheckBoxConnect := TCheckBox.Create(Form1);CheckBoxConnect.Parent:=PanelServer;
   CheckBoxConnect.Left:=2;CheckBoxConnect.Top:=0;
   CheckBoxConnect.Height:=20;CheckBoxConnect.Width:=20;
   CheckBoxConnect.Color:=clRed;CheckBoxConnect.caption:='';
   CheckBoxConnect.ShowHint:=true;CheckBoxConnect.Hint:='Check to connect to the network';
   CheckBoxConnect.Visible:=true;CheckBoxConnect.OnChange:=@CheckBoxConnectOnChange;

PanelStatus := TPanel.Create(Form1);PanelStatus.Parent:=form1;
PanelStatus.Left:=59;PanelStatus.Top:=378;PanelStatus.Height:=20;PanelStatus.Width:=100;
PanelStatus.Color:=clRed;PanelStatus.BevelColor:=clBlack;
PanelStatus.Caption:='Disconnected';PanelStatus.ShowHint:=true;PanelStatus.Hint:='Disconnected';
PanelStatus.Visible:=true;

PanelMiner := TPanel.Create(Form1);PanelMiner.Parent:=form1;
PanelMiner.Left:=169;PanelMiner.Top:=378;PanelMiner.Height:=20;PanelMiner.Width:=100;
PanelMiner.Color:=clRed;PanelMiner.BevelColor:=clBlack;
PanelMiner.ShowHint:=true;PanelMiner.Hint:='MINER: OFF';PanelMiner.caption:='MINER: OFF';
PanelMiner.Visible:=true;

   CheckBoxMiner := TCheckBox.Create(Form1);CheckBoxMiner.Parent:=PanelMiner;
   CheckBoxMiner.Left:=2;CheckBoxMiner.Top:=0;
   CheckBoxMiner.Height:=20;CheckBoxMiner.Width:=20;
   CheckBoxMiner.Color:=clRed;CheckBoxMiner.caption:='';
   CheckBoxMiner.ShowHint:=true;CheckBoxMiner.Hint:='Check to activate the miner';
   CheckBoxMiner.Visible:=true;CheckBoxMiner.OnChange:=@CheckBoxMinerOnChange;

// END VISUAL COMPONENTS -> FORM

// VISUAL COMPONENTES -> PANEL DATA (CONSOLE)
PanelData := TPanel.Create(Form1);PanelData.Parent:=form1;
PanelData.Left:=8;PanelData.Top:=18;PanelData.Height:=358;PanelData.Width:=628;
PanelData.Color:=clDefault;PanelData.BevelColor:=clBlack;
PanelData.Visible:=false;

   LabelUser := TLabel.Create(Form1);LabelUser.Parent:=PanelData;
   LabelUser.Font.Name:='consolas';LabelUser.Font.Size:=8;
   LabelUser.Left:=8;LabelUser.Top:=4;LabelUser.Font.Color:=clBlack;
   LabelUser.Caption:='Account   : ';LabelUser.Visible:=true;

   LabelBalance := TLabel.Create(Form1);LabelBalance.Parent:=PanelData;
   LabelBalance.Font.Name:='consolas';LabelBalance.Font.Size:=8;
   LabelBalance.Left:=168;LabelBalance.Top:=4;LabelBalance.Font.Color:=clBlack;
   LabelBalance.Caption:='Balance   : ';LabelBalance.Visible:=true;

   LabelListen := TLabel.Create(Form1);LabelListen.Parent:=PanelData;
   LabelListen.Font.Name:='consolas';LabelListen.Font.Size:=8;
   LabelListen.Left:=8;LabelListen.Top:=18;LabelListen.Font.Color:=clBlack;
   LabelListen.Caption:='Listening : ';LabelListen.Visible:=true;

   LabelConections := TLabel.Create(Form1);LabelConections.Parent:=PanelData;
   LabelConections.Font.Name:='consolas';LabelConections.Font.Size:=8;
   LabelConections.Left:=8;LabelConections.Top:=32;LabelConections.Font.Color:=clBlack;
   LabelConections.Caption:='Conections: ';LabelConections.Visible:=true;

   LabelNodes := TLabel.Create(Form1);LabelNodes.Parent:=PanelData;
   LabelNodes.Font.Name:='consolas';LabelNodes.Font.Size:=8;
   LabelNodes.Left:=8;LabelNodes.Top:=46;LabelNodes.Font.Color:=clBlack;
   LabelNodes.Caption:='ReachNodes: ';LabelNodes.Visible:=true;

   LabelAccsumHash := TLabel.Create(Form1);LabelAccsumHash.Parent:=PanelData;LabelAccsumHash.Font.Name:='consolas';
   LabelAccsumHash.Font.Size:=8;LabelAccsumHash.Left:=8;LabelAccsumHash.Top:=60;
   LabelAccsumHash.Font.Color:=clBlack;LabelAccsumHash.Caption:='AccSumHash: ';LabelAccsumHash.Visible:=true;

   LabelBlock := TLabel.Create(Form1);LabelBlock.Parent:=PanelData;
   LabelBlock.Font.Name:='consolas';LabelBlock.Font.Size:=8;
   LabelBlock.Left:=168;LabelBlock.Top:=18;LabelBlock.Font.Color:=clBlack;
   LabelBlock.Caption:='Blocks    : ';LabelBlock.Visible:=true;

   LabelAccos := TLabel.Create(Form1);LabelAccos.Parent:=PanelData;LabelAccos.
   Font.Name:='consolas';LabelAccos.Font.Size:=8;
   LabelAccos.Left:=168;LabelAccos.Top:=32;LabelAccos.Font.Color:=clBlack;
   LabelAccos.Caption:='Accounts  : ';LabelAccos.Visible:=true;

   LabelPending := TLabel.Create(Form1);LabelPending.Parent:=PanelData;
   LabelPending.Font.Name:='consolas';LabelPending.Font.Size:=8;LabelPending.Left:=168;
   LabelPending.Top:=46;LabelPending.Font.Color:=clBlack;
   LabelPending.Caption:='Pending Tx: ';LabelPending.Visible:=true;

   LabelDifficult := TLabel.Create(Form1);LabelDifficult.Parent:=PanelData;
   LabelDifficult.Font.Name:='consolas';LabelDifficult.Font.Size:=8;LabelDifficult.Left:=168;
   LabelDifficult.Top:=60;LabelDifficult.Font.Color:=clBlack;
   LabelDifficult.Caption:='Difficult : ';LabelDifficult.Visible:=true;

   LabelLast20 := TLabel.Create(Form1);LabelLast20.Parent:=PanelData;
   LabelLast20.Font.Name:='consolas';LabelLast20.Font.Size:=8;LabelLast20.Left:=378;
   LabelLast20.Top:=4;LabelLast20.Font.Color:=clBlack;
   LabelLast20.Caption:='Last 20   : ';LabelLast20.Visible:=true;

   LabelMiner := TLabel.Create(Form1);LabelMiner.Parent:=PanelData;LabelMiner.
   Font.Name:='consolas';LabelMiner.Font.Size:=8;
   LabelMiner.Left:=378;LabelMiner.Top:=18;LabelMiner.Font.Color:=clBlack;
   LabelMiner.Caption:='Hash MH/S : ';LabelMiner.Visible:=true;

   LabelTarget := TLabel.Create(Form1);LabelTarget.Parent:=PanelData;
   LabelTarget.Font.Name:='consolas';LabelTarget.Font.Size:=8;LabelTarget.Left:=378;
   LabelTarget.Top:=32;LabelTarget.Font.Color:=clBlack;
   LabelTarget.Caption:='Target    : ';LabelTarget.Visible:=true;

   LabelLastPing := TLabel.Create(Form1);LabelLastPing.Parent:=PanelData;
   LabelLastPing.Font.Name:='consolas';LabelLastPing.Font.Size:=8;
   LabelLastPing.Left:=378;LabelLastPing.Top:=46;LabelLastPing.Font.Color:=clBlack;
   LabelLastPing.Caption:='Last Ping : ';LabelLastPing.Visible:=true;

   LabelThisBlock := TLabel.Create(Form1);LabelThisBlock.Parent:=PanelData;
   LabelThisBlock.Font.Name:='consolas';LabelThisBlock.Font.Size:=8;
   LabelThisBlock.Left:=378;LabelThisBlock.Top:=60;LabelThisBlock.Font.Color:=clBlack;
   LabelThisBlock.Caption:='This Block: ';LabelThisBlock.Visible:=true;

   MainMemo := TMemo.Create(Form1);
   MainMemo.Parent:=PanelData;
   MainMemo.Left:=8;MainMemo.Top:=76;Mainmemo.Height:=250;mainmemo.Width:=614;
   mainmemo.Color:=clblack;mainmemo.Font.Color:=clwhite;mainmemo.ReadOnly:=true;
   mainmemo.Font.Size:=10;mainmemo.Font.Name:='consolas';
   mainmemo.Visible:=true;mainmemo.ScrollBars:=ssvertical;

   CommandLine := TEdit.Create(Form1); CommandLine.Parent:=PanelData;CommandLine.Font.Name:='consolas';
   CommandLine.Left:=8;CommandLine.Top:=330;CommandLine.Height:=12;CommandLine.Width:=614;
   CommandLine.AutoSize:=true;CommandLine.Color:=clBlack;CommandLine.Font.Color:=clWhite;
   CommandLine.Visible:=true;Commandline.OnKeyUp:=@CommandLineKeyup;
// END VISUAL COMPONENTS -> PANEL DATA (CONSOLE)

// VISUAL COMPONENTES -> PANEL NETWORK
PanelNetwork := TPanel.Create(Form1);PanelNetwork.Parent:=form1;
PanelNetwork.Left:=8;PanelNetwork.Top:=18;PanelNetwork.Height:=358;PanelNetwork.Width:=628;
PanelNetwork.Color:=clDefault;PanelNetwork.BevelColor:=clBlack;
PanelNetwork.Visible:=false;

   LabelPanNodes := TLabel.Create(Form1);LabelPanNodes.Parent:=PanelNetwork;
   LabelPanNodes.Caption:='NODES';LabelPanNodes.Font.Size:=11;
   LabelPanNodes.Font.Name:='consolas';
   LabelPanNodes.Left:=68;LabelPanNodes.Top:=10;

   GridNodes := TStringGrid.Create(Form1);GridNodes.Parent:=PanelNetwork;
   GridNodes.Left:=8;GridNodes.Top:=28;GridNodes.Height:=136;GridNodes.Width:=172;
   GridNodes.ColCount:=2;GridNodes.rowcount:=1;GridNodes.FixedCols:=0;GridNodes.FixedRows:=1;
   GridNodes.Options:= GridNodes.Options+[goRowSelect];
   GridNodes.ScrollBars:=ssvertical;
   GridNodes.Cells[0,0]:='IP';GridNodes.Cells[1,0]:='Port';
   GridNodes.ColWidths[0]:= 100;GridNodes.ColWidths[1]:= 50;

   ButtonDeleteNode := TButton.Create(Form1);ButtonDeleteNode.Parent:=PanelNetwork;
   ButtonDeleteNode.Left:=8;ButtonDeleteNode.Top:=164;ButtonDeleteNode.Height:=18;ButtonDeleteNode.Width:=60;
   ButtonDeleteNode.Caption:='Delete';ButtonDeleteNode.Font.Name:='consolas';
   ButtonDeleteNode.Visible:=true;ButtonDeleteNode.OnClick:=@ButtonDeleteNodeOnClick;

   ButtonAddNode := TButton.Create(Form1);ButtonAddNode.Parent:=PanelNetwork;
   ButtonAddNode.Left:=120;ButtonAddNode.Top:=164;ButtonAddNode.Height:=18;ButtonAddNode.Width:=60;
   ButtonAddNode.Caption:='Add';ButtonAddNode.Font.Name:='consolas';
   ButtonAddNode.Visible:=true;ButtonAddNode.OnClick:=@ButtonAddnodeOnClick;

      PanelAddNode := TPanel.Create(Form1);PanelAddNode.Parent:=PanelNetwork;
      PanelAddNode.Left:=8;PanelAddNode.Top:=28;PanelAddNode.Height:=154;PanelAddNode.Width:=172;
      PanelAddNode.Color:=clDefault;PanelAddNode.BevelColor:=clBlack;
      PanelAddNode.Visible:=false;

         EditAddnodeIP := TLabeledEdit.Create(Form1); EditAddnodeIP.Parent:=PanelAddNode;
         EditAddnodeIP.Font.Name:='consolas';EditAddnodeIP.Font.Size:=8;
         EditAddnodeIP.Left:=8;EditAddnodeIP.Top:=28;
         EditAddnodeIP.Height:=12;EditAddnodeIP.Width:=156;
         EditAddnodeIP.Color:=clBlack;EditAddnodeIP.Font.Color:=clWhite;
         EditAddnodeIP.Visible:=true;EditAddnodeIP.Alignment:=taRightJustify;
         EditAddnodeIP.LabelPosition:=lpabove;EditAddnodeIP.EditLabel.Caption:='IP';
         EditAddnodeIP.LabelSpacing:=0;

         EditAddNodePort := TLabeledEdit.Create(Form1); EditAddNodePort.Parent:=PanelAddNode;
         EditAddNodePort.Font.Name:='consolas';EditAddNodePort.Font.Size:=8;
         EditAddNodePort.Left:=8;EditAddNodePort.Top:=64;
         EditAddNodePort.Height:=12;EditAddNodePort.Width:=156;
         EditAddNodePort.Color:=clBlack;EditAddNodePort.Font.Color:=clWhite;
         EditAddNodePort.Visible:=true;EditAddNodePort.Alignment:=taRightJustify;
         EditAddNodePort.LabelPosition:=lpabove;EditAddNodePort.EditLabel.Caption:='PORT';
         EditAddNodePort.LabelSpacing:=0;

         ButAddNodeCancel := TButton.Create(Form1);ButAddNodeCancel.Parent:=PanelAddNode;
         ButAddNodeCancel.Left:=8;ButAddNodeCancel.Top:=130;ButAddNodeCancel.Height:=18;ButAddNodeCancel.Width:=60;
         ButAddNodeCancel.Caption:='Cancel';ButAddNodeCancel.Font.Name:='consolas';
         ButAddNodeCancel.Visible:=true;ButAddNodeCancel.OnClick:=@ButAddNodeCancelOnClick;

         ButAddNodeAdd := TButton.Create(Form1);ButAddNodeAdd.Parent:=PanelAddNode;
         ButAddNodeAdd.Left:=104;ButAddNodeAdd.Top:=130;ButAddNodeAdd.Height:=18;ButAddNodeAdd.Width:=60;
         ButAddNodeAdd.Caption:='Add';ButAddNodeAdd.Font.Name:='consolas';
         ButAddNodeAdd.Visible:=true;ButAddNodeAdd.OnClick:=@ButAddNodeAddOnClick;

   LabelPanBLNodes := TLabel.Create(Form1);LabelPanBLNodes.Parent:=PanelNetwork;
   LabelPanBLNodes.Caption:='BLACKLISTED';LabelPanBLNodes.Font.Size:=11;
   LabelPanBLNodes.Font.Name:='consolas';
   LabelPanBLNodes.Left:=195;LabelPanBLNodes.Top:=10;

   GridBLNodes := TStringGrid.Create(Form1);GridBLNodes.Parent:=PanelNetwork;
   GridBLNodes.Left:=190;GridBLNodes.Top:=28;GridBLNodes.Height:=136;GridBLNodes.Width:=122;
   GridBLNodes.ColCount:=1;GridBLNodes.rowcount:=1;GridBLNodes.FixedCols:=0;
   GridBLNodes.FixedRows:=1;GridBLNodes.Options:= GridBLNodes.Options+[goRowSelect];
   GridBLNodes.ScrollBars:=ssvertical;
   GridBLNodes.Cells[0,0]:='IP';GridBLNodes.ColWidths[0]:= 100;

   ButtonDeleteBL := TButton.Create(Form1);ButtonDeleteBL.Parent:=PanelNetwork;
   ButtonDeleteBL.Left:=190;ButtonDeleteBL.Top:=164;ButtonDeleteBL.Height:=18;
   ButtonDeleteBL.Width:=60;ButtonDeleteBL.Caption:='Delete';
   ButtonDeleteBL.Font.Name:='consolas';ButtonDeleteBL.Visible:=true;
   ButtonDeleteBL.OnClick:=@ButtonDeleteBLOnClick;

   PanelConexs := TPanel.Create(Form1);PanelConexs.Parent:=PanelNetwork;
   PanelConexs.Left:=8;PanelConexs.Top:=190;PanelConexs.Height:=162;
   PanelConexs.Width:=612;
   PanelConexs.Color:=clWhite;PanelConexs.BevelColor:=clScrollBar;
   PanelConexs.Visible:=true;

      LabelPanConxs := TLabel.Create(Form1);LabelPanConxs.Parent:=PanelConexs;
      LabelPanConxs.Caption:='CONNECTIONS';LabelPanConxs.Font.Size:=11;
      LabelPanConxs.Font.Name:='consolas';
      LabelPanConxs.Left:=275;LabelPanConxs.Top:=4;

      GridConxs := TStringGrid.Create(Form1);GridConxs.Parent:=PanelConexs;
      GridConxs.Left:=105;GridConxs.Top:=22;GridConxs.Height:=126;GridConxs.Width:=402;
      GridConxs.ColCount:=9;GridConxs.rowcount:=CONST_MAXConections+1;
      GridConxs.Font.Name:='consolas';GridConxs.Font.Size:=8;
      GridConxs.FixedCols:=0;GridConxs.FixedRows:=1;
      GridConxs.Options:= GridConxs.Options+[goRowSelect];
      GridConxs.ScrollBars:=ssvertical;GridConxs.AutoSize:=true;
      GridConxs.Cells[0,0]:='#';GridConxs.Cells[1,0]:='IP';GridConxs.Cells[2,0]:='Type';
      GridConxs.Cells[3,0]:='LB';GridConxs.Cells[4,0]:='LBH';
      GridConxs.Cells[5,0]:='ASH';GridConxs.Cells[6,0]:='Version';
      GridConxs.Cells[7,0]:='SRV';GridConxs.Cells[8,0]:='Diff';
      GridConxs.OnPrepareCanvas:= @GridConxsPrepareCanvas;

   PanelOptions := TScrollBox.Create(Form1);PanelOptions.Parent:=PanelNetwork;
   PanelOptions.Left:=320;PanelOptions.Top:=8;PanelOptions.Height:=176;
   PanelOptions.Width:=300;
   PanelOptions.Color:=clWhite;
   PanelOptions.Visible:=true;

      LabelUserPort := TLabel.Create(Form1);LabelUserPort.Parent:=PanelOptions;
      LabelUserPort.Caption:='Listening Port';LabelUserPort.Font.Size:=8;
      LabelUserPort.Font.Name:='consolas';
      LabelUserPort.Left:=8;LabelUserPort.Top:=10;

      EditUserPort := TEdit.Create(Form1); EditUserPort.Parent:=PanelOptions;
      EditUserPort.Font.Name:='consolas';EditUserPort.Font.Size:=8;
      EditUserPort.Left:=230;EditUserPort.Top:=6;
      EditUserPort.Height:=12;EditUserPort.Width:=40;
      EditUserPort.Color:=clBlack;EditUserPort.Font.Color:=clWhite;
      EditUserPort.Visible:=true;EditUserPort.Alignment:=taRightJustify;
      EditUserPort.OnEditingDone:=@EditUserPortOnEditingDone;

      LabelCpuMiner := TLabel.Create(Form1);LabelCpuMiner.Parent:=PanelOptions;
      LabelCpuMiner.Caption:='Minning CPUs';LabelCpuMiner.Font.Size:=8;
      LabelCpuMiner.Font.Name:='consolas';
      LabelCpuMiner.Left:=8;LabelCpuMiner.Top:=40;

      ComboCPUMiner := TComboBox.Create(Form1);ComboCPUMiner.Parent :=PanelOptions ;
      ComboCPUMiner.Font.Name:='consolas';ComboCPUMiner.Font.Size:=8;
      ComboCPUMiner.Left:=230;ComboCPUMiner.Top:=36;
      ComboCPUMiner.Height:=12;ComboCPUMiner.Width:=40;
      ComboCPUMiner.ReadOnly:=true;
      For contador := 1 to MAIN_CPUCOUNT do ComboCPUMiner.Items.Add(IntToStr(contador));
      ComboCPUMiner.OnChange:=@ComboCPUMinerOnChange;

      LabelMinimize := TLabel.Create(Form1);LabelMinimize.Parent:=PanelOptions;
      LabelMinimize.Caption:='Minimize to Systray';LabelMinimize.Font.Size:=8;
      LabelMinimize.Font.Name:='consolas';
      LabelMinimize.Left:=8;LabelMinimize.Top:=70;

      CheckBoxMin := TCheckBox.Create(Form1);CheckBoxMin.Parent:=PanelOptions;
      CheckBoxMin.Left:=250;CheckBoxMin.Top:=70;
      CheckBoxMin.Height:=20;CheckBoxMin.Width:=20;
      CheckBoxMin.Color:=clRed;CheckBoxMin.caption:='';
      CheckBoxMin.ShowHint:=true;CheckBoxMin.Hint:='Check to minimize to System tray';
      CheckBoxMin.Visible:=true;CheckBoxMin.OnChange:=@CheckBoxMinOnChange;

      LabelAutoConnect := TLabel.Create(Form1);LabelAutoConnect.Parent:=PanelOptions;
      LabelAutoConnect.Caption:='Autoconnect at launch';LabelAutoConnect.Font.Size:=8;
      LabelAutoConnect.Font.Name:='consolas';
      LabelAutoConnect.Left:=8;LabelAutoConnect.Top:=100;

      CheckBoxAutoConn := TCheckBox.Create(Form1);CheckBoxAutoConn.Parent:=PanelOptions;
      CheckBoxAutoConn.Left:=250;CheckBoxAutoConn.Top:=100;
      CheckBoxAutoConn.Height:=20;CheckBoxAutoConn.Width:=20;
      CheckBoxAutoConn.Color:=clRed;CheckBoxAutoConn.caption:='';
      CheckBoxAutoConn.ShowHint:=true;CheckBoxAutoConn.Hint:='Check to Auto-Connect at start';
      CheckBoxAutoConn.Visible:=true;CheckBoxAutoConn.OnChange:=@CheckBoxAutoConnOnChange;

      LabelFullNode := TLabel.Create(Form1);LabelFullNode.Parent:=PanelOptions;
      LabelFullNode.Caption:='Run Full Node';LabelFullNode.Font.Size:=8;
      LabelFullNode.Font.Name:='consolas';
      LabelFullNode.Left:=8;LabelFullNode.Top:=130;

      CheckBoxFullNode := TCheckBox.Create(Form1);CheckBoxFullNode.Parent:=PanelOptions;
      CheckBoxFullNode.Left:=250;CheckBoxFullNode.Top:=130;
      CheckBoxFullNode.Height:=20;CheckBoxFullNode.Width:=20;
      CheckBoxFullNode.Color:=clRed;CheckBoxFullNode.caption:='';
      CheckBoxFullNode.ShowHint:=true;CheckBoxFullNode.Hint:='Check to Run a Full Node';
      CheckBoxFullNode.Visible:=true;CheckBoxFullNode.OnChange:=@CheckBoxFullNodeOnChange;

      LabelUpdateNodes := TLabel.Create(Form1);LabelUpdateNodes.Parent:=PanelOptions;
      LabelUpdateNodes.Caption:='Update Nodes from Network';LabelUpdateNodes.Font.Size:=8;
      LabelUpdateNodes.Font.Name:='consolas';
      LabelUpdateNodes.Left:=8;LabelUpdateNodes.Top:=160;

      CheckBoxUpdateNodes := TCheckBox.Create(Form1);CheckBoxUpdateNodes.Parent:=PanelOptions;
      CheckBoxUpdateNodes.Left:=250;CheckBoxUpdateNodes.Top:=160;
      CheckBoxUpdateNodes.Height:=20;CheckBoxUpdateNodes.Width:=20;
      CheckBoxUpdateNodes.Color:=clRed;CheckBoxUpdateNodes.caption:='';
      CheckBoxUpdateNodes.ShowHint:=true;CheckBoxUpdateNodes.Hint:='Check to Update Nodes from Peers';
      CheckBoxUpdateNodes.Visible:=true;CheckBoxUpdateNodes.OnChange:=@CheckBoxUpdateNodesOnChange;

      LabelShowMinned := TLabel.Create(Form1);LabelShowMinned.Parent:=PanelOptions;
      LabelShowMinned.Caption:='Show minned as Trx';LabelShowMinned.Font.Size:=8;
      LabelShowMinned.Font.Name:='consolas';
      LabelShowMinned.Left:=8;LabelShowMinned.Top:=190;

      CheckBoxMinned := TCheckBox.Create(Form1);CheckBoxMinned.Parent:=PanelOptions;
      CheckBoxMinned.Left:=250;CheckBoxMinned.Top:=190;
      CheckBoxMinned.Height:=20;CheckBoxMinned.Width:=20;
      CheckBoxMinned.Color:=clRed;CheckBoxMinned.caption:='';
      CheckBoxMinned.ShowHint:=true;CheckBoxMinned.Hint:='Check to show minned blocks as Trxs';
      CheckBoxMinned.Visible:=true;CheckBoxMinned.OnChange:=@CheckBoxMinnedOnChange;

// END VISUAL COMPONENTS -> PANEL NETWORK

// VISUAL COMPONENTES -> PANEL WALLET
PanelWallet := TPanel.Create(Form1);PanelWallet.Parent:=form1;
PanelWallet.Left:=8;PanelWallet.Top:=18;PanelWallet.Height:=358;PanelWallet.Width:=628;
PanelWallet.Color:=clDefault;PanelWallet.BevelColor:=clBlack;
PanelWallet.Visible:=true;

   PanelSecAccTitle := TPanel.Create(Form1);PanelSecAccTitle.Parent:=PanelWallet;
   PanelSecAccTitle.Left:=8;PanelSecAccTitle.Top:=8;PanelSecAccTitle.Height:=32;
   PanelSecAccTitle.Width:=612;
   PanelSecAccTitle.Color:=clWhite;PanelSecAccTitle.BevelColor:=clScrollBar;
   PanelSecAccTitle.Visible:=true;

      LabelBigAccNumber := TLabel.Create(Form1);LabelBigAccNumber.Parent:=PanelSecAccTitle;
      LabelBigAccNumber.Caption:='Account Number: ';LabelBigAccNumber.Font.Size:=14;
      LabelBigAccNumber.AutoSize:=false;LabelBigAccNumber.Left:= 8;
      LabelBigAccNumber.Top:=2;LabelBigAccNumber.Width:=310;LabelBigAccNumber.Height:=20;
      LabelBigAccNumber.Alignment:=taLeftJustify;LabelBigAccNumber.Font.Name:='consolas';

      LabelBigBalance := TLabel.Create(Form1);LabelBigBalance.Parent:=PanelSecAccTitle;
      LabelBigBalance.Caption:='0 KDZ';LabelBigBalance.Font.Size:=18;LabelBigBalance.AutoSize:=false;
      LabelBigBalance.Left:= 306;LabelBigBalance.Top:=2;LabelBigBalance.Width:=304;
      LabelBigBalance.Height:=28;LabelBigBalance.Alignment:=taRightJustify;
      LabelBigBalance.Font.Name:='consolas';

   GridAddresses := TStringGrid.Create(Form1);GridAddresses.Parent:=PanelWallet;
   GridAddresses.Left:=8;GridAddresses.Top:=206;
   GridAddresses.Height:=144;GridAddresses.Width:=372;
   GridAddresses.ColCount:=2;GridAddresses.rowcount:=1;GridAddresses.FixedCols:=0;GridAddresses.FixedRows:=1;
   GridAddresses.Options:= GridAddresses.Options+[goRowSelect];GridAddresses.Font.Name:='consolas';
   GridAddresses.ScrollBars:=ssvertical;GridAddresses.Font.Size:=8;
   GridAddresses.Cells[0,0]:='Address';GridAddresses.Cells[1,0]:='Balance';
   GridAddresses.OnPrepareCanvas:= @GridAddressesPrepareCanvas;

      ButtonNewAddress := TButton.Create(Form1);ButtonNewAddress.Parent:=GridAddresses;
      ButtonNewAddress.Left:=72;ButtonNewAddress.Top:=1;ButtonNewAddress.Height:=18;ButtonNewAddress.Width:=60;
      ButtonNewAddress.Caption:='New';ButtonNewAddress.Font.Name:='consolas';
      ButtonNewAddress.Visible:=true;ButtonNewAddress.OnClick:=@ButtonNewAddressOnClick;
      ButtonNewAddress.ShowHint:=true;ButtonNewAddress.Hint:='Get a New Address';

      ButtonCopyAddress := TButton.Create(Form1);ButtonCopyAddress.Parent:=GridAddresses;
      ButtonCopyAddress.Left:=137;ButtonCopyAddress.Top:=1;ButtonCopyAddress.Height:=18;
      ButtonCopyAddress.Width:=60;ButtonCopyAddress.Caption:='Copy';
      ButtonCopyAddress.Font.Name:='consolas';ButtonCopyAddress.Visible:=true;
      ButtonCopyAddress.OnClick:=@ButtonCopyAddressOnClick;
      ButtonCopyAddress.ShowHint:=true;ButtonCopyAddress.Hint:='Copy Address to Clipboard';

   PanelSendFunds := TPanel.Create(Form1);PanelSendFunds.Parent:=PanelWallet;
   PanelSendFunds.Left:=8;PanelSendFunds.Top:=48;PanelSendFunds.Height:=150;
   PanelSendFunds.Width:=372;
   PanelSendFunds.Color:=clGradientInactiveCaption;PanelSendFunds.BevelColor:=clScrollBar;
   PanelSendFunds.Visible:=true;

      LabelSendFunds := TLabel.Create(Form1);LabelSendFunds.Parent:=PanelSendFunds;
      LabelSendFunds.Caption:='Send Funds';LabelSendFunds.Font.Size:=12;
      LabelSendFunds.Font.Name:='consolas';
      LabelSendFunds.Left:=150;LabelSendFunds.Top:=4;

      LabelSFDestination := TLabel.Create(Form1);LabelSFDestination.Parent:=PanelSendFunds;
      LabelSFDestination.Caption:='Destination';LabelSFDestination.Font.Size:=8;
      LabelSFDestination.Font.Name:='consolas';
      LabelSFDestination.Left:=8;LabelSFDestination.Top:=26;

      ButtonPasteDest := TButton.Create(Form1);ButtonPasteDest.Parent:=PanelSendFunds;
      ButtonPasteDest.Left:=92;ButtonPasteDest.Top:=22;ButtonPasteDest.Height:=18;
      ButtonPasteDest.Width:=18;ButtonPasteDest.Caption:='P';
      ButtonPasteDest.Font.Name:='consolas';ButtonPasteDest.Visible:=true;
      ButtonPasteDest.Font.Size:=8;
      ButtonPasteDest.OnClick:=@ButtonPasteDestOnClick;
      ButtonPasteDest.ShowHint:=true;ButtonPasteDest.Hint:='Paste From Clipboard';

      EditSFDesti := TEdit.Create(Form1); EditSFDesti.Parent:=PanelSendFunds;
      EditSFDesti.Font.Name:='consolas';EditSFDesti.Font.Size:=8;
      EditSFDesti.Left:=112;EditSFDesti.Top:=22;EditSFDesti.Height:=12;EditSFDesti.Width:=258;
      EditSFDesti.Color:=clBlack;EditSFDesti.Font.Color:=clWhite;
      EditSFDesti.Visible:=true;EditSFDesti.Alignment:=taRightJustify;

      LabelSFAmount := TLabel.Create(Form1);LabelSFAmount.Parent:=PanelSendFunds;
      LabelSFAmount.Caption:='Amount';LabelSFAmount.Font.Size:=8;
      LabelSFAmount.Font.Name:='consolas';
      LabelSFAmount.Left:=8;LabelSFAmount.Top:=46;

      ButtonMaxAmo := TButton.Create(Form1);ButtonMaxAmo.Parent:=PanelSendFunds;
      ButtonMaxAmo.Left:=92;ButtonMaxAmo.Top:=42;ButtonMaxAmo.Height:=18;
      ButtonMaxAmo.Width:=18;ButtonMaxAmo.Caption:='M';
      ButtonMaxAmo.Font.Name:='consolas';ButtonMaxAmo.Visible:=true;
      ButtonMaxAmo.Font.Size:=8;
      ButtonMaxAmo.OnClick:=@ButtonMaxAmoOnClick;
      ButtonMaxAmo.ShowHint:=true;ButtonMaxAmo.Hint:='Set maximum available';

      EditSFAmount := TEdit.Create(Form1); EditSFAmount.Parent:=PanelSendFunds;
      EditSFAmount.Font.Name:='consolas';EditSFAmount.Font.Size:=8;
      EditSFAmount.Left:=112;EditSFAmount.Top:=42;EditSFAmount.Height:=12;EditSFAmount.Width:=258;
      EditSFAmount.Color:=clBlack;EditSFAmount.Font.Color:=clWhite;
      EditSFAmount.Visible:=true;EditSFAmount.Alignment:=taRightJustify;
      EditSFAmount.OnChange:=@EditSFAmountOnChange;

      LabelConcept := TLabel.Create(Form1);LabelConcept.Parent:=PanelSendFunds;
      LabelConcept.Caption:='Concept';LabelConcept.Font.Size:=8;
      LabelConcept.Font.Name:='consolas';
      LabelConcept.Left:=8;LabelConcept.Top:=66;

      EditConcept := TEdit.Create(Form1); EditConcept.Parent:=PanelSendFunds;
      EditConcept.Font.Name:='consolas';EditConcept.Font.Size:=8;
      EditConcept.Left:=112;EditConcept.Top:=62;EditConcept.Height:=12;EditConcept.Width:=258;
      EditConcept.Color:=clBlack;EditConcept.Font.Color:=clWhite;
      EditConcept.Visible:=true;EditConcept.Alignment:=taRightJustify;


      LabelCorAmo := TLabel.Create(Form1);LabelCorAmo.Parent:=PanelSendFunds;
      LabelCorAmo.Caption:='0,00';LabelCorAmo.Font.Size:=8;
      LabelCorAmo.Font.Name:='consolas';LabelCorAmo.Alignment:=taRightJustify;
      LabelCorAmo.Left:=344;LabelCorAmo.Top:=86;

      ButtonSendFunds := TButton.Create(Form1);ButtonSendFunds.Parent:=PanelSendFunds;
      ButtonSendFunds.Left:=160;ButtonSendFunds.Top:=124;
      ButtonSendFunds.Height:=24;ButtonSendFunds.Width:=60;
      ButtonSendFunds.Font.Size:=12;
      ButtonSendFunds.Caption:='Send';ButtonSendFunds.Font.Name:='consolas';
      ButtonSendFunds.Visible:=true;ButtonSendFunds.OnClick:=@ButtonSendFundsOnClick;
      ButtonSendFunds.ShowHint:=true;ButtonSendFunds.Hint:='Send the funds';

   GridPending := TStringGrid.Create(Form1);GridPending.Parent:=PanelWallet;
   GridPending.Left:=388;GridPending.Top:=48;GridPending.Height:=150;
   GridPending.Width:=232;GridPending.ColCount:=1;GridPending.rowcount:=1;
   GridPending.FixedCols:=0;GridPending.FixedRows:=1;
   GridPending.Font.Name:='consolas';
   GridPending.ScrollBars:=ssvertical;GridPending.Font.Size:=12;
   GridPending.Cells[0,0]:='Amount';GridPending.ColWidths[0]:= 210;
   GridPending.OnPrepareCanvas:= @GridPendingPrepareCanvas;

   GrisUserTrxs := TStringGrid.Create(Form1);GrisUserTrxs.Parent:=PanelWallet;
   GrisUserTrxs.Left:=388;GrisUserTrxs.Top:=206;GrisUserTrxs.Height:=144;
   GrisUserTrxs.Width:=232;GrisUserTrxs.ColCount:=8;GrisUserTrxs.rowcount:=1;
   GrisUserTrxs.FixedCols:=0;GrisUserTrxs.FixedRows:=1;
   GrisUserTrxs.Font.Name:='consolas';
   GrisUserTrxs.ScrollBars:=ssvertical;GrisUserTrxs.Font.Size:=10;
   GrisUserTrxs.Cells[0,0]:='Block';GrisUserTrxs.ColWidths[0]:= 50;
   GrisUserTrxs.Cells[1,0]:='Amount';GrisUserTrxs.ColWidths[1]:= 160;
   GrisUserTrxs.Cells[2,0]:='TrxID';GrisUserTrxs.ColWidths[2]:= 1;
   GrisUserTrxs.Cells[3,0]:='Type';GrisUserTrxs.ColWidths[3]:= 1;
   GrisUserTrxs.OnPrepareCanvas:= @GrisUserTrxsPrepareCanvas;
   GrisUserTrxs.Options:= GrisUserTrxs.Options+[goRowSelect];

      ButtonDetails := TButton.Create(Form1);ButtonDetails.Parent:=GrisUserTrxs;
      ButtonDetails.Left:=62;ButtonDetails.Top:=1;
      ButtonDetails.Height:=18;ButtonDetails.Width:=60;
      ButtonDetails.Caption:='Details';ButtonDetails.Font.Name:='consolas';
      ButtonDetails.Visible:=true;ButtonDetails.OnClick:=@ButtonDetailsOnClick;
      ButtonDetails.ShowHint:=true;ButtonDetails.Hint:='Show transaction details';

// END VISUAL COMPONENTS -> PANEL WALLET

// FORM NOTIFY
FormNotify := Tform.Create(Form1);
FormNotify.Height:=200;FormNotify.Width:=360;
FormNotify.Position:=poOwnerFormCenter;
FormNotify.Caption:='Kreditz Wallet';
FormNotify.BorderStyle:=bsDialog;
FormNotify.BorderIcons:=[];
FormNotify.Visible:=false;
FormNotify.PopupParent := Self;

   MemoNotify := TMemo.Create(Form1);MemoNotify.Parent:=FormNotify;
   MemoNotify.Font.Size:=10;MemoNotify.ReadOnly:=true;
   MemoNotify.Color:=clForm;MemoNotify.BorderStyle:=bsNone;
   MemoNotify.Height:=150;MemoNotify.Width:=340;
   MemoNotify.Font.Name:='consolas';MemoNotify.Alignment:=taLeftJustify;
   MemoNotify.Left:=10;MemoNotify.Top:=10;MemoNotify.AutoSize:=false;

   ButtonCloseNotify := TButton.Create(FormNotify);ButtonCloseNotify.Parent:=FormNotify;
   ButtonCloseNotify.Left:=150;ButtonCloseNotify.Top:=170;
   ButtonCloseNotify.Height:=24;ButtonCloseNotify.Width:=60;
   ButtonCloseNotify.Caption:='Close';ButtonCloseNotify.Font.Name:='consolas';
   ButtonCloseNotify.Font.Size:=12;
   ButtonCloseNotify.Visible:=true;ButtonCloseNotify.OnClick:=@ButtonCloseNotifyOnClick;
// END FORM NOTIFY
ABitmap.Free;
End;

// ON FORM CLOSE
procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
MINER_IsMinerOn := false;
OutputText('Closing...');
CloseConectionsToServers();
Sleep(150);
TurnListenOff();
end;

// CLICK TO SHOW CONSOLE
procedure TForm1.ButtonConsolaOnClick(Sender: TObject);
begin
PanelData.Visible := true;ButtonConsola.Font.Style:=[fsBOLD];
PanelNetwork.Visible:= false;ButtonNodes.Font.Style:=[];
PanelWallet.Visible:= false;ButtonWallet.Font.Style:=[];
commandline.SetFocus;
end;

// CLICK TO SHOW NODES
procedure TForm1.ButtonNodesOnClick(Sender: TObject);
Begin
PanelData.Visible := false;ButtonConsola.Font.Style:=[];
PanelNetwork.Visible:= true;ButtonNodes.Font.Style:=[fsBOLD];
PanelWallet.Visible:= false;ButtonWallet.Font.Style:=[];
UpdateGridNodes();
UpdateBLGridNodes();
PanelAddNode.Visible:=false;
End;

// CLICK TO SHOW WALLET
procedure TForm1.ButtonWalletOnClick(Sender: TObject);
Begin
PanelData.Visible := false;ButtonConsola.Font.Style:=[];
PanelNetwork.Visible:= false;ButtonNodes.Font.Style:=[];
PanelWallet.Visible:= true;ButtonWallet.Font.Style:=[fsBOLD];
UpdateMyAddresses();
end;

// CLICK ON CHECKBOX CONNECT
Procedure TForm1.CheckBoxConnectOnChange(Sender: TObject);
Begin
if CheckBoxConnect.Checked then
   begin
   ProcessLines.Add('connect');
   ProcessLines.Add('listenon');
   end
else
   begin
   ProcessLines.Add('disconnect');
   ProcessLines.Add('listenoff');
   end;
End;

// CLICK ON CHECKBOX MINER
Procedure TForm1.CheckBoxMinerOnChange(Sender: TObject);
Begin
if CheckBoxMiner.Checked then ProcessLines.Add('mineron')
else ProcessLines.Add('mineroff');
End;

// CLICK TO DELETE AN EXISTING NODE
procedure TForm1.ButtonDeleteNodeOnClick(Sender: TObject);
Begin
if ((GridNodes.Row>=0) and (length(arraynodos)> 0))then
   begin
   ProcessLines.Add('deletenode '+IntToStr(GridNodes.Row));
   end;
End;

// CLICK TO ADD A NEW NODE
procedure TForm1.ButtonAddnodeOnClick(Sender: TObject);
Begin
PanelAddNode.Visible:=true;
EditAddnodeIP.SetFocus;
end;

// CLICK CANCEL ADD NEW NODE
Procedure TForm1.ButAddNodeCancelOnClick(Sender: TObject);
Begin
EditAddnodeIP.Text:='';
EditAddNodePort.Text:='';
PanelAddNode.Visible:=false;
End;

// CLICK PROCESS ADD NEW NODE
Procedure TForm1.ButAddNodeAddOnClick(Sender: TObject);
Begin
ProcessLines.Add('addnode '+EditAddnodeIP.Text+' '+EditAddNodePort.Text);
EditAddnodeIP.Text:='';
EditAddNodePort.Text:='';
PanelAddNode.Visible:=false;
End;

// CLICK TO DELETE AN EXISTING BLNODE
procedure TForm1.ButtonDeleteBLOnClick(Sender: TObject);
Begin
if ((GridBLNodes.Row>=0) and (length(ArrayBlacklisted)> 0))then
   begin
   ProcessLines.Add('deletebl '+IntToStr(GridBLNodes.Row));
   end;
end;

// CLICK TO CREATE A NEW ADDRESS
procedure TForm1.ButtonNewAddressOnClick(Sender: TObject);
Begin
ProcessLines.Add('NEWADDRESS')
end;

// COPY ADDRESS TO CLIPBOARD
procedure TForm1.ButtonCopyAddressOnClick(Sender: TObject);
Begin
Clipboard.AsText:= Copy(GridAddresses.Cells[0,GridAddresses.row],1,35);
End;

// PASTE FROM CLIPBOARD
Procedure TForm1.ButtonPasteDestOnClick(Sender: TObject);
Begin
EditSFDesti.Text:=Clipboard.AsText;
End;

// CLICK TO SET THE MAXIMUM AVAILABLE TO SEND
Procedure TForm1.ButtonMaxAmoOnClick(Sender: TObject);
var
  available : int64;
  Cadena : string;
  CadLen : integer;
Begin
Available := GetTotalAccountBalance()-GetTotalAccountPendingPayments();
available := Available - GetComisionValue(MAIN_AccountBalance);
cadena := IntToStr(Available);
CadLen := Length(cadena);
Cadena := copy(cadena,1,CadLen-2)+','+copy(Cadena,CadLen-1,2);
EditSFAmount.Text:=Cadena;
End;

// CLICK TO SEND FUNDS
Procedure TForm1.ButtonSendFundsOnClick(Sender: TObject);
Begin
ProcessLines.Add('SENDTO '+EditSFDesti.Text+' '+EditSFAmount.Text+' '+EditConcept.text);
End;

// CLICK TO SHOW TRX DETAILS
Procedure TForm1.ButtonDetailsOnClick(Sender: TObject);
var
  DetString, trfrmsg : String;
  trxtipo, amount, blockN,sendera,receiver,trxhash,timestamp , concepto: String;
Begin
if GrisUserTrxs.Row > 0 then
   begin
   blockN := GrisUserTrxs.Cells[0,GrisUserTrxs.Row];
   amount := GrisUserTrxs.Cells[1,GrisUserTrxs.Row];
   trxhash := GrisUserTrxs.Cells[2,GrisUserTrxs.Row];
   trxtipo := GrisUserTrxs.Cells[3,GrisUserTrxs.Row];
   sendera := GrisUserTrxs.Cells[4,GrisUserTrxs.Row];
   receiver := GrisUserTrxs.Cells[5,GrisUserTrxs.Row];
   timestamp:= GrisUserTrxs.Cells[6,GrisUserTrxs.Row];
   concepto := GrisUserTrxs.Cells[7,GrisUserTrxs.Row];
   if trxtipo = 'MINE' then
      begin
      DetString:= 'Type  : Block Mined'+SLINEBREAK+
                  'Block : '+blockN+SLINEBREAK+
                  'Reward: '+amount+SLINEBREAK+
                  'Time  : '+TimestampToDate(TimeStamp);
      end;
   if trxtipo = 'TRFR' then
      begin
      if IsAddressMine(sendera) > -1 then trfrmsg := 'Transfer sent'
      else trfrmsg := 'Transfer received';
      DetString:= 'Type    : '+trfrmsg+SLINEBREAK+
                  'Block   : '+blockN+SLINEBREAK+
                  'Sender  : '+sendera+SLINEBREAK+
                  'Receiver: '+receiver+SLINEBREAK+
                  'Amount  : '+amount+SLINEBREAK+
                  'Concept : '+concepto+SLINEBREAK+
                  'Time    : '+TimestampToDate(TimeStamp)+SLINEBREAK+
                  'TrxID   : '+trxhash;
      end;
   ShowAlert(DetString);
   end;
End;

//DRAW GRIDADDRESSES
procedure TForm1.GridAddressesPrepareCanvas(sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
var
  ts: TTextStyle;
begin
if ARow=0 then GridAddresses.Canvas.Font.Style := [fsBold];
if (ACol = 1) and (aRow=0) then
   begin
   ts := GridAddresses.Canvas.TextStyle;
   ts.Alignment := taCenter;
   GridAddresses.Canvas.TextStyle := ts;
   end;
if (ACol = 1) and (aRow>0) then
   begin
   ts := GridAddresses.Canvas.TextStyle;
   ts.Alignment := taRightJustify;
   GridAddresses.Canvas.TextStyle := ts;
   end;
end;

//DRAW GRIDPENDING
Procedure TForm1.GridPendingPrepareCanvas(sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
var
  ts: TTextStyle;
Begin
if ARow=0 then GridPending.Canvas.Font.Style := [fsBold];
if (aRow=0) then
   begin
   ts := GridPending.Canvas.TextStyle;
   ts.Alignment := taCenter;
   GridPending.Canvas.TextStyle := ts;
   end;
if (aRow>0) then
   begin
   ts := GridPending.Canvas.TextStyle;
   ts.Alignment := taRightJustify;
   GridPending.Canvas.TextStyle := ts;
   if Copy(GridPending.Cells[0,aRow],1,1) = '-' then GridPending.Canvas.Font.Color:=clRed
   else GridPending.Canvas.Font.Color:=clGreen;
   end;
End;

//DRAW GRID USER TRXS
Procedure TForm1.GrisUserTrxsPrepareCanvas(sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
var
  ts: TTextStyle;
Begin
if ARow=0 then GrisUserTrxs.Canvas.Font.Style := [fsBold];
if ((aRow=0) and (aCol=0)) then
   begin
   ts := GrisUserTrxs.Canvas.TextStyle;
   ts.Alignment := taCenter;
   GrisUserTrxs.Canvas.TextStyle := ts;
   end;
if ((aRow=0) and (aCol=1)) then
   begin
   ts := GrisUserTrxs.Canvas.TextStyle;
   ts.Alignment := taRightJustify;
   GrisUserTrxs.Canvas.TextStyle := ts;
   end;
if ((aRow>0) and (acol=1)) then
   begin
   ts := GrisUserTrxs.Canvas.TextStyle;
   ts.Alignment := taRightJustify;
   GrisUserTrxs.Canvas.TextStyle := ts;
   if Copy(GrisUserTrxs.Cells[1,aRow],1,1) = '-' then GrisUserTrxs.Canvas.Font.Color:=clRed
   else GrisUserTrxs.Canvas.Font.Color:=clGreen;
   end;
if ((aRow>0) and (acol=0)) then
   begin
   ts := GrisUserTrxs.Canvas.TextStyle;
   ts.Alignment := taCenter;
   GrisUserTrxs.Canvas.TextStyle := ts;
   end;
End;

// DRAW GRID CONEXIONS
Procedure Tform1.GridConxsPrepareCanvas(sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
var
  ts: TTextStyle;
Begin
if ARow=0 then GridConxs.Canvas.Font.Style := [fsBold];
ts := GridConxs.Canvas.TextStyle;
ts.Alignment := taCenter;
GridConxs.Canvas.TextStyle := ts;
End;

// SHOW NOTIFY FORM
Procedure ShowAlert(Message:String);
Begin
MemoNotify.lines.Clear;
MemoNotify.lines.Add(Message);
FormNotify.Show;
sysutils.beep();
Form1.Enabled:=false;
end;

// CLICK TO CLOSE FORM NOTIFY
Procedure TForm1.ButtonCloseNotifyOnClick(Sender: TObject);
Begin
FormNotify.Close;
Form1.Enabled:=true;
Form1.BringToFront;
End;

// DOUBLE CLICK TRAY ICON TO RESTORE
Procedure TForm1.DoubleClickSysTray(Sender: TObject);
Begin
SysTrayIcon.visible:=false;
Form1.WindowState:=wsNormal;
Form1.Show;
MAIN_MinOnTray := false;
End;

// CHANGE USER LISTENING PORT
Procedure TForm1.EditUserPortOnEditingDone(Sender: TObject);
Begin
if not MAIN_ProgramStarted then exit;
ProcessLines.Add('SETPORT '+EditUserPort.Text);
End;

// CHANGE CPUS MINING
Procedure TForm1.ComboCPUMinerOnChange(Sender: TObject);
Begin
if not MAIN_ProgramStarted then exit;
ProcessLines.Add('CPUMINE '+ComboCPUMiner.Text);
End;

// CHANGE MINIMIZE TO TASK
Procedure TForm1.CheckBoxMinOnChange(Sender: TObject);
Begin
if not MAIN_ProgramStarted then exit;
if CheckBoxMin.Checked then ProcessLines.Add('MINTOTASK ON')
else ProcessLines.Add('MINTOTASK OFF');
End;

// CHANGE AUTOCONNECTION ON START UP
Procedure TForm1.CheckBoxAutoConnOnChange(Sender: TObject);
Begin
if not MAIN_ProgramStarted then exit;
if CheckBoxAutoConn.Checked then ProcessLines.Add('AUTOCONN ON')
else ProcessLines.Add('AUTOCONN OFF');
end;

// CHANGE FULL NODE MODE
Procedure TForm1.CheckBoxFullNodeOnChange(Sender: TObject);
Begin
if not MAIN_ProgramStarted then exit;
if CheckBoxFullNode.Checked then ProcessLines.Add('FULLNODE ON')
else ProcessLines.Add('FULLNODE OFF');
end;

// CHANGE UPDATE NODES FROM PEERS
Procedure TForm1.CheckBoxUpdateNodesOnChange(Sender: TObject);
Begin
if not MAIN_ProgramStarted then exit;
if CheckBoxUpdateNodes.Checked then ProcessLines.Add('UPDATENODES ON')
else ProcessLines.Add('UPDATENODES OFF');
end;

// CHANEG SHOW MINNED AS TRX
Procedure TForm1.CheckBoxMinnedOnChange(Sender: TObject);
Begin
if not MAIN_ProgramStarted then exit;
if CheckBoxMinned.Checked then ProcessLines.Add('SHOWMINNED ON')
else ProcessLines.Add('SHOWMINNED OFF');
End;

END. // END FORM

