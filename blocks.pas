unit Blocks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Forms, MAster, TimeUnit, CM_Crypto, commandlineparser,Dialogs,
  fileutil;

function BuildNewBlock(blNumber:int64;TimeStamp,Account,Solution,NewBlHash, TargetHash,difficulty:String): boolean;
procedure SaveNewBlockToDisk(BlockHeader:BlockHeaderData;ArrBlockTxs: Array of TranxData;Solution, filename:string);
function GetDiffForNextBlock(block,ThisBlockTime:int64):String;
function DecreaseDiff(minediff:String;variation:int64):String;
function IncreaseDiff(minediff:String;variation:int64):String;
Function GetLast20Average(ThisBlockTime:int64):int64;
function GetBlockReward(BlNumber:int64):Int64;
procedure AddFundsToAddress(Address:String;Amount:Int64;block:int64);
procedure SetAddressPubKey(Address,PubKey:String;block:int64);
function GetNegativeValue(number:int64):int64;
procedure CreateBlockZero();
function UndoneLastBlock(BlNumber:int64 = -1):Boolean;
procedure LBData();

implementation

Uses
  MC_Main, Protocol;

// BUILDS A NEW BLOCK GetTimeStamp
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
           ThisTrxFee := GetComisionValue(StrToInt64(TrxData.Ammount));
           TrxData.Ammount := IntToStr(StrToInt64(TrxData.Ammount)-ThisTrxFee);
         MinerFee := MinerFee + ThisTrxFee;
         TransfersList.Add('TRFR '+TrxData.Receiver+' '+TrxData.Ammount);
         TransfersList.Add('TRFR '+TrxData.Sender+' '+IntToStr(GetNegativeValue(StrToInt64(TrxData.Ammount)+ThisTrxFee)));
         SetLength(ArrBlockTxs,Length(ArrBlockTxs)+1);
         ArrBlockTxs[Length(ArrBlockTxs)-1] := TrxData;
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

// SHOWS LAST BLOCK DATA
Procedure LBData();
Begin
// to be implemented
end;

END. // END UNIT

