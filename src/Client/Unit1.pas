unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, 
  Vcl.StdCtrls, Vcl.ExtCtrls,Vcl.Imaging.jpeg,System.ZLib,
  mormot.core.base,
  mormot.core.log,
  mormot.core.rtti,
  mormot.net.sock,
  mormot.net.async,
  uSimpleProtocol;

const
  // Connection parameters
  SERVER_IP = 'localhost';
  SERVER_PORT = '3434';
  CONNECTION_COUNT = 1;
  CONNECTION_TIMEOUT = 10; // seconds
  PROCESS_NAME = 'MultiSocketClient';
  THREAD_POOL_COUNT = 1;

type
  // Custom client connection class
  TConnection = class(TAsyncConnection)
  protected
    function OnRead: TPollAsyncSocketOnReadWrite; override;
    procedure AfterCreate; override; // Called after connection is created
    procedure OnClose; override; // Called when connection is closed
     private
     fNickName: string;
     procedure ProcessCommand(const Data: TBytes);
     procedure SendProtocolMessage(const Message: RawByteString);
  public
    procedure SendData(const Data: TBytes);
    property NickName: string read fNickName write fNickName;
  end;

  // Main client form with auto-reconnection
  TForm1 = class(TForm)
    ReconnectTimer: TTimer;
    LogMemo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ReconnectTimerTimer(Sender: TObject);
  private
    fClient: TAsyncClient;
    fConnection: TConnection;
    fLogFamily: TSynLogFamily;
    fIsConnected: Boolean;
    fReconnectAttempts: Integer;
    procedure ConnectToServer;
    procedure AddLogMessage(const Msg: string);
  public
    procedure OnConnectionLost;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function CaptureScreenshot: TBytes;
var
  DC: HDC; // Device context for screen
  ScreenBMP: TBitmap; // Bitmap for screen capture
  jpgImage: TJPEGImage; // JPEG for compression
  MS, CompressedMS: TMemoryStream; // Memory streams for processing
  CompressionStream: TZCompressionStream; // Compression stream
begin
  Result := nil;

  // Create required objects for screen capture
  ScreenBMP := TBitmap.Create;
  jpgImage := TJPEGImage.Create;
  MS := TMemoryStream.Create;
  CompressedMS := TMemoryStream.Create;
  try
    // Set bitmap dimensions to match screen resolution
    ScreenBMP.Width := Screen.Width;
    ScreenBMP.Height := Screen.Height;

    // Get device context for the entire screen (0 = desktop window)
    DC := GetDC(0);
    try
      // Copy screen pixels to bitmap using BitBlt
      // SRCCOPY = direct copy of source pixels
      BitBlt(ScreenBMP.Canvas.Handle, 0, 0, Screen.Width,
        Screen.Height, DC, 0, 0, SRCCOPY);
    finally
      // Always release the device context
      ReleaseDC(0, DC);
    end;

    // Convert to JPEG with 30% quality (high compression)
    jpgImage.Assign(ScreenBMP);
    jpgImage.CompressionQuality := 30;
    jpgImage.SaveToStream(MS);

    // Apply additional zlib compression
    MS.Position := 0;
    CompressionStream := TZCompressionStream.Create(CompressedMS);
    try
      CompressionStream.CopyFrom(MS, MS.Size);
    finally
      CompressionStream.Free;
    end;

    // Prepare compressed data for return
    CompressedMS.Position := 0;
    SetLength(Result, CompressedMS.Size);
    if CompressedMS.Size > 0 then
      CompressedMS.ReadBuffer(Result[0], CompressedMS.Size);

  finally
    // Clean up resources
    ScreenBMP.Free;
    jpgImage.Free;
    MS.Free;
    CompressedMS.Free;
  end;
end;

{ TConnection }

procedure TConnection.AfterCreate;
begin
  inherited AfterCreate;
  
  // Generate nickname and send identification
  fNickName := 'Client' + IntToStr(GetTickCount64);
  
  // Send client identification using protocol message
  SendData(BytesOf('NewCon|' + fNickName));

  // Update UI
  if Assigned(Form1) then
  begin
    Form1.fConnection := self;
    Form1.fIsConnected := True;
    Form1.fReconnectAttempts := 0;
    Form1.ReconnectTimer.Enabled := False; // Stop reconnection timer
    Form1.Caption := 'mORMot2 - Client - Connected as: ' + fNickName;
    Form1.AddLogMessage('Connection established successfully');
    Form1.AddLogMessage('Nickname: ' + fNickName);
    Form1.AddLogMessage('Registration sent to server');
  end;
  
  TSynLog.Add.Log(sllInfo, 'Client connected as: %', [fNickName]);
end;

procedure TConnection.OnClose;
begin
  // Update UI on disconnection
  if Assigned(Form1) then
  begin
    Form1.fConnection := nil;
    Form1.fIsConnected := False;
    Form1.AddLogMessage('Connection closed');
    Form1.OnConnectionLost; // Trigger reconnection logic
  end;
  
  TSynLog.Add.Log(sllInfo, 'Client disconnected');
  inherited OnClose;
end;

function TConnection.OnRead: TPollAsyncSocketOnReadWrite;
var
  bufferData: RawByteString;
  data: TBytes;
  messageSize: UInt32;
begin
  result := soContinue;
  
  try
    // Process all available data in the buffer
    while fRd.Len > 0 do
    begin
      // Convert buffer to string for processing
      SetString(bufferData, PAnsiChar(fRd.Buffer), fRd.Len);
      
      // Check if buffer starts with our magic marker (protocol message)
      if TSimpleProtocol.HasMagicMarker(bufferData) then
      begin
        // Try to parse protocol message
        if TSimpleProtocol.TryParseMessage(bufferData, data) then
        begin
          // Calculate total message size and remove from buffer
          messageSize := SizeOf(TSimpleMessage) + Length(data);
          fRd.Remove(messageSize);
          
          // Process protocol command
          ProcessCommand(data);
        end
        else
        begin
          // Incomplete protocol message, wait for more data
          break;
        end;
      end
      else
      begin
        // Invalid data - not our protocol, close connection
        if Assigned(Form1) then
          Form1.AddLogMessage('Invalid protocol data received from server');
        TSynLog.Add.Log(sllWarning, 'Invalid protocol data received from server');
        result := soClose;
        break;
      end;
    end;
    
  except
    on E: Exception do
    begin
      if Assigned(Form1) then
        Form1.AddLogMessage('OnRead error: ' + E.Message);
      TSynLog.Add.Log(sllWarning, 'OnRead error: %', [E.Message]);
      result := soClose;
    end;
  end;
end;

procedure TConnection.ProcessCommand(const Data: TBytes);
var
  sl: TStringList;
  ScreenshotData: TBytes;
begin
  sl := TStringList.Create;
  try
    sl.Delimiter := '|';
    sl.StrictDelimiter := True;
    sl.DelimitedText := StringOf(Data);

    if sl.Count > 0 then
    begin

      if sl[0] = 'All' then
      begin
        Form1.AddLogMessage('Message to all: ' + sl[1]);
      end;

      if sl[0] = 'Selected' then
      begin
        Form1.AddLogMessage('Message to me: ' + sl[1]);
      end;

      if sl[0] = 'GetScreenShot' then
      begin
        TThread.Queue(nil,
          procedure
          var
            ScreenshotData: TBytes;
          begin
            // Capture screenshot
            ScreenshotData := CaptureScreenshot;

            if Length(ScreenshotData) > 0 then
            begin
            // Send compressed image to server
            SendData(BytesOf('ScreenShot|') + ScreenshotData);
            end;
          end);
      end
    end;

  finally
    sl.Free;
  end;
end;

procedure TConnection.SendProtocolMessage(const Message: RawByteString);
begin
  fOwner.WriteString(self, Message);
  if Assigned(Form1) then
    Form1.AddLogMessage('Sent protocol message: ' + IntToStr(Length(Message)) + ' bytes');
end;

procedure TConnection.SendData(const Data: TBytes);
begin
  TSimpleProtocol.SendMessage(SendProtocolMessage, Data);
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  fIsConnected := False;
  fReconnectAttempts := 0;
  
  // Configure logging
  fLogFamily := TSynLog.Family;
  fLogFamily.Level := LOG_VERBOSE;
  fLogFamily.PerThreadLog := ptIdentifiedInOnFile;
  fLogFamily.EchoToConsole := LOG_VERBOSE;
  
  // Configure reconnection timer
  ReconnectTimer.Interval := 3000; // 3 seconds
  ReconnectTimer.Enabled := False;
  
  AddLogMessage('=== mORMot2 Client Started ===');
  AddLogMessage('Target Server: ' + SERVER_IP + ':' + SERVER_PORT);
  AddLogMessage('');

  AddLogMessage('Starting initial connection attempt...');
  
  // Start initial connection attempt and enable timer to keep trying
  ConnectToServer;
  
  // Always start the reconnection timer - it will handle both initial connection and reconnections
  ReconnectTimer.Enabled := True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ReconnectTimer.Enabled := False;
  if Assigned(fClient) then
  begin
    fClient.Free;
    fClient := nil;
  end;
end;

procedure TForm1.ConnectToServer;
begin
  try
    // Free existing client if any
    if Assigned(fClient) then
    begin
      fClient.Free;
      fClient := nil;
    end;
    
    Inc(fReconnectAttempts);

    AddLogMessage('Connection attempt #' + IntToStr(fReconnectAttempts));
    
    // Create client
    fClient := TAsyncClient.Create(
      SERVER_IP,                      // Server IP
      SERVER_PORT,                    // Server port
      CONNECTION_COUNT,               // Number of connections
      CONNECTION_TIMEOUT,             // Connection timeout in seconds
      nil, nil,                       // OnStart, OnStop callbacks
      TConnection,                    // Connection class
      PROCESS_NAME,                   // Process name
      fLogFamily.SynLogClass,         // Log class
      [],                             // Options
      THREAD_POOL_COUNT               // Thread pool count
    );

    AddLogMessage('TAsyncClient created successfully');

      except
      on E: Exception do
      begin
        AddLogMessage('Connection attempt failed: ' + E.Message);
        TSynLog.Add.Log(sllWarning, 'Connection attempt %d failed: %', [fReconnectAttempts, E.Message]);
      end;
    end;
end;

procedure TForm1.OnConnectionLost;
begin
  AddLogMessage('Connection lost detected');
  TSynLog.Add.Log(sllWarning, 'Connection lost, starting reconnection attempts');
  
  // Start reconnection timer
  if not ReconnectTimer.Enabled then
  begin
    fReconnectAttempts := 0; // Reset counter for new reconnection cycle
    AddLogMessage('Starting reconnection cycle...');
    ReconnectTimer.Enabled := True;
  end;
end;

procedure TForm1.ReconnectTimerTimer(Sender: TObject);
begin
  if not fIsConnected then
  begin
    AddLogMessage('Reconnection timer triggered');
    ConnectToServer;
  end
  else
  begin
    // Connected, stop the timer
    AddLogMessage('Connected - stopping reconnection timer');
    ReconnectTimer.Enabled := False;
  end;
end;

procedure TForm1.AddLogMessage(const Msg: string);
var
  TimeStr: string;
begin
  TimeStr := FormatDateTime('hh:nn:ss.zzz', Now);
  LogMemo.Lines.Add('[' + TimeStr + '] ' + Msg);
  
  // Auto-scroll to bottom
  LogMemo.SelStart := Length(LogMemo.Text);
  LogMemo.SelLength := 0;
  SendMessage(LogMemo.Handle, EM_SCROLLCARET, 0, 0);
  
  Application.ProcessMessages;
end;

end.