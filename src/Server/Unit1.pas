unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, 
  System.Generics.Collections, System.ZLib, Vcl.Forms, Vcl.ComCtrls, Vcl.Controls,
  Vcl.Menus, Vcl.ExtCtrls, Vcl.Graphics, SyncObjs, Vcl.Imaging.jpeg,
  mormot.core.base,
  mormot.core.log,
  mormot.core.rtti,
  mormot.net.sock,
  mormot.net.async,
  uSimpleProtocol;

const
  // Server configuration parameters
  SERVER_PORT = '3434';
  PROCESS_NAME = 'MultiSocketServer';
  THREAD_POOL_COUNT = 4;
  STARTUP_TIMEOUT = 10; // seconds

type
  // Client data structure (equivalent to TConnectedUserData)
  TConnectedUserData = class
    Line: TAsyncConnection;
    ID: string;
  end;

  // Custom client connection class
  TConnection = class(TAsyncConnection)
  protected
    function OnRead: TPollAsyncSocketOnReadWrite; override;
    procedure AfterCreate; override; // Called after connection is created
    procedure OnClose; override; // Called when connection is closed
  private
    fNickName: string;
    procedure ProcessCommand(const Data: TBytes);
    function GetRemoteIPDisplay: RawUtf8;
    procedure SendProtocolMessage(const Message: RawByteString);
  public
    constructor Create(aOwner: TAsyncConnections; const aRemoteIP: TNetAddr); override;
    procedure SendData(const Data: TBytes);
    property NickName: string read fNickName write fNickName;
    property RemoteIPDisplay: RawUtf8 read GetRemoteIPDisplay;
  end;

  // Main server form
  TForm1 = class(TForm)
    StatusBar1: TStatusBar;
    ListView1: TListView;
    PopupMenu1: TPopupMenu;
    S1: TMenuItem;
    S2: TMenuItem;
    G1: TMenuItem;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure S1Click(Sender: TObject);
    procedure S2Click(Sender: TObject);
    procedure G1Click(Sender: TObject);
  private
    fServer: TAsyncServer;
    fLogFamily: TSynLogFamily;
    fClients: TObjectDictionary<TPollAsyncConnectionHandle, TConnectedUserData>;
    fConnectedUsersLock: TCriticalSection;
    fconnectedclients: Integer;
    fConnectedUsers: TStringList;
  private
    function GetSelectedClientID: TPollAsyncConnectionHandle;
  public
    procedure SendMessageToAll(const Data: TBytes);
    procedure SendMessageToSelectedClient(ClientID: TPollAsyncConnectionHandle; const Data: TBytes);
    procedure DisplayScreenShot(const aData: TBytes);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TConnection }

constructor TConnection.Create(aOwner: TAsyncConnections; const aRemoteIP: TNetAddr);
begin
  inherited Create(aOwner, aRemoteIP);
  // Override the RemoteIP to show localhost properly
  if fRemoteIP = '' then
    aRemoteIP.IP(fRemoteIP, {localasvoid=}false); // Show 127.0.0.1 instead of empty
end;

function TConnection.GetRemoteIPDisplay: RawUtf8;
begin
  if fRemoteIP = '' then
    Result := '127.0.0.1' // Fallback for localhost
  else
    Result := fRemoteIP;
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
        TSynLog.Add.Log(sllWarning, 'Invalid protocol data received from %', [RemoteIPDisplay]);
        result := soClose;
        break;
      end;
    end;
    
  except
    on E: Exception do
    begin
      TSynLog.Add.Log(sllWarning, 'OnRead error for %: %', [RemoteIPDisplay, E.Message]);
      result := soClose;
    end;
  end;
end;

procedure TConnection.AfterCreate;
var
  UserData: TConnectedUserData;
begin
  inherited AfterCreate;
  
  // Register connection immediately using Handle as unique identifier
  // Nickname will be set later via NewCon|NickName message
  
  if Assigned(Form1) then
  begin
    Form1.fConnectedUsersLock.Enter;
    try
      // Create user data structure using Handle as key
      UserData := TConnectedUserData.Create;
      UserData.Line := self;
      UserData.ID := ''; // Will be set when we get nickname
      
      // Add to collections using Handle as unique key
      Form1.fClients.AddOrSetValue(Handle, UserData);
      
      // Update connection count
      Inc(Form1.fconnectedclients);
      Form1.StatusBar1.Panels[1].Text := 'Connections: ' + IntToStr(Form1.fconnectedclients);
      
    finally
      Form1.fConnectedUsersLock.Leave;
    end;
  end;
  
  TSynLog.Add.Log(sllInfo, 'Client connected from % (Handle=%)', [RemoteIPDisplay, Handle]);
end;

procedure TConnection.OnClose;
var
  I: Integer;
  UserData: TConnectedUserData;
  clientid: string;
begin
  // Connection closing - handle disconnection based on Handle
  if Assigned(Form1) then
  begin
    Form1.fConnectedUsersLock.Enter;
    try
      clientid := fNickName;
      
      // Remove from clients dictionary using Handle as key
      if Form1.fClients.TryGetValue(Handle, UserData) then
      begin
        Form1.fClients.Remove(Handle);
        
        // Remove from fConnectedUsers list
        for I := Form1.fConnectedUsers.Count - 1 downto 0 do
        begin
          if Form1.fConnectedUsers.Objects[I] = UserData then
          begin
            Form1.fConnectedUsers.Delete(I);
            Break;
          end;
        end;
      end;
      
      // Remove from ListView using Handle stored in Data
      for I := Form1.ListView1.Items.Count - 1 downto 0 do
      begin
        if TPollAsyncConnectionHandle(Form1.ListView1.Items[I].Data) = Handle then
        begin
          Form1.ListView1.Items.Delete(I);
          Break;
        end;
      end;
      
      // Update connection count
      Dec(Form1.fconnectedclients);
      Form1.StatusBar1.Panels[1].Text := 'Connections: ' + IntToStr(Form1.fconnectedclients);
      
      TSynLog.Add.Log(sllInfo, 'Client disconnected: % from % (Handle=%)', 
        [clientid, RemoteIPDisplay, Handle]);
    finally
      Form1.fConnectedUsersLock.Leave;
    end;
  end;
  
  inherited OnClose;
end;

procedure TConnection.ProcessCommand(const Data: TBytes);
var
  sl: TStringList;
  UserData: TConnectedUserData;
  li: TListItem;
begin
  sl := TStringList.Create;
  try
    sl.Delimiter := '|';
    sl.StrictDelimiter := True;
    sl.DelimitedText := StringOf(Data);
    
    if sl.Count > 0 then
    begin
      if sl[0] = 'NewCon' then
      begin
        // NewCon|NickName format
        if sl.Count > 1 then
        begin
          fNickName := sl[1];
          
          // Update the client data and ListView
          if Assigned(Form1) then
          begin
            Form1.fConnectedUsersLock.Enter;
            try
              if Form1.fClients.TryGetValue(Handle, UserData) then
              begin
                UserData.ID := fNickName;
                
                li := Form1.ListView1.Items.Add;
                li.Caption := string(RemoteIPDisplay);
                li.SubItems.Add(fNickName);
                li.Data := Pointer(Handle);
                
                Form1.fConnectedUsers.AddObject(fNickName, UserData);
                
                TSynLog.Add.Log(sllInfo, 'Client registered: % from % (Handle=%)',
                  [fNickName, RemoteIPDisplay, Handle]);
              end;
            finally
              Form1.fConnectedUsersLock.Leave;
            end;
          end;
        end;
      end;

      if sl[0] = 'ScreenShot' then
      begin
        TThread.Queue(nil,
          procedure
          var
            imageData: TBytes;
          begin
            try
              imageData := Copy(Data, 11, Length(Data));
              Form1.DisplayScreenShot(imageData);
            except
              on E: Exception do
              begin
                TSynLog.Add.Log(sllError, 'Error processing screenshot from %: %', [RemoteIPDisplay, E.Message]);
              end;
            end;
          end);
      end

     //...
    end;
    
  finally
    sl.Free;
  end;
end;

procedure TConnection.SendProtocolMessage(const Message: RawByteString);
begin
  fOwner.WriteString(self, Message);
end;

procedure TConnection.SendData(const Data: TBytes);
begin
  TSimpleProtocol.SendMessage(SendProtocolMessage, Data);
end;

{ TForm1 }

procedure TForm1.DisplayScreenShot(const aData: TBytes);
var
  CompressedMS, DecompressedMS: TMemoryStream;
  DecompressionStream: TZDecompressionStream;
  jpgImage: TJPEGImage;
begin
  // Create streams for decompression
  CompressedMS := TMemoryStream.Create;
  DecompressedMS := TMemoryStream.Create;
  jpgImage := TJPEGImage.Create;
  try
    // Load compressed data into stream
    CompressedMS.WriteBuffer(aData[0], Length(aData));
    CompressedMS.Position := 0;

    // Decompress zlib data
    DecompressionStream := TZDecompressionStream.Create(CompressedMS);
    try
      DecompressedMS.CopyFrom(DecompressionStream, 0);
    finally
      DecompressionStream.Free;
    end;

    // Load JPEG from decompressed data
    DecompressedMS.Position := 0;
    jpgImage.LoadFromStream(DecompressedMS);

    // Display in TImage component
    if Assigned(Form1) and Assigned(Form1.Image1) then
    begin
      Form1.Image1.Picture.Assign(jpgImage);
      Form1.Image1.Stretch := True; // Auto-fit to component size
    end;

  finally
    CompressedMS.Free;
    DecompressedMS.Free;
    jpgImage.Free;
  end;
end;

function TForm1.GetSelectedClientID: TPollAsyncConnectionHandle;
begin
  if (ListView1.Selected <> nil) then
    Result := TPollAsyncConnectionHandle(ListView1.Selected.Data)
  else
    Result := 0; // Invalid ClientID when nothing selected
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  fClients := TObjectDictionary<TPollAsyncConnectionHandle, TConnectedUserData>.Create([doOwnsValues]);
  fConnectedUsers := TStringList.Create;
  fConnectedUsersLock := TCriticalSection.Create;
  fconnectedclients := 0;
  
  // Configure logging
  fLogFamily := TSynLog.Family;
  fLogFamily.Level := LOG_VERBOSE;
  fLogFamily.PerThreadLog := ptIdentifiedInOnFile;
  fLogFamily.EchoToConsole := LOG_VERBOSE;


  // Create and start server
  fServer := TAsyncServer.Create(
    SERVER_PORT,                      // Port
    nil, nil,                         // OnStart, OnStop callbacks
    TConnection,                      // Our connection class
    PROCESS_NAME,                     // Process name
    fLogFamily.SynLogClass,           // Log class
    [],                               // Options
    THREAD_POOL_COUNT                 // Thread pool count
  );

  try
    fServer.WaitStarted(STARTUP_TIMEOUT);
    StatusBar1.Panels[0].Text := 'Status: Active';
    TSynLog.Add.Log(sllInfo, 'Multi-Socket Server started on port %', [SERVER_PORT]);
  except
    on E: Exception do
    begin
      StatusBar1.Panels[0].Text := 'Status: Failed to start - ' + E.Message;
      TSynLog.Add.Log(sllError, 'Failed to start server: %', [E.Message]);
    end;
  end;
end;

procedure TForm1.FormDestroy;
begin
  fServer.Free;
  fClients.Free;
  fConnectedUsers.Free;
  fConnectedUsersLock.Free;
end;

procedure TForm1.SendMessageToAll(const Data: TBytes);
var
  UserData: TConnectedUserData;
begin
  fConnectedUsersLock.Enter;
  try
    for UserData in fClients.Values do
    begin
      TConnection(UserData.Line).SendData(Data);
    end;
  finally
    fConnectedUsersLock.Leave;
  end;
end;

procedure TForm1.SendMessageToSelectedClient(ClientID: TPollAsyncConnectionHandle; const Data: TBytes);
var
  UserData: TConnectedUserData;
begin
  fConnectedUsersLock.Enter;
  try
    if fClients.TryGetValue(ClientID, UserData) then
    begin
      TConnection(UserData.Line).SendData(Data);
    end;
  finally
    fConnectedUsersLock.Leave;
  end;
end;

procedure TForm1.S1Click(Sender: TObject);
begin
  SendMessageToAll(BytesOf('All|' + 'Hello ALL'));
end;

procedure TForm1.S2Click(Sender: TObject);
var
  ClientID: TPollAsyncConnectionHandle;
begin
  ClientID := GetSelectedClientID;
  if ClientID <> 0 then
  begin
    SendMessageToSelectedClient(ClientID, BytesOf('Selected|' + 'MSG JUST TO YOU!'));
  end;
end;

procedure TForm1.G1Click(Sender: TObject);
var
  ClientID: TPollAsyncConnectionHandle;
begin
  ClientID := GetSelectedClientID;
  if ClientID <> 0 then
  begin
    SendMessageToSelectedClient(ClientID, BytesOf('GetScreenShot'));
  end;
end;


end. 