unit uSimpleProtocol;

interface

uses
  System.SysUtils, System.Classes,
  mormot.core.base,
  mormot.core.log,
  mormot.net.async;

const
  // Magic marker for our protocol messages
  PROTOCOL_MAGIC = $CAFEBABE;

type
  // Simplified message structure (no command ID)
  TSimpleMessage = packed record
    Magic: UInt32;        // Magic marker
    DataSize: UInt32;     // Size of data that follows
    // Data follows after this header
  end;

  // Send callback function type
  TSendMessageCallback = procedure(const Message: RawByteString) of object;

  // Helper class for protocol operations (Netcom7-style)
  TSimpleProtocol = class
  public
    // Pack a message with magic marker (TBytes output)
    class function PackMessage(const Data: TBytes): RawByteString;
    
    // Send a protocol message via callback (TBytes input)
    class procedure SendMessage(SendCallback: TSendMessageCallback; const Data: TBytes);
    
    // Try to parse protocol message from buffer (TBytes output)
    class function TryParseMessage(const Buffer: RawByteString; out Data: TBytes): Boolean;
    
    // Check if buffer starts with our magic marker
    class function HasMagicMarker(const Buffer: RawByteString): Boolean;

  end;

implementation

{ TSimpleProtocol }

class function TSimpleProtocol.PackMessage(const Data: TBytes): RawByteString;
var
  Header: TSimpleMessage;
begin
  Header.Magic := PROTOCOL_MAGIC;
  Header.DataSize := Length(Data);
  
  SetLength(Result, SizeOf(Header) + Length(Data));
  Move(Header, Result[1], SizeOf(Header));
  if Length(Data) > 0 then
    Move(Data[0], Result[SizeOf(Header) + 1], Length(Data));
end;

class procedure TSimpleProtocol.SendMessage(SendCallback: TSendMessageCallback; const Data: TBytes);
var
  Message: RawByteString;
begin
  Message := PackMessage(Data);
  if Assigned(SendCallback) then
    SendCallback(Message);
end;

class function TSimpleProtocol.TryParseMessage(const Buffer: RawByteString; out Data: TBytes): Boolean;
var
  Header: TSimpleMessage;
begin
  Result := False;
  SetLength(Data, 0);
  
  // Check minimum size for header
  if Length(Buffer) < SizeOf(Header) then
    Exit;
    
  // Extract header
  Move(Buffer[1], Header, SizeOf(Header));
  
  // Check magic marker
  if Header.Magic <> PROTOCOL_MAGIC then
    Exit;
    
  // Check if we have all the data
  if Length(Buffer) < SizeOf(Header) + Header.DataSize then
    Exit;
    
  // Extract data
  if Header.DataSize > 0 then
  begin
    SetLength(Data, Header.DataSize);
    Move(Buffer[SizeOf(Header) + 1], Data[0], Header.DataSize);
  end;
  
  Result := True;
end;

class function TSimpleProtocol.HasMagicMarker(const Buffer: RawByteString): Boolean;
begin
  Result := (Length(Buffer) >= 4) and (PUInt32(@Buffer[1])^ = PROTOCOL_MAGIC);
end;



end. 