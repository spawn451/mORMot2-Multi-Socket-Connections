program Server;

uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  Unit1 in 'Unit1.pas' {Form1},
  uSimpleProtocol in '..\Common\uSimpleProtocol.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Carbon');
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end. 