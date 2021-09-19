program Demo;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {FormMain},
  Settings in 'Settings.pas' {FormSettings};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormSettings, FormSettings);
  Application.Run;
end.
