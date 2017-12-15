program Wannabe;

{$MODE OBJFPC}
{$LONGSTRINGS ON}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Main, superobject
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;

  Application.Initialize();

  Application.CreateForm(TMainForm, MainForm);

  Application.Run();
end.

