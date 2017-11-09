program Wannabe;

{$MODE OBJFPC}
{$LONGSTRINGS ON}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Main,
  { you can add units after this }
  EditorTools, CanvasFigures;

{$R *.res}

begin
  RequireDerivedFormResource := True;

  Application.Initialize();

  Application.CreateForm(TMainForm, MainForm);

  Application.Run();
end.

