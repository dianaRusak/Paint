program Paint;

{$mode objfpc}{$H+}

uses
        {$IFDEF UNIX}{$IFDEF UseCThreads}
        cthreads,
        {$ENDIF}{$ENDIF}
        Interfaces, // this includes the LCL widgetset
        Forms, paint1;

{$R *.res}

begin
        RequireDerivedFormResource:=True;
        Application.Initialize;
        Application.CreateForm(TForm1, Form1);
        Application.Run;
end.
