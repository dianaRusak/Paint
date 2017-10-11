unit paint1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Menus, StdCtrls; //painted;

type

  { TForm1 }

  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    PaintBox1: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure FormWhite();
    procedure MenuItem4Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.MenuItem4Click(Sender: TObject);
begin
  ShowMessage('Б8103а 2017 год. Русак Диана Игоревна');  //справка
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FormWhite();
end;

procedure TForm1.FormWhite();
begin
  PaintBox1.Canvas.Brush.Color := clWindow;
  PaintBox1.Canvas.FillRect(PaintBox1.Canvas.ClipRect);
end;

end.

