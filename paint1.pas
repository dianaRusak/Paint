unit paint1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Menus, StdCtrls;

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
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure MenuItem4Click(Sender: TObject);
    procedure OnPaint();
    procedure PaintBox1Paint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  DownClick: Boolean; //флаг нажатой кнопки
  LineralArray: array of array of
  Tpoint;//во внешнем - линии, во внутреннем - точки линии

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.MenuItem4Click(Sender: TObject);
begin
  ShowMessage('Б8103а 2017 год. Русак Диана Игоревна');  //справка
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  with PaintBox1.Canvas do begin
    Brush.Color := clWhite;
    Pen.Width := 1;
    FillRect(0, 0, Width, Height);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  DownClick := False;
  with PaintBox1.Canvas do begin
    Brush.Color := clWhite;
    Pen.Width := 1;
  end;
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  DownClick := True;
  SetLength(LineralArray, Length(LineralArray) + 1);
  SetLength(LineralArray[0], Length(LineralArray[0]) + 1);
  LineralArray[Length(LineralArray)-1, Length(LineralArray[0])-1] := Point(x, y);
  Invalidate;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  if DownClick then begin
    Setlength(LineralArray[length(LineralArray)], length(LineralArray[length(LineralArray)]) + 1);
    LineralArray[Length(LineralArray) - 1, Length(LineralArray[Length(LineralArray) - 1]) - 1] := Point(x, y);
  end;
  Invalidate;
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  DownClick := False;
  Invalidate;
end;

procedure TForm1.OnPaint();
begin
  Paintbox1.canvas.polyline(LineralArray[1]);
end;

end.
