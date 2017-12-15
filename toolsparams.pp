unit ToolsParams;

{$mode objfpc}{$H+}

interface

uses
   Classes, SysUtils,Graphics, Controls, ComCtrls, StdCtrls, ColorBox, ExtCtrls;

type

TToolParam = class
  private
    FName:string;
    procedure ChangeControl(Sender: TObject); virtual; abstract;
  public
    property Name: string read FName;
  function ToControl(AParentPanel: TPanel):TControl; virtual; abstract;
end;

TToolParamsList = array of TToolParam;

{TColorLineParam }

TColorLineParam = class(TToolParam)
  private
    fColorLine: TColor;
    procedure ChangeControl(Sender: TObject); override;
  public
    property Value: TColor read fColorLine;
    constructor Create;
    function ToControl(AParentPanel: TPanel): TControl; override;
end;

{ TColorBrushParam }

TColorBrushParam = class(TToolParam)
  private
    fColorBrush: TColor;
    procedure ChangeControl(Sender: TObject); override;
  public
    property Value: TColor read fColorBrush;
    constructor Create;
    function ToControl(AParentPanel: TPanel): TControl; override;
end;


{ TFillStyleParam }

TFillStyleParam = class(TToolParam)
  private
    FFillIndex: integer;
    const FFillStyles: array[0..7] of TBrushStyle = (bsSolid, bsClear,
    bsHorizontal, bsVertical, bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross);
    function fGetFillStyle: TBrushStyle;
    procedure ChangeControl(Sender: TObject); override;
    procedure fDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
      State: TOwnerDrawState);
public
  property Value: TBrushStyle read fGetFillStyle;
  constructor Create;
  function ToControl(AParentPanel: TPanel): TControl; override;
end;

{ TWidthLineParam }

TWidthLineParam = class(TToolParam)
private
  fWidthLine: TWidth;
  procedure ChangeControl(Sender: TObject); override;
public
  property Value: TWidth read fWidthLine;
  constructor Create;
  function ToControl(AParentPanel: TPanel): TControl; override;
end;

{ TStyleLineParam }
TStyleLineParam = class(TToolParam)
  private
    FLineIndex: integer;
    const fStylesLine: array[0..5] of TPenStyle = (psSolid, psClear, psDot, psDash,
      psDashDot, psDashDotDot);
  {
    'Сплошной'; psSolid),
    'Прерывистый'; psDash),
    'Точечный'; psDot),
    'Точка-тире'; psDashDot),
    'Точка-точка-тире' psDashDotDot)
  }
    function fGetLineStyle: TPenStyle;
    procedure ChangeControl(Sender: TObject); override;
    procedure FDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
  public
    property Value: TPenStyle read fGetLineStyle;
    constructor Create;
    function ToControl(AParentPanel: TPanel): TControl; override;
end;

{ TRadiusParam }

TRadiusParam = class(TToolParam)
  private
    fRadius: integer;
    procedure ChangeControl(Sender: TObject); override;
  public
    property Value: integer read fRadius;
    constructor Create;
    function ToControl(AParentPanel: TPanel): TControl; override;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////

implementation

////////////////////////////////////////////////////////////////////////////////////////////////////
//TColorBrushParam
////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TColorBrushParam.ChangeControl(Sender: TObject);
begin
  fColorBrush := (Sender as TColorBox).Selected;
end;

constructor TColorBrushParam.Create;
begin
  fName := 'Color of Brush';
  fColorBrush := clBlack;
end;

function TColorBrushParam.ToControl(AParentPanel: TPanel): TControl;
begin
  Result := TColorBox.Create(AParentPanel);
  with Result as TColorBox do
  begin
    Parent := AParentPanel;
    ColorRectWidth := 10;
    Style := [cbCustomColor, cbExtendedColors, cbPrettyNames, cbStandardColors];
    Selected := fColorBrush;
    OnSelect := @ChangeControl;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
//TFillStyleParam
////////////////////////////////////////////////////////////////////////////////////////////////////

function TFillStyleParam.fGetFillStyle: TBrushStyle;
begin
  Result := FFillStyles[FFillIndex];
end;

procedure TFillStyleParam.ChangeControl(Sender: TObject);
begin
  FFillIndex := (Sender as TComboBox).ItemIndex;
end;

procedure TFillStyleParam.fDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
begin
  with (Control as TComboBox).Canvas do
  begin
    FillRect(ARect);
    Pen.Color := clBlack;
    Pen.Style := psClear;
    Pen.Width := 1;
    Brush.Style := FFillStyles[Index];
    if Brush.Style <> bsClear then
      Brush.Color := clBlack;
    Rectangle(ARect.Left + 1, ARect.Top + 1, ARect.Right - 1,ARect.Bottom - 1);
  end;
end;

constructor TFillStyleParam.Create;
begin
  FName := 'Fill style';
end;

function TFillStyleParam.ToControl(AParentPanel: TPanel): TControl;
var
  i: TBrushStyle;
  s: string;
begin
  Result := TComboBox.Create(AParentPanel);
  with Result as TComboBox do
  begin
    Parent := AParentPanel;
    Style := csOwnerDrawFixed;
    OnDrawItem := @FDrawItem;
    OnChange := @ChangeControl;
    for i in FFillStyles do
    begin
      WriteStr(s, i);
      Items.Add(s);
    end;
    ReadOnly := true;
    ItemIndex := FFillIndex;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
//TStyleLineParam
////////////////////////////////////////////////////////////////////////////////////////////////////

constructor TStyleLineParam.Create;
begin
  fName := 'Style of Line';
end;

function TStyleLineParam.ToControl(AParentPanel: TPanel): TControl;
  var
  i: TPenStyle;
  s: string;
begin
  Result := TComboBox.Create(AParentPanel);
  with Result as TComboBox do
  begin
    Parent := AParentPanel;
    Style := csOwnerDrawFixed;
    OnDrawItem := @FDrawItem;
    OnChange := @ChangeControl;
    for i in FStylesLine do
    begin
      WriteStr(s, i);
      Items.Add(s);
    end;
    ReadOnly := true;
    ItemIndex := FLineIndex;
  end;
end;

function TStyleLineParam.fGetLineStyle: TPenStyle;
begin
  Result := fStylesLine[FLineIndex];
end;

procedure TStyleLineParam.ChangeControl(Sender: TObject);
begin
   FLineIndex := (Sender as TComboBox).ItemIndex;
end;

procedure TStyleLineParam.FDrawItem(Control: TWinControl; Index: Integer;
	ARect: TRect; State: TOwnerDrawState);
begin
  with (Control as TComboBox).Canvas do
  begin
    FillRect(ARect);
    Pen.Color := clBlack;
    Pen.Style := fStylesLine[Index];
    Pen.Width := 1;
    Line(ARect.Left + 1, (ARect.Top + ARect.Bottom) div 2, ARect.Right - 1,
       (ARect.Top + ARect.Bottom) div 2);
   end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
//TWidthLineParam
////////////////////////////////////////////////////////////////////////////////////////////////////

constructor TWidthLineParam.Create;
begin
  fName := 'Width of Line';
  fWidthLine := 1;
end;

function TWidthLineParam.ToControl(AParentPanel: TPanel): TControl;
begin
  Result := TTrackBar.Create(AParentPanel);
  with Result as TTrackBar do
  begin
    OnChange := @ChangeControl;
    Parent := AParentPanel;
    Min := 1;
    Max := 20;
    PageSize := 1;
    Position := fWidthLine;
  end;
end;

procedure TWidthLineParam.ChangeControl(Sender: TObject);
begin
  fWidthLine := (Sender as TTrackBar).Position;
end;
////////////////////////////////////////////////////////////////////////////////////////////////////
//TRadiusParam
////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TRadiusParam.ChangeControl(Sender: TObject);
begin
  fRadius := (Sender as TTrackBar).Position;
end;

constructor TRadiusParam.Create;
begin
  fName := 'Радиус';
  fRadius := 5;
end;

function TRadiusParam.ToControl(AParentPanel: TPanel): TControl;
begin
  Result := TTrackBar.Create(AParentPanel);
  with Result as TTrackBar do
  begin
    OnChange := @ChangeControl;
    Parent := AParentPanel;
    Min := 5;
    Max := 50;
    PageSize := 1;
    Position := fRadius;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
//ColorPen
////////////////////////////////////////////////////////////////////////////////////////////////////

constructor TColorLineParam.Create;
begin
  fName := 'Color of Line';
  fColorLine := clBlack;
end;

function TColorLineParam.ToControl(AParentPanel: TPanel): TControl;
begin
  Result := TColorBox.Create(AParentPanel);
  with Result as TColorBox do
  begin
    Parent := AParentPanel;
    ColorRectWidth := 10;
    Style := [cbCustomColor, cbExtendedColors, cbPrettyNames, cbStandardColors];
    Selected := fColorLine;
    OnSelect := @ChangeControl;
  end;
end;

procedure TColorLineParam.ChangeControl(Sender: TObject);
begin
  fColorLine := (Sender as TColorBox).Selected;
end;

end.
