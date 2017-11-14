unit ToolsParams;

{$mode objfpc}{$H+}

interface

uses
   Classes, SysUtils,Graphics, Dialogs, Controls, ComCtrls, Spin, StdCtrls, ColorBox, ExtCtrls;

type

////////////////////////////////////////////////////////////////////////////////////////////////////
//TParamOpen
////////////////////////////////////////////////////////////////////////////////////////////////////

TToolParam = class
	private
	  FName:string;
	  procedure ChangeControl(Sender: TObject); virtual; abstract;
	public
	  property Name: string read FName;
    function ToControl(AParentPanel: TPanel):TControl; virtual; abstract;
end;

TToolParamsList = array of TToolParam;

	{ TColorLineParam }

TColorLineParam = class(TToolParam)
	private
	  fColorLine: TColor;
	  procedure ChangeControl(Sender: TObject); override;
	public
	  property Value: TColor read fColorLine;
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

////////////////////////////////////////////////////////////////////////////////////////////////////

implementation

{ TFillStyleParam }

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
      Pen.Style := psSolid;
      Pen.Width := 1;
      Brush.Style := FFillStyles[Index];
      if Index <> 1 then
        Brush.Color := clBlack;
      Rectangle(ARect.Left + 1, ARect.Top + 1, ARect.Right - 1,
        ARect.Bottom - 1);
    end;
end;

constructor TFillStyleParam.Create;
begin
  FName := 'Fill style';
  FFillIndex := 1;
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

{ TStyleLineParam }

constructor TStyleLineParam.Create;
begin
  fName := 'Style of Line';
  //fStyleLine := psSolid;
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

{ TWidthLineParam }

constructor TWidthLineParam.Create;
begin
  fName := 'Width of Line';
  fWidthLine := 0;
end;

function TWidthLineParam.ToControl(AParentPanel: TPanel): TControl;
begin
  Result := TTrackBar.Create(AParentPanel);
  with Result as TTrackBar do
  begin
    Parent := AParentPanel;
    Position := fWidthLine;

  end;
end;

procedure TWidthLineParam.ChangeControl(Sender: TObject);
begin
  fWidthLine := (Sender as TTrackBar).Position;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
//Color
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

////////////////////////////////////////////////////////////////////////////////////////////////////
//Width


end.
