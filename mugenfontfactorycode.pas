unit mugenfontfactorycode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, LazFileUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    LabeledEdit3: TLabeledEdit;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    SaveDialog1: TSaveDialog;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LabeledEdit1Change(Sender: TObject);
    procedure LabeledEdit2Change(Sender: TObject);
    procedure LabeledEdit3Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var Form1: TForm1;

implementation

function convert_file_name(const source:string): string;
var target:string;
begin
 target:=source;
 if Pos(' ',source)>0 then
 begin
  target:='"'+source+'"';
 end;
 convert_file_name:=target;
end;

function execute_program(const executable:string;const argument:string):Integer;
var code:Integer;
begin
 try
  code:=ExecuteProcess(executable,argument,[]);
 except
  code:=-1;
 end;
 execute_program:=code;
end;

procedure run_backend_tool(const arguments:string);
var error:SmallInt;
var message: array[0..5] of string=('Operation successfully complete','Cant open input file','Cant create output file','Cant jump to target offset','Cant allocate memory','Invalid format');
var job,status:string;
begin
 status:='Cant execute an external program';
 job:=ExtractFilePath(Application.ExeName)+'fntreconstructor.exe';
 error:=execute_program(job,arguments);
 if error>=0 then
 begin
  status:=message[error];
 end;
 ShowMessage(status);
end;

procedure decompile_font(const font:string);
begin
 run_backend_tool(convert_file_name(font));
end;

procedure compile_font(const configuration:string;const graphic:string;const font:string);
var target,job:string;
begin
 target:=ExtractFileNameWithoutExt(font)+'.fnt';
 job:=convert_file_name(configuration)+' '+convert_file_name(graphic)+' '+convert_file_name(target);
 run_backend_tool(job);
end;

procedure window_setup();
begin
 Application.Title:='MUGEN FONT FACTORY';
 Form1.Caption:='MUGEN FONT FACTORY 2.1.9';
 Form1.BorderStyle:=bsDialog;
 Form1.Font.Name:=Screen.MenuFont.Name;
 Form1.Font.Size:=14;
end;

procedure interface_setup();
begin
 Form1.LabeledEdit1.LabelPosition:=lpLeft;
 Form1.LabeledEdit2.LabelPosition:=Form1.LabeledEdit1.LabelPosition;
 Form1.LabeledEdit3.LabelPosition:=Form1.LabeledEdit1.LabelPosition;
 Form1.LabeledEdit1.Enabled:=False;
 Form1.LabeledEdit2.Enabled:=Form1.LabeledEdit1.Enabled;
 Form1.LabeledEdit3.Enabled:=Form1.LabeledEdit1.Enabled;
 Form1.Button3.Enabled:=False;
 Form1.Button5.Enabled:=Form1.Button3.Enabled;
 Form1.LabeledEdit1.Text:='';
 Form1.LabeledEdit2.Text:=Form1.LabeledEdit1.Text;
 Form1.LabeledEdit3.Text:=Form1.LabeledEdit1.Text;
 Form1.Button1.ShowHint:=True;
 Form1.Button2.ShowHint:=Form1.Button1.ShowHint;
 Form1.Button3.ShowHint:=Form1.Button1.ShowHint;
 Form1.Button4.ShowHint:=Form1.Button1.ShowHint;
 Form1.Button5.ShowHint:=Form1.Button1.ShowHint;
end;

procedure language_setup();
begin
 Form1.LabeledEdit1.EditLabel.Caption:='Text file';
 Form1.LabeledEdit2.EditLabel.Caption:='Graphic file';
 Form1.LabeledEdit3.EditLabel.Caption:='Font file';
 Form1.Button1.Caption:='Open';
 Form1.Button2.Caption:='Open';
 Form1.Button3.Caption:='Compile';
 Form1.Button4.Caption:='Open';
 Form1.Button5.Caption:='Decompile';
 Form1.Button1.Hint:='Set a source graphic file';
 Form1.Button2.Hint:='Set a source text file';
 Form1.Button3.Hint:='Compile a mugen font';
 Form1.Button4.Hint:='Open a target mugen font';
 Form1.Button5.Hint:='Decompile a mugen font';
 Form1.PageControl1.Pages[0].Caption:='Compilation';
 Form1.PageControl1.Pages[1].Caption:='Decompilation';
end;

procedure setup();
begin
 window_setup();
 interface_setup();
 language_setup();
end;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
 setup();
end;

procedure TForm1.LabeledEdit1Change(Sender: TObject);
begin
 Form1.Button3.Enabled:=(Form1.LabeledEdit1.Text<>'') and (Form1.LabeledEdit2.Text<>'');
end;

procedure TForm1.LabeledEdit2Change(Sender: TObject);
begin
 Form1.Button3.Enabled:=(Form1.LabeledEdit1.Text<>'') and (Form1.LabeledEdit2.Text<>'');
end;

procedure TForm1.LabeledEdit3Change(Sender: TObject);
begin
 Form1.Button5.Enabled:=Form1.LabeledEdit3.Text<>'';
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
 Form1.OpenDialog1.Title:='Open a source text file';
 Form1.OpenDialog1.DefaultExt:='*.txt';
 Form1.OpenDialog1.FileName:='*.txt';
 Form1.OpenDialog1.Filter:='Text files|*.txt';
 if Form1.OpenDialog1.Execute()=True then
 begin
  Form1.LabeledEdit1.Text:=Form1.OpenDialog1.FileName;
 end;

end;

procedure TForm1.Button2Click(Sender: TObject);
begin
 Form1.OpenDialog1.Title:='Open a source graphic file';
 Form1.OpenDialog1.DefaultExt:='*.pcx';
 Form1.OpenDialog1.FileName:='*.pcx';
 Form1.OpenDialog1.Filter:='PC PaintBrush image|*.pcx';
 if Form1.OpenDialog1.Execute()=True then
 begin
  Form1.LabeledEdit2.Text:=Form1.OpenDialog1.FileName;
 end;

end;

procedure TForm1.Button3Click(Sender: TObject);
begin
 Form1.SaveDialog1.Title:='Save a target mugen font';
 Form1.SaveDialog1.DefaultExt:='*.fnt';
 Form1.SaveDialog1.FileName:='*.fnt';
 Form1.SaveDialog1.Filter:='Mugen font|*.fnt';
 if Form1.SaveDialog1.Execute()=True then
 begin
  compile_font(Form1.LabeledEdit1.Text,Form1.LabeledEdit2.Text,Form1.SaveDialog1.FileName);
 end;

end;

procedure TForm1.Button4Click(Sender: TObject);
begin
 Form1.OpenDialog1.Title:='Open a source mugen font';
 Form1.OpenDialog1.DefaultExt:='*.fnt';
 Form1.OpenDialog1.FileName:='*.fnt';
 Form1.OpenDialog1.Filter:='Mugen font|*.fnt';
 if Form1.OpenDialog1.Execute()=True then
 begin
  Form1.LabeledEdit3.Text:=Form1.OpenDialog1.FileName;
 end;

end;

procedure TForm1.Button5Click(Sender: TObject);
begin
 decompile_font(Form1.LabeledEdit3.Text);
end;

end.
