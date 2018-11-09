unit mugenfontfactorycode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, LazUTF8;

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

function get_path(): string;
function check_input(input:string):Boolean;
function convert_file_name(source:string): string;
function execute_program(executable:string;argument:string):Integer;
procedure window_setup();
procedure interface_setup();
procedure set_text_source();
procedure set_graphic_source();
procedure set_font_source();
procedure set_font_target();
procedure run_backend_tool(arguments:string);
procedure decompile_font(font:string);
procedure compile_font(text:string;graphic:string;font:string);
var Form1: TForm1;

implementation

function get_path(): string;
begin
get_path:=ExtractFilePath(Application.ExeName);
end;

function check_input(input:string):Boolean;
var target:Boolean;
begin
target:=True;
if input='' then
begin
target:=False;
end;
check_input:=target;
end;

function convert_file_name(source:string): string;
var target:string;
begin
target:=source;
if Pos(' ',source)>0 then
begin
target:='"';
target:=target+source+'"';
end;
convert_file_name:=target;
end;

function execute_program(executable:string;argument:string):Integer;
var code:Integer;
begin
try
code:=ExecuteProcess(UTF8ToWinCp(executable),UTF8ToWinCp(argument),[]);
except
On EOSError do code:=-1;
end;
execute_program:=code;
end;

procedure window_setup();
begin
 Application.Title:='MUGEN FONT FACTORY';
 Form1.Caption:='MUGEN FONT FACTORY 2.0.7';
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
Form1.LabeledEdit1.EditLabel.Caption:='Text file';
Form1.LabeledEdit2.EditLabel.Caption:='Graphic file';
Form1.LabeledEdit3.EditLabel.Caption:='Font file';
Form1.Button1.ShowHint:=True;
Form1.Button2.ShowHint:=Form1.Button1.ShowHint;
Form1.Button3.ShowHint:=Form1.Button1.ShowHint;
Form1.Button4.ShowHint:=Form1.Button1.ShowHint;
Form1.Button5.ShowHint:=Form1.Button1.ShowHint;
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

procedure set_text_source();
begin
Form1.OpenDialog1.Title:='Open a source text file';
Form1.OpenDialog1.DefaultExt:='*.txt';
Form1.OpenDialog1.FileName:='*.txt';
Form1.OpenDialog1.Filter:='Text files|*.txt';
end;

procedure set_graphic_source();
begin
Form1.OpenDialog1.Title:='Open a source graphic file';
Form1.OpenDialog1.DefaultExt:='*.pcx';
Form1.OpenDialog1.FileName:='*.pcx';
Form1.OpenDialog1.Filter:='PC PaintBrush image|*.pcx';
end;

procedure set_font_source();
begin
Form1.OpenDialog1.Title:='Open a source mugen font';
Form1.OpenDialog1.DefaultExt:='*.fnt';
Form1.OpenDialog1.FileName:='*.fnt';
Form1.OpenDialog1.Filter:='Mugen font|*.fnt';
end;

procedure set_font_target();
begin
Form1.SaveDialog1.Title:='Save a target mugen font';
Form1.SaveDialog1.DefaultExt:='*.fnt';
Form1.SaveDialog1.FileName:='*.fnt';
Form1.SaveDialog1.Filter:='Mugen font|*.fnt';
end;

procedure run_backend_tool(arguments:string);
var error:SmallInt;
var message: array[0..3] of string;
var job:string;
begin
message[0]:='Operation successfully complete';
message[1]:='Cant allocate memory';
message[2]:='File operation error';
message[3]:='Invalid format';
job:=get_path()+'fntreconstructor';
error:=execute_program(job,arguments);
if error=-1 then
begin
ShowMessage('Cant execute a external program');
end
else
begin
ShowMessage(message[error]);
end;

end;

procedure decompile_font(font:string);
begin
run_backend_tool(convert_file_name(font));
end;

procedure compile_font(text:string;graphic:string;font:string);
var target,job:string;
begin
target:=ExtractFileNameWithoutExt(font)+'.fnt';
job:=convert_file_name(text)+' '+convert_file_name(graphic)+' '+convert_file_name(target);
run_backend_tool(job);
end;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
window_setup();
interface_setup();
end;

procedure TForm1.LabeledEdit1Change(Sender: TObject);
begin
Form1.Button3.Enabled:=check_input(Form1.LabeledEdit1.Text) and check_input(Form1.LabeledEdit2.Text);
end;

procedure TForm1.LabeledEdit2Change(Sender: TObject);
begin
Form1.Button3.Enabled:=check_input(Form1.LabeledEdit1.Text) and check_input(Form1.LabeledEdit2.Text);
end;

procedure TForm1.LabeledEdit3Change(Sender: TObject);
begin
Form1.Button5.Enabled:=check_input(Form1.LabeledEdit3.Text);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
set_text_source();
if Form1.OpenDialog1.Execute()=True then
begin
Form1.LabeledEdit1.Text:=Form1.OpenDialog1.FileName;
end;

end;

procedure TForm1.Button2Click(Sender: TObject);
begin
set_graphic_source();
if Form1.OpenDialog1.Execute()=True then
begin
Form1.LabeledEdit2.Text:=Form1.OpenDialog1.FileName;
end;

end;

procedure TForm1.Button3Click(Sender: TObject);
begin
set_font_target();
if Form1.SaveDialog1.Execute()=True then
begin
compile_font(Form1.LabeledEdit1.Text,Form1.LabeledEdit2.Text,Form1.SaveDialog1.FileName);
end;

end;

procedure TForm1.Button4Click(Sender: TObject);
begin
set_font_source();
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
