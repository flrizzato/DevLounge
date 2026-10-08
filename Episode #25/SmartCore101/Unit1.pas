unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Rtti, System.Bindings.Outputs,
  Vcl.Bind.Editors, Data.Bind.EngExt, Vcl.Bind.DBEngExt, Data.Bind.Components,
  Data.Bind.ObjectScope, SmartCoreAI.LiveBindings.Core, Vcl.StdCtrls,
  SmartCoreAI.Comp.Connection, SmartCoreAI.Comp.Chat, SmartCoreAI.Types,
  SmartCoreAI.Driver.OpenAI;

type
  TForm1 = class(TForm)
    AIConnection1: TAIConnection;
    AIChatRequest1: TAIChatRequest;
    Edit1: TEdit;
    Memo1: TMemo;
    AIChatBindSource1: TAIChatBindSource;
    BindingsList1: TBindingsList;
    LinkControlToField1: TLinkControlToField;
    Button1: TButton;
    AIOpenAIDriver1: TAIOpenAIDriver;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.Button1Click(Sender: TObject);
begin
  AIChatRequest1.Chat(Edit1.Text);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  AIChatBindSource1.SetChatRequest(AIChatRequest1);
end;

end.
