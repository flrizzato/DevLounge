unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.JSON, REST.Backend.EMSServices,
  EMSHosting.ExtensionsServices, EMSHosting.EdgeHTTPListener,
  REST.Backend.Providers, EMSHosting.EdgeService, REST.Backend.EMSProvider,
  Vcl.StdCtrls, System.StrUtils, System.Sensors, System.Sensors.Components;

type
  TForm1 = class(TForm)
    EMSProvider1: TEMSProvider;
    EMSEdgeService1: TEMSEdgeService;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
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
  EMSEdgeService1.Active := not EMSEdgeService1.Active;
  Button1.Caption := IfThen(EMSEdgeService1.Active, 'Stop' , 'Start');
end;

end.
