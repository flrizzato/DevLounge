unit uGpsResource;

// EMS Resource Module

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  EMS.Services, EMS.ResourceAPI, EMS.ResourceTypes, System.Sensors,
  System.Sensors.Components;

type

  [ResourceName('gps')]
  TGpsResource1 = class(TDataModule)
    LocationSensor1: TLocationSensor;
    procedure LocationSensor1LocationChanged(Sender: TObject;
      const OldLocation, NewLocation: TLocationCoord2D);
  published
    procedure Get(const AContext: TEndpointContext;
      const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
  private
    fLatitude: string;
    fLongitude: string;
  end;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}
{$R *.dfm}

procedure TGpsResource1.Get(const AContext: TEndpointContext;
  const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
begin
  fLatitude := '-23.641517254327955';
  fLongitude := '-46.53649400446457';

  var aLocation := '{"Latitude":"' + fLatitude + '", "Longitude":"' + fLongitude + '"}';

  AResponse.Body.SetValue(TJSONObject.ParseJSONValue
    (TEncoding.UTF8.GetBytes(aLocation), 0) as TJSONValue, True);
end;

procedure Register;
begin
  RegisterResource(TypeInfo(TGpsResource1));
end;

procedure TGpsResource1.LocationSensor1LocationChanged(Sender: TObject;
  const OldLocation, NewLocation: TLocationCoord2D);
begin
  fLatitude := NewLocation.Latitude.ToString;
  fLongitude := NewLocation.Longitude.ToString;
end;

initialization

Register;

end.
