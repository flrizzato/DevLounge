object GpsResource1: TGpsResource1
  Height = 375
  Width = 750
  PixelsPerInch = 120
  object LocationSensor1: TLocationSensor
    Active = True
    ActivityType = Other
    UsageAuthorization = Always
    OnLocationChanged = LocationSensor1LocationChanged
    Left = 80
    Top = 40
  end
end
