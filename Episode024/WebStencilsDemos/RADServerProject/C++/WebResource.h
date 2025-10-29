// EMS Resource Modules
//---------------------------------------------------------------------------

#ifndef WebResourceH
#define WebResourceH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <EMS.Services.hpp>
#include <EMS.ResourceAPI.hpp>
#include <EMS.ResourceTypes.hpp>
#include <Data.DB.hpp>
#include <EMS.DataSetResource.hpp>
#include <FireDAC.Comp.Client.hpp>
#include <FireDAC.Comp.DataSet.hpp>
#include <FireDAC.ConsoleUI.Wait.hpp>
#include <FireDAC.DApt.hpp>
#include <FireDAC.DApt.Intf.hpp>
#include <FireDAC.DatS.hpp>
#include <FireDAC.Phys.hpp>
#include <FireDAC.Phys.IB.hpp>
#include <FireDAC.Phys.IBDef.hpp>
#include <FireDAC.Phys.Intf.hpp>
#include <FireDAC.Stan.Async.hpp>
#include <FireDAC.Stan.Def.hpp>
#include <FireDAC.Stan.Error.hpp>
#include <FireDAC.Stan.Intf.hpp>
#include <FireDAC.Stan.Option.hpp>
#include <FireDAC.Stan.Param.hpp>
#include <FireDAC.Stan.Pool.hpp>
#include <FireDAC.UI.Intf.hpp>
#include <EMS.FileResource.hpp>
#include <Web.HTTPApp.hpp>
#include <Web.Stencils.hpp>
#include <memory> // For unique_ptr

// Forward declarations
class TCodeExamples;
class TTasksController;
class TCustomersController;
class TEnvironmentSettings;

#pragma explicit_rtti methods (public)
//---------------------------------------------------------------------------
// Define the Environment Settings class for RAD Server C++
class TEnvironmentSettings : public TObject
{
private:
    System::UnicodeString FAppVersion;
    System::UnicodeString FAppName;
    System::UnicodeString FAppEdition;
    System::UnicodeString FCompanyName;
    System::UnicodeString FResource;
    bool FDebugMode;
    bool FIsRadServer;

public:
    TEnvironmentSettings();

__published:
    __property System::UnicodeString AppVersion = {read=FAppVersion};
    __property System::UnicodeString AppName = {read=FAppName};
    __property System::UnicodeString AppEdition = {read=FAppEdition};
    __property System::UnicodeString CompanyName = {read=FCompanyName};
    __property System::UnicodeString Resource = {read=FResource};
    __property bool DebugMode = {read=FDebugMode};
    __property bool IsRadServer = {read=FIsRadServer};
};


//---------------------------------------------------------------------------
class TWebstencilsResource1 : public TDataModule
{
__published:
    TEMSFileResource *html;
    TWebStencilsEngine *WebStencilsEngine1;
    TWebStencilsProcessor *WebStencilsProcessor;
    TFDConnection *FDConnection;
    TFDQuery *customers;
    TEMSFileResource *css;
    TEMSFileResource *js;
    TEMSFileResource *img;

private:
    TCodeExamples* FCodeExamples;
    TTasksController* FTasksController;
    TCustomersController* FCustomersController;
    TEnvironmentSettings* FEnvironmentSettings;
    void __fastcall WebStencilsEngine1Value(TObject* Sender, const String AObjectName,
            const String APropName, String &AReplaceText, bool &AHandled);

public:
    __fastcall TWebstencilsResource1(TComponent* Owner);
    void GetHome(TEndpointContext* AContext, TEndpointRequest* ARequest, TEndpointResponse* AResponse);
    void DeleteTask(TEndpointContext* AContext, TEndpointRequest* ARequest, TEndpointResponse* AResponse);
    void PostTask(TEndpointContext* AContext, TEndpointRequest* ARequest, TEndpointResponse* AResponse);
    void GetTasksEdit(TEndpointContext* AContext, TEndpointRequest* ARequest, TEndpointResponse* AResponse);
    void PutTaskToggleCompleted(TEndpointContext* AContext, TEndpointRequest* ARequest, TEndpointResponse* AResponse);
    void PutTask(TEndpointContext* AContext, TEndpointRequest* ARequest, TEndpointResponse* AResponse);
    void GetPaginatedCustomers(TEndpointContext* AContext, TEndpointRequest* ARequest, TEndpointResponse* AResponse);
    void GetAllCustomersEndpoint(TEndpointContext* AContext, TEndpointRequest* ARequest, TEndpointResponse* AResponse);
};

#endif
