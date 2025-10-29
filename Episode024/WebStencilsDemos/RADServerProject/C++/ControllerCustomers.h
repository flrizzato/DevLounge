//---------------------------------------------------------------------------
#ifndef ControllerCustomersH
#define ControllerCustomersH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Web.Stencils.hpp>
#include <FireDAC.Comp.Client.hpp>
#include <EMS.ResourceAPI.hpp> // For RAD Server request/response

// Forward declarations
class TPaginationParams; // Use the C++ class we created

//---------------------------------------------------------------------------
class TCustomersController : public TObject
{
private:
    TFDQuery* FCustomers;
    Web::Stencils::TWebStencilsProcessor* FWebStencilsProcessor;
    Web::Stencils::TWebStencilsEngine* FWebStencilsEngine;

    // Render Template Overloads
    System::UnicodeString RenderTemplate(System::UnicodeString ATemplate, TPaginationParams* APaginationParams);
    System::UnicodeString RenderTemplate(System::UnicodeString ATemplate); // For no pagination info

public:
    __fastcall TCustomersController(Web::Stencils::TWebStencilsEngine* AWebStencilsEngine, TFDQuery* ACustomers);
    virtual __fastcall ~TCustomersController();

    // Endpoint methods adapted for RAD Server
    void __fastcall GetCustomers(TEndpointRequest* ARequest, TEndpointResponse* AResponse);
    void __fastcall GetAllCustomers(TEndpointRequest* ARequest, TEndpointResponse* AResponse);
};
//---------------------------------------------------------------------------
#endif 