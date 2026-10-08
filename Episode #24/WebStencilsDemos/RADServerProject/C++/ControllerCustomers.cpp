//---------------------------------------------------------------------------
#pragma hdrstop

#include "ControllerCustomers.h"
#include "ModelPaginationParams.h" // Include the C++ header
#include <Web.Stencils.hpp>
#include <System.IOUtils.hpp>
#include <System.SysUtils.hpp>
#include <System.JSON.hpp>     // For TryGetValue if used directly
#include <memory>              // For std::unique_ptr
#include <iostream>            // For basic error logging
#include <FireDAC.Comp.Client.hpp>
#include "FDQueryHelpers.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

__fastcall TCustomersController::TCustomersController(Web::Stencils::TWebStencilsEngine* AWebStencilsEngine, TFDQuery* ACustomers)
    : TObject()
{
    try
    {
        FWebStencilsEngine = AWebStencilsEngine;
        FWebStencilsProcessor = new Web::Stencils::TWebStencilsProcessor(nullptr);
        FWebStencilsProcessor->Engine = FWebStencilsEngine;
        FCustomers = ACustomers;
    }
    catch (Exception &E)
    {
        std::cerr << "TCustomersController Constructor Error: " << AnsiString(E.Message).c_str() << std::endl;
        throw;
    }
}
//---------------------------------------------------------------------------

__fastcall TCustomersController::~TCustomersController()
{
    delete FWebStencilsProcessor;
}
//---------------------------------------------------------------------------

System::UnicodeString TCustomersController::RenderTemplate(System::UnicodeString ATemplate)
{
    FWebStencilsProcessor->InputFileName = TPath::Combine(FWebStencilsEngine->RootDirectory,
        System::UnicodeString("customers") + System::Sysutils::PathDelim + ATemplate + ".html");
    return FWebStencilsProcessor->Content();
}
//---------------------------------------------------------------------------

// Overload for templates with pagination data
System::UnicodeString TCustomersController::RenderTemplate(System::UnicodeString ATemplate, TPaginationParams* APaginationParams)
{
    FWebStencilsProcessor->InputFileName = TPath::Combine(FWebStencilsEngine->RootDirectory,
        System::UnicodeString("customers") + System::Sysutils::PathDelim + ATemplate + ".html");

    if (APaginationParams != nullptr)
    {
        FWebStencilsProcessor->AddVar("customersPagination", APaginationParams, false);
    }

    System::UnicodeString Result = FWebStencilsProcessor->Content();

    if (APaginationParams != nullptr)
    {
        FWebStencilsProcessor->DataVars->Remove("customersPagination");
    }
    return Result;
}
//---------------------------------------------------------------------------

void __fastcall TCustomersController::GetCustomers(TEndpointRequest* ARequest, TEndpointResponse* AResponse)
{
    std::unique_ptr<TPaginationParams> LPaginationParams;
    try
    {
        // Create pagination params object from request
        LPaginationParams = std::unique_ptr<TPaginationParams>(new TPaginationParams(ARequest, "pagination"));

        FDQueryHelpers::SetPageSize(FCustomers, LPaginationParams->PageSize);
        FDQueryHelpers::SetPageNumber(FCustomers, LPaginationParams->PageNumber);
        FDQueryHelpers::ApplyPagination(FCustomers); // Activates/refreshes query

        // Update the total pages in the params object *after* applying pagination
        LPaginationParams->TotalPages = FDQueryHelpers::GetTotalPages(FCustomers);

        // Render template and set RAD Server response
        // Pass the raw pointer using .get()
        AResponse->Body->SetString(RenderTemplate("pagination", LPaginationParams.get()));
    }
    catch (Exception &E)
    {
        std::cerr << "TCustomersController::GetCustomers Error: " << AnsiString(E.Message).c_str() << std::endl;
        AResponse->RaiseError(500, "Error retrieving customers", E.Message);
    }
}
//---------------------------------------------------------------------------

void __fastcall TCustomersController::GetAllCustomers(TEndpointRequest* ARequest, TEndpointResponse* AResponse)
{
    try
    {
        FDQueryHelpers::CancelPagination(FCustomers); // Deactivates/refreshes

        // Render template and set RAD Server response
        AResponse->Body->SetString(RenderTemplate("bigtable"));
    }
    catch (Exception &E)
    {
        std::cerr << "TCustomersController::GetAllCustomers Error: " << AnsiString(E.Message).c_str() << std::endl;
        AResponse->RaiseError(500, "Error retrieving all customers", E.Message);
    }
}
//--------------------------------------------------------------------------- 