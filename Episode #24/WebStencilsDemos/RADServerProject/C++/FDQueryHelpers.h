#ifndef FDQueryHelpersH
#define FDQueryHelpersH

#include <FireDAC.Comp.Client.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>

// Global helper functions mimicking the Delphi TFDQueryHelper
namespace FDQueryHelpers {

    void SetPageSize(TFDQuery* Query, int Value);
    int GetPageSize(TFDQuery* Query);
    void SetPageNumber(TFDQuery* Query, int Value);
    int GetPageNumber(TFDQuery* Query);
    int GetTotalRecords(TFDQuery* Query);
    int GetTotalPages(TFDQuery* Query);
    void ApplyPagination(TFDQuery* Query);
    void CancelPagination(TFDQuery* Query);

} // namespace FDQueryHelpers

#endif 