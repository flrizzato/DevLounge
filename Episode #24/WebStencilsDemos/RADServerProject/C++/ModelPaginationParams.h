//---------------------------------------------------------------------------
#ifndef ModelPaginationParamsH
#define ModelPaginationParamsH
//---------------------------------------------------------------------------
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <EMS.ResourceAPI.hpp>

//---------------------------------------------------------------------------
class TPaginationParams : public TObject
{
private:
    int FPageSize;
    int FPageNumber;
    int FTotalPages;
    System::UnicodeString FUri; // Keep if used by template, otherwise remove

    void ParsePaginationParams(TEndpointRequest* ARequest);

    static const int DEFAULT_PAGE_SIZE = 10;
    static const int DEFAULT_PAGE_NUMBER = 1;
    static const int MAX_PAGE_SIZE = 100;

__published:
    __fastcall TPaginationParams(TEndpointRequest* ARequest, System::UnicodeString AUri);
    __property int PageSize = {read=FPageSize};
    __property int PageNumber = {read=FPageNumber};
    __property int TotalPages = {read=FTotalPages, write=FTotalPages};
    __property System::UnicodeString Uri = {read=FUri}; // Keep if used by template

};
//---------------------------------------------------------------------------
#endif 