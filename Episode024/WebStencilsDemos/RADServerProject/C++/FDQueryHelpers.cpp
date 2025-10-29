#pragma hdrstop

#include "FDQueryHelpers.h"
#include <System.DateUtils.hpp>
#include <FireDAC.Stan.Option.hpp>
#include <FireDAC.Stan.Param.hpp>
#include <memory>
#include <climits>

//-----------------------------------------------------------------------------
#pragma package(smart_init)
//-----------------------------------------------------------------------------
namespace FDQueryHelpers {

    static int FCachedRecordCount = 0;
    static TDateTime FLastCountTime = 0;
    const int CACHE_TIMEOUT = 5; // seconds

    // Helper to get skip count, ensuring it's not negative
    int GetActualSkipCount(TFDQuery* Query)
    {
        int Result = Query->FetchOptions->RecsSkip;
        return (Result < 0) ? 0 : Result;
    }

    void SetPageSize(TFDQuery* Query, int Value)
    {
        if (Value <= 0)
        {
            throw Exception("PageSize must be greater than 0");
        }

        // Store the current page number before changing page size
        int LOldPageNumber = GetPageNumber(Query);

        // Set the new page size
        Query->FetchOptions->RecsMax = Value;

        // Keep the same page number, but recalculate skip count for new page size
        Query->FetchOptions->RecsSkip = (LOldPageNumber - 1) * Value;
    }

    int GetPageSize(TFDQuery* Query)
    {
        int Result = Query->FetchOptions->RecsMax;
        return (Result <= 0) ? INT_MAX : Result; // Treat 0 or less as 'all records'
    }

    void SetPageNumber(TFDQuery* Query, int Value)
    {
        if (Value <= 0)
        {
            throw Exception("PageNumber must be greater than 0");
        }

        // Calculate skip count based on the new page number and current page size
        int CurrentPageSize = GetPageSize(Query);
        if (CurrentPageSize == INT_MAX) // Cannot set page number if page size is effectively infinite
            Query->FetchOptions->RecsSkip = 0;
        else
            Query->FetchOptions->RecsSkip = (Value - 1) * CurrentPageSize;
    }

    int GetPageNumber(TFDQuery* Query)
    {
        int CurrentPageSize = GetPageSize(Query);
        if (CurrentPageSize == INT_MAX)
        {
            return 1;
        }
        else
        {
            return (GetActualSkipCount(Query) / CurrentPageSize) + 1;
        }
    }

    int GetTotalRecords(TFDQuery* Query)
    {
        // Check cache first
        if ((FCachedRecordCount > 0) &&
            (SecondsBetween(Now(), FLastCountTime) < CACHE_TIMEOUT))
        {
            return FCachedRecordCount;
        }

        // Cache expired or not set, get fresh count
        std::unique_ptr<TFDQuery> LCloneQuery(new TFDQuery(nullptr));
        LCloneQuery->Connection = Query->Connection;
        LCloneQuery->SQL->Text = Query->SQL->Text;

        if (Query->Params->Count > 0)
        {
            LCloneQuery->Params->AssignValues(Query->Params);
        }

        LCloneQuery->FetchOptions->RecordCountMode = TFDRecordCountMode::cmTotal;

        LCloneQuery->Open();
        try
        {
            int Result = (LCloneQuery->RecordCount > 0) ? LCloneQuery->RecordCount : 0;
            // Update cache
            FCachedRecordCount = Result;
            FLastCountTime = Now();
            return Result;
        }
        __finally
        {
            LCloneQuery->Close();
        }
    }

    int GetTotalPages(TFDQuery* Query)
    {
        int CurrentPageSize = GetPageSize(Query);
        if (CurrentPageSize == INT_MAX)
        {
            return 1;
        }
        else
        {
            int LTotalRecs = GetTotalRecords(Query);
            int Result = (LTotalRecs + CurrentPageSize - 1) / CurrentPageSize;
            return (Result == 0) ? 1 : Result; // Ensure at least 1 page
        }
    }

    void ApplyPagination(TFDQuery* Query)
    {
        if (!Query->Active)
            return;

        // Save current fetch settings (RecsSkip is already calculated by SetPageNumber/SetPageSize)
        int LSavedSkip = Query->FetchOptions->RecsSkip;
        int LSavedMax = Query->FetchOptions->RecsMax;

        Query->Disconnect();
        // Reapply fetch settings before opening
        Query->FetchOptions->RecsSkip = LSavedSkip;
        Query->FetchOptions->RecsMax = LSavedMax;
        Query->Open();
    }

    void CancelPagination(TFDQuery* Query)
    {
        Query->FetchOptions->RecsSkip = -1;
        Query->FetchOptions->RecsMax = -1;
        if (!Query->Active)
            return;

        Query->Disconnect();
        Query->Open();
    }

}