#include <vcl.h>
#pragma hdrstop

#include "ModelTasks.h"
#include <System.SysUtils.hpp>
#include <System.Variants.hpp>
#pragma package(smart_init)

// TTaskItem implementation
TTaskItem::TTaskItem(int AId, const String& ADescription, const bool ACompleted) {
    FId = AId;
    FDescription = ADescription;
    FCompleted = ACompleted;
}

// TTasks implementation
TTasks::TTasks(TFDConnection* AFDConnection) {
    FDConnection = AFDConnection;
}

TTasks::~TTasks() {
}

int __fastcall TTasks::GetCount() {
    Variant count = FDConnection->ExecSQLScalar("SELECT COUNT(*) FROM tasks");
    return VarIsNull(count) ? 0 : (int)count;
}

int __fastcall TTasks::GetCompletedCount() {
    Variant count = FDConnection->ExecSQLScalar("SELECT COUNT(*) FROM tasks WHERE completed = True");
    return VarIsNull(count) ? 0 : (int)count;
}

TList__1<TObject*>* __fastcall TTasks::GetAllTasks() {
    TList__1<TObject*>* LItems = new TList__1<TObject*>();

    if (FDConnection) {
        std::unique_ptr<TFDQuery> query(new TFDQuery(nullptr));
        try {
            query->Connection = FDConnection;
            query->SQL->Text = "SELECT * FROM tasks ORDER BY id";
            query->Open();

            while (!query->Eof) {
                TTaskItem* newItem = new TTaskItem(query->FieldByName("id")->AsInteger,
                                                   query->FieldByName("description")->AsString,
                                                   query->FieldByName("completed")->AsBoolean);
                LItems->Add(newItem);
                query->Next();
            }
            query->Close();
        }
        catch(...) {
            delete LItems;
            throw;
        }
    }
    return LItems;
}

TTaskItem* TTasks::FindTaskById(int AId) {
    TTaskItem* result = nullptr;
    if (FDConnection) {
        std::unique_ptr<TFDQuery> query(new TFDQuery(nullptr));
        try {
            query->Connection = FDConnection;
            query->SQL->Text = "SELECT * FROM tasks WHERE id = :id";
            query->ParamByName("id")->AsInteger = AId;
            query->Open();

            if (!query->Eof) {
                 result = new TTaskItem(query->FieldByName("id")->AsInteger,
                                        query->FieldByName("description")->AsString,
                                        query->FieldByName("completed")->AsBoolean);
            }
            query->Close();
        }
        catch(...) {
            delete result;
            throw;
        }
    }
    return result;
}

void TTasks::AddTask(const String& ADescription) {
    if (ADescription.IsEmpty()) {
        throw Exception("Task description cannot be empty");
    }

    if (FDConnection) {
        int nextId = 1;
        Variant maxId = FDConnection->ExecSQLScalar("SELECT MAX(id) FROM tasks");
        if (!VarIsNull(maxId)) {
            nextId = (int)maxId + 1;
        }

        FDConnection->ExecSQL("INSERT INTO tasks(id, description, completed) VALUES (:id, :description, :completed)",
                            OPENARRAY(Variant, (nextId, ADescription, false)));
    }
}

void TTasks::EditTask(int AId, const String& ADescription) {
    if (ADescription.IsEmpty()) {
        throw Exception("Task description cannot be empty");
    }
    if (FDConnection) {
        FDConnection->ExecSQL("UPDATE tasks SET description = :description WHERE id = :id",
                             OPENARRAY(Variant, (ADescription, AId)));
    }
}

void TTasks::DeleteTask(int AId) {
    if (FDConnection) {
        FDConnection->ExecSQL("DELETE FROM tasks WHERE id = :id", OPENARRAY(Variant, (AId)));
    }
}

void TTasks::ToggleCompletedTask(int AId) {
    if (FDConnection) {
        FDConnection->ExecSQL("EXECUTE PROCEDURE toggle_completed_task :id", OPENARRAY(Variant, (AId)));
    }
} 