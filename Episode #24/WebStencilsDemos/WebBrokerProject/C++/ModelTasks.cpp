#include <vcl.h>
#pragma hdrstop

#include "ModelTasks.h"
#include <System.SysUtils.hpp>
#pragma package(smart_init)

// TTaskItem implementation
TTaskItem::TTaskItem(int AId, const String& ADescription) {
    FId = AId;
    FDescription = ADescription;
    FCompleted = false;
}

// TTasks implementation
void __fastcall TTasks::ClassCreate() {

}

void __fastcall TTasks::ClassDestroy() {
    if (FInstance) {
        delete FInstance;
        FInstance = nullptr;
    }
    if (FLock) {
        delete FLock;
        FLock = nullptr;
    }
}

TTasks* TTasks::GetInstance() {
    if (!FInstance) {
        FLock->Acquire();
        try {
            if (!FInstance) {
                FInstance = new TTasks();
                // Add some initial tasks
                FInstance->AddTask("Refactor that spaghetti code written in the 90s.");
                FInstance->AddTask("Convince management to upgrade from Delphi 7.");
                FInstance->AddTask("Remove all the captions of the TPanels.");
                FInstance->AddTask("Use `with` statement responsibly... and then regret it immediately.");
                FInstance->AddTask("Refactor all the business logic inside OnClick events.");
                FInstance->AddTask("Convince the team that VCL is still cool.");
                FInstance->AddTask("Document those \"temporary\" global variables created 5 years ago.");
                FInstance->ToggleCompletedTask(2);
            }
        }
        __finally {
            FLock->Release();
        }
    }
    return FInstance;
}

TTasks::TTasks() {
    FItems = new TList__1<TObject*>();
    FNextId = 1;
}

TTasks::~TTasks() {
    // Clean up all TTaskItem objects
    FLock->Acquire();
    try {
        for (int i = 0; i < FItems->Count; i++) {
            delete static_cast<TTaskItem*>(FItems->Items[i]);
        }
        delete FItems;
        FItems = nullptr;
    }
    __finally {
        FLock->Release();
    }
}

int __fastcall TTasks::GetCount() {
    FLock->Acquire();
    try {
        return FItems->Count;
    }
    __finally {
        FLock->Release();
    }
}

int __fastcall TTasks::GetCompletedCount() {
    FLock->Acquire();
    try {
        int result = 0;
        for (int i = 0; i < FItems->Count; i++) {
            TTaskItem* item = static_cast<TTaskItem*>(FItems->Items[i]);
            if (item->Completed) {
                result++;
            }
        }
        return result;
    }
    __finally {
        FLock->Release();
    }
}

TList__1<TObject*>* __fastcall TTasks::GetAllTasks() {
    FLock->Acquire();
    try {
        return FItems;
    }
    __finally {
        FLock->Release();
    }
}

int __fastcall TTasks::GetNextId() {
    FLock->Acquire();
    try {
        return FNextId;
    }
    __finally {
        FLock->Release();
    }
}

TTaskItem* TTasks::FindTaskById(int AId) {
    FLock->Acquire();
    try {
        for (int i = 0; i < FItems->Count; i++) {
            TTaskItem* item = static_cast<TTaskItem*>(FItems->Items[i]);
            if (item->Id == AId) {
                return item;
            }
        }
        return nullptr;
    }
    __finally {
        FLock->Release();
    }
}

TTaskItem* TTasks::AddTask(const String& ADescription) {
    if (ADescription.IsEmpty()) {
        throw Exception("Task description cannot be empty");
    }

    FLock->Acquire();
    try {
        TTaskItem* newItem = new TTaskItem(FNextId, ADescription);
        FItems->Add(newItem);
        FNextId++;
        return newItem;
    }
    __finally {
        FLock->Release();
    }
}

void TTasks::EditTask(int AId, const String& ADescription) {
    if (ADescription.IsEmpty()) {
        throw Exception("Task description cannot be empty");
    }

    FLock->Acquire();
    try {
        TTaskItem* item = FindTaskById(AId);
        if (!item) {
            throw Exception("Task not found");
        }
        item->Description = ADescription;
    }
    __finally {
        FLock->Release();
    }
}

void TTasks::DeleteTask(int AId) {
    FLock->Acquire();
    try {
        TTaskItem* item = FindTaskById(AId);
        if (!item) {
            throw Exception("Task not found");
        }
        FItems->Remove(item);
        delete item;
    }
    __finally {
        FLock->Release();
    }
}

TTaskItem* TTasks::ToggleCompletedTask(int AId) {
    FLock->Acquire();
    try {
        TTaskItem* item = FindTaskById(AId);
        if (!item) {
            throw Exception("Task not found");
        }
        item->Completed = !item->Completed;
        return item;
    }
    __finally {
        FLock->Release();
    }
}