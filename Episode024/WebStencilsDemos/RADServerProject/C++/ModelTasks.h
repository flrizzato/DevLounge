// TasksModel.h
#ifndef ModelTasksH
#define ModelTasksH

#include <System.hpp>
#include <System.Classes.hpp>
#include <System.SyncObjs.hpp>
#include <System.Generics.Collections.hpp>
#include <FireDAC.Comp.Client.hpp>
#include <memory>

class TTaskItem : public TPersistent {
private:
    int FId;
    String FDescription;
    bool FCompleted;

public:
    TTaskItem(int AId, const String& ADescription, const bool ACompleted = false);

__published: // Properties for WebStencils RTTI access
    __property int Id = {read=FId};
    __property String Description = {read=FDescription, write=FDescription};
    __property bool Completed = {read=FCompleted, write=FCompleted};
};

class TTasks : public TObject {
private:
    TFDConnection* FDConnection;

    int __fastcall GetCount();
    int __fastcall GetCompletedCount();
    TList__1<TObject*>* __fastcall GetAllTasks();

public:
    TTasks(TFDConnection* AFDConnection = nullptr);
    virtual ~TTasks();
    TTaskItem* FindTaskById(int AId);
    void AddTask(const String& ADescription);
    void EditTask(int AId, const String& ADescription);
    void DeleteTask(int AId);
    void ToggleCompletedTask(int AId);

__published: // Properties for WebStencils RTTI access
    __property int Count = {read=GetCount};
    __property int CompletedCount = {read=GetCompletedCount};
    __property TList__1<TObject*>* AllTasks = {read=GetAllTasks};
};

#endif 