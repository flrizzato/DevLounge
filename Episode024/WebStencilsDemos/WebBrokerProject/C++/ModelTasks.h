// TasksModel.h
#ifndef ModelTasksH
#define ModelTasksH

#include <System.hpp>
#include <System.Classes.hpp>
#include <System.SyncObjs.hpp>
#include <System.Generics.Collections.hpp>
#include <memory>

class TTaskItem : public TObject {
private:
    int FId;
    String FDescription;
    bool FCompleted;

public:
    TTaskItem(int AId, const String& ADescription);
    
    __published:
    __property int Id = {read=FId};
    __property String Description = {read=FDescription, write=FDescription};
    __property bool Completed = {read=FCompleted, write=FCompleted};
};

class TTasks : public TObject {
private:
    static TTasks* FInstance;
    static TCriticalSection* FLock;
    
    TList__1<TObject*>* FItems;
    int FNextId;

    TTasks();
    int __fastcall GetCount();
    int __fastcall GetCompletedCount();
    TList__1<TObject*>* __fastcall GetAllTasks();
    int __fastcall GetNextId();

public:
    static TTasks* GetInstance();
    static void __fastcall ClassCreate();
    static void __fastcall ClassDestroy();

    virtual ~TTasks();
    TTaskItem* FindTaskById(int AId);
    TTaskItem* AddTask(const String& ADescription);
    void EditTask(int AId, const String& ADescription);
    void DeleteTask(int AId);
    TTaskItem* ToggleCompletedTask(int AId);

    __published:
    __property int Count = {read=GetCount};
    __property int CompletedCount = {read=GetCompletedCount};
    __property int NextId = {read=GetNextId};
    __property TList__1<TObject*>* AllTasks = {read=GetAllTasks};
};

// Initialize static members
inline TTasks* TTasks::FInstance = nullptr;
inline TCriticalSection* TTasks::FLock = new TCriticalSection();

#endif 