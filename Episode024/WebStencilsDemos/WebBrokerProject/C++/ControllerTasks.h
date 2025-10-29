// TasksController.h
#ifndef ControllerTasksH
#define ControllerTasksH

#include <System.hpp>
#include <System.Classes.hpp>
#include <Web.HTTPApp.hpp>
#include <Web.Stencils.hpp>
#include "ModelTasks.h"

class TTasksController : public TObject {
private:
    TTasks* FTasks;
    TWebStencilsProcessor* FWebStencilsProcessor;
    TWebStencilsEngine* FWebStencilsEngine;

    String RenderTemplate(const String& ATemplate, TTaskItem* ATask = nullptr);

public:
    TTasksController(TWebStencilsEngine* AWebStencilsEngine);
    virtual ~TTasksController();

    void CreateTask(TObject* Sender, TWebRequest* Request, TWebResponse* Response, bool& Handled);
    void GetEditTask(TObject* Sender, TWebRequest* Request, TWebResponse* Response, bool& Handled);
    void EditTask(TObject* Sender, TWebRequest* Request, TWebResponse* Response, bool& Handled);
    void DeleteTask(TObject* Sender, TWebRequest* Request, TWebResponse* Response, bool& Handled);
    void ToggleCompletedTask(TObject* Sender, TWebRequest* Request, TWebResponse* Response, bool& Handled);
};

#endif