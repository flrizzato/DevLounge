// TasksController.h
#ifndef ControllerTasksH
#define ControllerTasksH

#include <System.hpp>
#include <System.Classes.hpp>
#include <EMS.ResourceAPI.hpp>
#include <FireDAC.Comp.Client.hpp>
#include <Web.Stencils.hpp>
#include "ModelTasks.h"

class TTasksController : public TObject {
private:
    TTasks* FTasks;
    TWebStencilsProcessor* FWebStencilsProcessor;
    TWebStencilsEngine* FWebStencilsEngine;

    String RenderTemplate(const String& ATemplate, TTaskItem* ATask = nullptr);

public:
    TTasksController(TWebStencilsEngine* AWebStencilsEngine, TFDConnection* AFDConnection);
    virtual ~TTasksController();

    void CreateTask(TEndpointRequest* ARequest, TEndpointResponse* AResponse);
    void GetEditTask(TEndpointRequest* ARequest, TEndpointResponse* AResponse);
    void EditTask(TEndpointRequest* ARequest, TEndpointResponse* AResponse);
    void DeleteTask(TEndpointRequest* ARequest, TEndpointResponse* AResponse);
    void ToggleCompletedTask(TEndpointRequest* ARequest, TEndpointResponse* AResponse);
};

#endif 