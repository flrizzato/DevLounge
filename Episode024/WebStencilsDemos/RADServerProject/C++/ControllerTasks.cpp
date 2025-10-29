#include <vcl.h>
#pragma hdrstop

#include "ControllerTasks.h"
#include <System.NetEncoding.hpp>
#include <System.SysUtils.hpp>
#include <System.IOUtils.hpp>
#include <System.JSON.hpp>
#pragma package(smart_init)

TTasksController::TTasksController(TWebStencilsEngine* AWebStencilsEngine, TFDConnection* AFDConnection) {
    if (!AWebStencilsEngine) {
        throw Exception("WebStencilsEngine cannot be null");
    }
    
    try {
        FWebStencilsEngine = AWebStencilsEngine;
        FWebStencilsProcessor = new TWebStencilsProcessor(nullptr);
        FWebStencilsProcessor->Engine = FWebStencilsEngine;
        FTasks = new TTasks(AFDConnection);
        FWebStencilsEngine->AddVar("Tasks", FTasks);
    }
    catch (Exception& E) {
        if (FWebStencilsProcessor) {
            delete FWebStencilsProcessor;
            FWebStencilsProcessor = nullptr;
        }
        throw;
    }
}

TTasksController::~TTasksController() {
    if (FWebStencilsProcessor) {
        delete FWebStencilsProcessor;
        FWebStencilsProcessor = nullptr;
    }
    if (FTasks) {
        delete FTasks;
        FTasks = nullptr;
    }
}

String TTasksController::RenderTemplate(const String& ATemplate, TTaskItem* ATask) {
    String rootDirectory = FWebStencilsEngine->RootDirectory;
    FWebStencilsProcessor->InputFileName = TPath::Combine(rootDirectory, "partials/tasks/" + ATemplate + ".html");
    
    try {
        if (ATask) {
            FWebStencilsProcessor->AddVar("Task", ATask, false);
        }

        String result = FWebStencilsProcessor->Content();
        return result;
    }
    __finally {
        if (ATask) {
            FWebStencilsProcessor->DataVars->Remove("Task");
        }
    }
}

void TTasksController::CreateTask(TEndpointRequest* ARequest, TEndpointResponse* AResponse) {
    try {
        TJSONObject* jsonObj;
        String taskDescription;
        
        if (!(ARequest->Body->TryGetObject(jsonObj) && 
             jsonObj->TryGetValue<String>("task", taskDescription))) {
            AResponse->RaiseBadRequest("Bad request", "Missing data");
            return;
        }
        
        if (taskDescription.IsEmpty()) {
            AResponse->RaiseBadRequest("Error", "Task description is required");
            return;
        }
        
        taskDescription = TNetEncoding::HTML->Encode(taskDescription);
        FTasks->AddTask(taskDescription);
        AResponse->Body->SetString(RenderTemplate("card"));
    }
    catch (Exception& E) {
        AResponse->RaiseBadRequest("Error", E.Message);
    }
}

void TTasksController::DeleteTask(TEndpointRequest* ARequest, TEndpointResponse* AResponse) {
    try {
        String id = ARequest->Params->Values["id"];
        if (id.IsEmpty()) {
            AResponse->RaiseBadRequest("Error", "Task ID is required");
            return;
        }
        
        int taskId = id.ToInt();
        if (taskId <= 0) {
            AResponse->RaiseBadRequest("Error", "Invalid task ID");
            return;
        }
        
        FTasks->DeleteTask(taskId);
        AResponse->Body->SetString(RenderTemplate("card"));
    }
    catch (Exception& E) {
        AResponse->RaiseBadRequest("Error", E.Message);
    }
}

void TTasksController::EditTask(TEndpointRequest* ARequest, TEndpointResponse* AResponse) {
    try {
        TJSONObject* jsonObj;
        String taskDescription;
        
        if (!(ARequest->Body->TryGetObject(jsonObj) && 
             jsonObj->TryGetValue<String>("task", taskDescription))) {
            AResponse->RaiseBadRequest("Bad request", "Missing data");
            return;
        }
        
        if (taskDescription.IsEmpty()) {
            AResponse->RaiseBadRequest("Error", "Task description is required");
            return;
        }
        
        String id = ARequest->Params->Values["id"];
        if (id.IsEmpty()) {
            AResponse->RaiseBadRequest("Error", "Task ID is required");
            return;
        }
        
        int taskId = id.ToInt();
        if (taskId <= 0) {
            AResponse->RaiseBadRequest("Error", "Invalid task ID");
            return;
        }
        
        taskDescription = TNetEncoding::HTML->Encode(taskDescription);
        FTasks->EditTask(taskId, taskDescription);
        AResponse->Body->SetString(RenderTemplate("card"));
    }
    catch (Exception& E) {
        AResponse->RaiseBadRequest("Error", E.Message);
    }
}

void TTasksController::GetEditTask(TEndpointRequest* ARequest, TEndpointResponse* AResponse) {
    try {
        String id = ARequest->Params->Values["id"];
        if (id.IsEmpty()) {
            AResponse->RaiseBadRequest("Error", "Task ID is required");
            return;
        }
        
        int taskId = id.ToInt();
        if (taskId <= 0) {
            AResponse->RaiseBadRequest("Error", "Invalid task ID");
            return;
        }
        
        TTaskItem* task = FTasks->FindTaskById(taskId);
        if (!task) {
            AResponse->RaiseBadRequest("Error", "Task not found");
            return;
        }
        
        AResponse->Body->SetString(RenderTemplate("itemEdit", task));
    }
    catch (Exception& E) {
        AResponse->RaiseBadRequest("Error", E.Message);
    }
}

void TTasksController::ToggleCompletedTask(TEndpointRequest* ARequest, TEndpointResponse* AResponse) {
    try {
        String id = ARequest->Params->Values["id"];
        if (id.IsEmpty()) {
            AResponse->RaiseBadRequest("Error", "Task ID is required");
            return;
        }
        
        int taskId = id.ToInt();
        if (taskId <= 0) {
            AResponse->RaiseBadRequest("Error", "Invalid task ID");
            return;
        }
        
        FTasks->ToggleCompletedTask(taskId);
        AResponse->Body->SetString(RenderTemplate("card"));
    }
    catch (Exception& E) {
        AResponse->RaiseBadRequest("Error", E.Message);
    }
}
