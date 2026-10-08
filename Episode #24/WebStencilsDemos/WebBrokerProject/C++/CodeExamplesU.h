// CodeExamplesU.h
#ifndef CodeExamplesUH
#define CodeExamplesUH

#include <System.hpp>
#include <System.Classes.hpp>
#include "Web.Stencils.hpp"

class TCodeExample : public TObject {
private:
    String FId;
    String FCode;

public:
    TCodeExample(const String& AId, const String& ACode);
    
    __published:
    __property String Id = {read=FId};
    __property String Code = {read=FCode, write=FCode};
};

class TCodeExamples : public TObject {
private:
    TStringList* FItems;
    TWebStencilsEngine* FWebStencilsEngine;
    void InitValues();

public:
    TCodeExamples(TWebStencilsEngine* AWebStencilsEngine);
    virtual ~TCodeExamples();

    void Add(const String& AId, const String& ACode);
    TCodeExample* Find(const String& AId);
};

#endif
