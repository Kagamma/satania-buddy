unit Vosk;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

interface

uses
  Math, dynlibs;

type
  PVoskModel = Pointer;
  PVoskSpkModel = Pointer;
  PVoskRecognizer = Pointer;
  PVoskBatchModel = Pointer;
  PVoskBatchRecognizer = Pointer;

const
{$IFDEF Linux}
  VoskLib = 'libvosk.so';
{$ENDIF}
{$IFDEF Windows}
  VoskLib = 'libvosk.dll';
{$ENDIF}

var
  vosk_model_new: function(Path: PChar): PVoskModel; cdecl;
  vosk_model_free: procedure(Model: PVoskModel); cdecl;
  vosk_model_find_word: function(Model: PVoskModel; W: PChar): Integer; cdecl;
  vosk_spk_model_new: function(Path: PChar): PVoskSpkModel; cdecl;
  vosk_spk_model_free: procedure(Model: PVoskSpkModel); cdecl;
  vosk_recognizer_new: function(Model: PVoskModel; SampleRate: Single): PVoskRecognizer; cdecl;
  vosk_recognizer_new_spk: function(Model: PVoskModel; SampleRate: Single; SpkModel: PVoskSpkModel): PVoskRecognizer; cdecl;
  vosk_recognizer_new_grm: function(Model: PVoskModel; SampleRate: Single; Grammar: PChar): PVoskRecognizer; cdecl;
  vosk_recognizer_set_spk_model: procedure(Rec: PVoskRecognizer; SpkModel: PVoskSpkModel); cdecl;
  vosk_recognizer_set_max_alternatives: procedure(Rec: PVoskRecognizer; MaxAlternatives: Integer); cdecl;
  vosk_recognizer_set_words: procedure(Rec: PVoskRecognizer; Words: Integer); cdecl;              
  vosk_recognizer_set_nlsml: procedure(Rec: PVoskRecognizer; NLSML: Integer); cdecl;
  vosk_recognizer_accept_waveform: function(Rec: PVoskRecognizer; Data: Pointer; Len: Integer): Integer; cdecl;
  vosk_recognizer_accept_waveform_s: function(Rec: PVoskRecognizer; Data: Pointer; Len: Integer): Integer; cdecl;
  vosk_recognizer_accept_waveform_f: function(Rec: PVoskRecognizer; Data: Pointer; Len: Integer): Integer; cdecl;
  vosk_recognizer_result: function(Rec: PVoskRecognizer): PChar; cdecl;
  vosk_recognizer_partial_result: function(Rec: PVoskRecognizer): PChar; cdecl;
  vosk_recognizer_final_result: function(Rec: PVoskRecognizer): PChar; cdecl;
  vosk_recognizer_reset: procedure(Rec: PVoskRecognizer); cdecl;
  vosk_recognizer_free: procedure(Rec: PVoskRecognizer); cdecl;
  vosk_set_log_level: procedure(Level: Integer); cdecl;
  vosk_gpu_init: procedure; cdecl;
  vosk_gpu_thread_init: procedure; cdecl;
  Lib: TLibHandle;

implementation

procedure LoadLibrary;
begin
  Lib := DynLibs.LoadLibrary(VoskLib);
  if Lib <> DynLibs.NilHandle then
  begin
    vosk_model_new := GetProcAddress(Lib, 'vosk_model_new');      
    vosk_model_free := GetProcAddress(Lib, 'vosk_model_free');
    vosk_model_find_word := GetProcAddress(Lib, 'vosk_model_find_word');
    vosk_spk_model_new := GetProcAddress(Lib, 'vosk_spk_model_new');
    vosk_spk_model_free := GetProcAddress(Lib, 'vosk_spk_model_free');
    vosk_recognizer_new := GetProcAddress(Lib, 'vosk_recognizer_new');
    vosk_recognizer_new_spk := GetProcAddress(Lib, 'vosk_recognizer_new_spk');
    vosk_recognizer_new_grm := GetProcAddress(Lib, 'vosk_recognizer_new_grm');
    vosk_recognizer_set_spk_model := GetProcAddress(Lib, 'vosk_recognizer_set_spk_model');
    vosk_recognizer_set_max_alternatives := GetProcAddress(Lib, 'vosk_recognizer_set_max_alternatives');
    vosk_recognizer_set_words := GetProcAddress(Lib, 'vosk_recognizer_set_words');
    vosk_recognizer_set_nlsml := GetProcAddress(Lib, 'vosk_recognizer_set_nlsml');
    vosk_recognizer_accept_waveform := GetProcAddress(Lib, 'vosk_recognizer_accept_waveform');
    vosk_recognizer_accept_waveform_s := GetProcAddress(Lib, 'vosk_recognizer_accept_waveform_s');
    vosk_recognizer_accept_waveform_f := GetProcAddress(Lib, 'vosk_recognizer_accept_waveform_f');
    vosk_recognizer_result := GetProcAddress(Lib, 'vosk_recognizer_result');
    vosk_recognizer_partial_result := GetProcAddress(Lib, 'vosk_recognizer_partial_result');
    vosk_recognizer_final_result := GetProcAddress(Lib, 'vosk_recognizer_final_result');
    vosk_recognizer_reset := GetProcAddress(Lib, 'vosk_recognizer_reset');
    vosk_recognizer_free := GetProcAddress(Lib, 'vosk_recognizer_free');
    vosk_set_log_level := GetProcAddress(Lib, 'vosk_set_log_level');
    vosk_gpu_init := GetProcAddress(Lib, 'vosk_gpu_init');
    vosk_gpu_thread_init := GetProcAddress(Lib, 'vosk_gpu_thread_init');
  end;
end;

initialization
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  LoadLibrary;

end.


