unit whisper;

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}
{$macro on}
{$ifdef windows}
  {$define WHISPERCALL:=cdecl}
{$else}
  {$define WHISPERCALL:=cdecl}
{$endif}
{$packrecords C}
{$packenum 4}

interface

uses
  ctypes, Math, dynlibs;

const
  WHISPER_SAMPLE_RATE = 16000;
  WHISPER_N_FFT       = 400;
  WHISPER_N_MEL       = 80;
  WHISPER_HOP_LENGTH  = 160;
  WHISPER_CHUNK_SIZE  = 30;
  {$ifdef windows}
  WHISPER_LIB         = 'libwhisper.dll';
  {$else}
  WHISPER_LIB         = 'libwhisper.so';
  {$endif}

type
  Twhisper_sampling_strategy = (
    WHISPER_SAMPLING_GREEDY,      // similar to OpenAI's GreedyDecoder
    WHISPER_SAMPLING_BEAM_SEARCH  // similar to OpenAI's BeamSearchDecoder
  );

  Twhisper_token = cint;
  Pwhisper_token = ^Twhisper_token;

  Twhisper_context = record
    // Internal C++ struct
  end;
  Pwhisper_context = ^Twhisper_context;
  Pwhisper_state = Pointer;

  Twhisper_token_data = record
    id,
    tid: Twhisper_token;
    p,
    plog,
    pt,
    ptsum: cfloat;
    t0,
    t1: cint64;
  end;
  Pwhisper_token_data = ^Twhisper_token_data;

  Twhisper_model_loader_func_read = function(ctx: Pointer; output: Pointer; read_size: csize_t): csize_t; WHISPERCALL;
  Twhisper_model_loader_func_eof = function(ctx: Pointer): cbool; WHISPERCALL;
  Twhisper_model_loader_func_close = procedure(ctx: Pointer); WHISPERCALL;
  Twhisper_model_loader = record
    context: Pointer;
    fread: Twhisper_model_loader_func_read;
    feof: Twhisper_model_loader_func_eof;
    fclose: Twhisper_model_loader_func_close;
  end;
  Pwhisper_model_loader = ^Twhisper_model_loader;

  Twhisper_new_segment_callback = procedure(ctx: Pwhisper_context; state: Pwhisper_state; n_new: cint; user_data: Pointer); WHISPERCALL;
  Twhisper_progress_callback = procedure(ctx: Pwhisper_context; state: Pwhisper_state; progress: cint; user_data: Pointer); WHISPERCALL;
  Twhisper_encoder_begin_callback = function(ctx: Pwhisper_context; state: Pwhisper_state; user_data: Pointer): cbool; WHISPERCALL;
  Twhisper_logits_filter_callback = procedure(ctx: Pwhisper_context; state: Pwhisper_state; tokens: Pwhisper_token_data; n_tokens: cint; logits: Pcfloat; user_data: Pointer); WHISPERCALL;
  Twhisper_full_params = record
    strategy: Twhisper_sampling_strategy;
    n_threads: cint;
    n_max_text_ctx: cint;
    offset_ms: cint;
    duration_ms: cint;
    translate: cbool;
    no_context: cbool;
    cfloat_segment: cbool;
    print_special: cbool;
    print_progress: cbool;
    print_realtime: cbool;
    print_timestamps: cbool;
    token_timestamps: cbool;
    thold_pt: cfloat;
    thold_ptsum: cfloat;
    max_len: cint;
    split_on_word: cbool;
    max_tokens: cint;
    speed_up: cbool;
    debug_mode: cbool;
    audio_ctx: cint;
    tdrz_enable: cbool;
    initial_prompt: PChar;
    prompt_tokens: Pwhisper_token;
    prompt_n_tokens: cint;
    language: PChar;
    detect_language: cbool;
    suppress_blank: cbool;
    suppress_non_speech_tokens: cbool;
    temperature: cfloat;
    max_initial_ts: cfloat;
    length_penalty: cfloat;
    temperature_inc: cfloat;
    entropy_thold: cfloat;
    logprob_thold: cfloat;
    no_speech_thold: cfloat;
    greedy: record
      best_of: cint;
    end;
    beam_search: record
      beam_size: cint;
      patience: cfloat;
    end;
    new_segment_callback: Twhisper_new_segment_callback;
    new_segment_callback_user_data: Pointer;
    progress_callback: Twhisper_progress_callback;
    progress_callback_user_data: Pointer;
    encoder_begin_callback: Twhisper_encoder_begin_callback;
    encoder_begin_callback_user_data: Pointer;
    logits_filter_callback: Twhisper_logits_filter_callback;
    logits_filter_callback_user_data: Pointer;
  end;
  Pwhisper_full_params = ^Twhisper_full_params;

var
  whisper_init_from_file: function(path_model: PChar): Pwhisper_context; WHISPERCALL;
  whisper_init_from_buffer: function(buffer: Pointer; buffer_size: csize_t): Pwhisper_context; WHISPERCALL;
  whisper_init: function(loader: Pwhisper_model_loader): Pwhisper_context; WHISPERCALL;

  whisper_init_from_file_no_state: function(const path_model: PChar): Pwhisper_context; WHISPERCALL;
  whisper_init_from_buffer_no_state: function(buffer: Pointer; buffer_size: SizeUInt): Pwhisper_context; WHISPERCALL;
  whisper_init_no_state: function(loader: Pwhisper_model_loader): Pwhisper_context; WHISPERCALL;

  whisper_init_state: function(ctx: Pwhisper_context): Pwhisper_state; WHISPERCALL;

  whisper_ctx_init_openvino_encoder: function(ctx: Pwhisper_context; model_path, device, cache_dir: PChar): cint; WHISPERCALL;

  whisper_free: procedure(ctx: Pwhisper_context); WHISPERCALL;
  whisper_free_state: procedure(state: Pwhisper_state); WHISPERCALL;
  whisper_free_params: procedure(params: Pwhisper_full_params); WHISPERCALL;

  whisper_pcm_to_mel: function(ctx: Pwhisper_context; samples: pcfloat; n_samples, n_threads: cint): cint; WHISPERCALL;
  whisper_pcm_to_mel_with_state: function(ctx: Pwhisper_context; state: Pwhisper_state; samples: pcfloat; n_samples, n_threads: cint): cint; WHISPERCALL;

  whisper_pcm_to_mel_phase_vocoder: function(ctx: Pwhisper_context; samples: pcfloat; n_samples, n_threads: cint): cint; WHISPERCALL;
  whisper_pcm_to_mel_phase_vocoder_with_state: function(ctx: Pwhisper_context; state: Pwhisper_state; samples: pcfloat; n_samples, n_threads: cint): cint; WHISPERCALL;

  whisper_set_mel: function(ctx: Pwhisper_context; data: pcfloat; n_len, n_mel: cint): cint; WHISPERCALL;
  whisper_set_mel_with_state: function(ctx: Pwhisper_context; state: Pwhisper_state; data: pcfloat; n_len, n_mel: cint): cint; WHISPERCALL;

  whisper_encode: function(ctx: Pwhisper_context; offset, n_threads: cint): cint; WHISPERCALL;
  whisper_encode_with_state: function(ctx: Pwhisper_context; state: Pwhisper_state; offset, n_threads: cint): cint; WHISPERCALL;

  whisper_decode: function(ctx: Pwhisper_context; tokens: Pwhisper_token; n_tokens, n_past, n_threads: cint): cint; WHISPERCALL;
  whisper_decode_with_state: function(ctx: Pwhisper_context; state: Pwhisper_state; tokens: Pwhisper_token; n_tokens, n_past, n_threads: cint): cint; WHISPERCALL;

  whisper_tokenize: function(ctx: Pwhisper_context; text: PChar; tokens: Pwhisper_token; n_max_tokens: cint): cint; WHISPERCALL;

  whisper_lang_max_id: function : cint; WHISPERCALL;
  whisper_lang_id: function(lang: PChar): cint; WHISPERCALL;
  whisper_lang_str: function(id: cint): PChar; WHISPERCALL;

  whisper_lang_auto_detect: function(ctx: Pwhisper_context; offset_ms, n_threads: cint; lang_probs: pcfloat): cint; WHISPERCALL;
  whisper_lang_auto_detect_with_state: function(ctx: Pwhisper_context; state: Pwhisper_state; offset_ms, n_threads: cint; lang_probs: pcfloat): cint; WHISPERCALL;

  whisper_n_len: function(ctx: Pwhisper_context): cint; WHISPERCALL;
  whisper_n_len_from_state: function(state: Pwhisper_state): cint; WHISPERCALL;
  whisper_n_vocab: function(ctx: Pwhisper_context): cint; WHISPERCALL;
  whisper_n_text_ctx: function(ctx: Pwhisper_context): cint; WHISPERCALL;
  whisper_n_audio_ctx: function(ctx: Pwhisper_context): cint; WHISPERCALL;
  whisper_is_multilingual: function(ctx: Pwhisper_context): cint; WHISPERCALL;
  whisper_model_n_vocab: function(ctx: Pwhisper_context): cint; WHISPERCALL;
  whisper_model_n_audio_ctx: function(ctx: Pwhisper_context): cint; WHISPERCALL;
  whisper_model_n_audio_state: function(ctx: Pwhisper_context): cint; WHISPERCALL;
  whisper_model_n_audio_head: function(ctx: Pwhisper_context): cint; WHISPERCALL;
  whisper_model_n_audio_layer: function(ctx: Pwhisper_context): cint; WHISPERCALL;
  whisper_model_n_text_ctx: function(ctx: Pwhisper_context): cint; WHISPERCALL;
  whisper_model_n_text_state: function(ctx: Pwhisper_context): cint; WHISPERCALL;
  whisper_model_n_text_head: function(ctx: Pwhisper_context): cint; WHISPERCALL;
  whisper_model_n_text_layer: function(ctx: Pwhisper_context): cint; WHISPERCALL;
  whisper_model_n_mels: function(ctx: Pwhisper_context): cint; WHISPERCALL;
  whisper_model_ftype: function(ctx: Pwhisper_context): cint; WHISPERCALL;
  whisper_model_type: function(ctx: Pwhisper_context): cint; WHISPERCALL;

  whisper_get_logits: function(ctx: Pwhisper_context): pcfloat; WHISPERCALL;
  whisper_get_logits_from_state: function(state: Pwhisper_state): pcfloat; WHISPERCALL;

  whisper_token_to_str: function(ctx: Pwhisper_context; token: Twhisper_token): PChar; WHISPERCALL;
  whisper_model_type_readable: function(ctx: Pwhisper_context): PChar; WHISPERCALL;

  whisper_token_eot: function(ctx: Pwhisper_context): Twhisper_token; WHISPERCALL;
  whisper_token_sot: function(ctx: Pwhisper_context): Twhisper_token; WHISPERCALL;
  whisper_token_solm: function(ctx: Pwhisper_context): Twhisper_token; WHISPERCALL;
  whisper_token_prev: function(ctx: Pwhisper_context): Twhisper_token; WHISPERCALL;
  whisper_token_nosp: function(ctx: Pwhisper_context): Twhisper_token; WHISPERCALL;
  whisper_token_not: function(ctx: Pwhisper_context): Twhisper_token; WHISPERCALL;
  whisper_token_beg: function(ctx: Pwhisper_context): Twhisper_token; WHISPERCALL;
  whisper_token_lang: function(ctx: Pwhisper_context; lang_id: cint): Twhisper_token; WHISPERCALL;

  whisper_token_translate: function(ctx: Pwhisper_context): Twhisper_token; WHISPERCALL;
  whisper_token_transcribe: function(ctx: Pwhisper_context): Twhisper_token; WHISPERCALL;

  whisper_print_timings: procedure(ctx: Pwhisper_context); WHISPERCALL;
  whisper_reset_timings: procedure(ctx: Pwhisper_context); WHISPERCALL;

  whisper_print_system_info: function : PChar; WHISPERCALL;

//

  whisper_full_default_params_by_ref: function(strategy: Twhisper_sampling_strategy): Pwhisper_full_params; WHISPERCALL;
  whisper_full_default_params: function(strategy: Twhisper_sampling_strategy): Twhisper_full_params; WHISPERCALL;
  whisper_full: function(ctx: Pwhisper_context; params: Twhisper_full_params; samples: pcfloat; n_samples: cint): cint; WHISPERCALL;
  whisper_full_with_state: function(ctx: Pwhisper_context; state: Pwhisper_state; params: Twhisper_full_params; samples: pcfloat; n_samples: cint): cint; WHISPERCALL;
  whisper_full_parallel: function(ctx: Pwhisper_context; params: Twhisper_full_params; samples: pcfloat; n_samples: cint; n_processors: cint): cint; WHISPERCALL;
  whisper_full_n_segments: function(ctx: Pwhisper_context): cint; WHISPERCALL;
  whisper_full_n_segments_from_state: function(state: Pwhisper_state): cint; WHISPERCALL;
  whisper_full_lang_id: function(ctx: Pwhisper_context): cint; WHISPERCALL;
  whisper_full_lang_id_from_state: function(state: Pwhisper_state): cint; WHISPERCALL;
  whisper_full_get_segment_t0: function(ctx: Pwhisper_context; i_segment: cint): cint64; WHISPERCALL;
  whisper_full_get_segment_t0_from_state: function(state: Pwhisper_state; i_segment: cint): cint64; WHISPERCALL;
  whisper_full_get_segment_t1: function(ctx: Pwhisper_context; i_segment: cint): cint64; WHISPERCALL;
  whisper_full_get_segment_t1_from_state: function(state: Pwhisper_state; i_segment: cint): cint64; WHISPERCALL;
  whisper_full_get_segment_speaker_turn_next: function(ctx: Pwhisper_context; i_segment: cint): cbool; WHISPERCALL;
  whisper_full_get_segment_text: function(ctx: Pwhisper_context; i_segment: cint): PChar; WHISPERCALL;
  whisper_full_get_segment_text_from_state: function(state: Pwhisper_state; i_segment: cint): PChar; WHISPERCALL;
  whisper_full_n_tokens: function(ctx: Pwhisper_context; i_segment: cint): cint; WHISPERCALL;
  whisper_full_n_tokens_from_state: function(state: Pwhisper_state; i_segment: cint): cint; WHISPERCALL;
  whisper_full_get_token_text: function(ctx: Pwhisper_context; i_segment, i_token: cint): PChar; WHISPERCALL;
  whisper_full_get_token_text_from_state: function(ctx: Pwhisper_context; state: Pwhisper_state; i_segment, i_token: cint): PChar; WHISPERCALL;
  whisper_full_get_token_id: function(ctx: Pwhisper_context; i_segment, i_token: cint): Twhisper_token; WHISPERCALL;
  whisper_full_get_token_id_from_state: function(state: Pwhisper_state; i_segment, i_token: cint): Twhisper_token; WHISPERCALL;
  whisper_full_get_token_data: function(ctx: Pwhisper_context; i_segment, i_token: cint): Twhisper_token_data; WHISPERCALL;
  whisper_full_get_token_data_from_state: function(state: Pwhisper_state; i_segment, i_token: cint): Twhisper_token_data; WHISPERCALL;
  whisper_full_get_token_p: function(ctx: Pwhisper_context; i_segment, i_token: cint): cfloat; WHISPERCALL;
  whisper_full_get_token_p_from_state: function(state: Pwhisper_state; i_segment, i_token: cint): cfloat; WHISPERCALL;
  Lib: TLibHandle;

implementation

procedure LoadLibrary;
begin
  Lib := DynLibs.LoadLibrary(WHISPER_LIB);
  if Lib <> DynLibs.NilHandle then
  begin
    whisper_init_from_file := GetProcAddress(Lib, 'whisper_init_from_file');
    whisper_init_from_buffer := GetProcAddress(Lib, 'whisper_init_from_buffer');
    whisper_init := GetProcAddress(Lib, 'whisper_init');

    whisper_init_from_file_no_state := GetProcAddress(Lib, 'whisper_init_from_file_no_state');
    whisper_init_from_buffer_no_state := GetProcAddress(Lib, 'whisper_init_from_buffer_no_state');
    whisper_init_no_state := GetProcAddress(Lib, 'whisper_init_no_state');

    whisper_init_state := GetProcAddress(Lib, 'whisper_init_state');

    whisper_ctx_init_openvino_encoder := GetProcAddress(Lib, 'whisper_ctx_init_openvino_encoder');

    whisper_free := GetProcAddress(Lib, 'whisper_free');
    whisper_free_state := GetProcAddress(Lib, 'whisper_free_state');
    whisper_free_params := GetProcAddress(Lib, 'whisper_free_params');

    whisper_pcm_to_mel := GetProcAddress(Lib, 'whisper_pcm_to_mel');
    whisper_pcm_to_mel_with_state := GetProcAddress(Lib, 'whisper_pcm_to_mel_with_state');

    whisper_pcm_to_mel_phase_vocoder := GetProcAddress(Lib, 'whisper_pcm_to_mel_phase_vocoder');
    whisper_pcm_to_mel_phase_vocoder_with_state := GetProcAddress(Lib, 'whisper_pcm_to_mel_phase_vocoder_with_state');

    whisper_set_mel := GetProcAddress(Lib, 'whisper_set_mel');
    whisper_set_mel_with_state := GetProcAddress(Lib, 'whisper_set_mel_with_state');

    whisper_encode := GetProcAddress(Lib, 'whisper_encode');
    whisper_encode_with_state := GetProcAddress(Lib, 'whisper_encode_with_state');

    whisper_decode := GetProcAddress(Lib, 'whisper_decode');
    whisper_decode_with_state := GetProcAddress(Lib, 'whisper_decode_with_state');

    whisper_tokenize := GetProcAddress(Lib, 'whisper_tokenize');

    whisper_lang_max_id := GetProcAddress(Lib, 'whisper_lang_max_id');
    whisper_lang_id := GetProcAddress(Lib, 'whisper_lang_id');
    whisper_lang_str := GetProcAddress(Lib, 'whisper_lang_str');

    whisper_lang_auto_detect := GetProcAddress(Lib, 'whisper_lang_auto_detect');
    whisper_lang_auto_detect_with_state := GetProcAddress(Lib, 'whisper_lang_auto_detect_with_state');

    whisper_n_len := GetProcAddress(Lib, 'whisper_n_len');
    whisper_n_len_from_state := GetProcAddress(Lib, 'whisper_n_len_from_state');
    whisper_n_vocab := GetProcAddress(Lib, 'whisper_n_vocab');
    whisper_n_text_ctx := GetProcAddress(Lib, 'whisper_n_text_ctx');
    whisper_n_audio_ctx := GetProcAddress(Lib, 'whisper_n_audio_ctx');
    whisper_is_multilingual := GetProcAddress(Lib, 'whisper_is_multilingual');
    whisper_model_n_vocab := GetProcAddress(Lib, 'whisper_model_n_vocab');
    whisper_model_n_audio_ctx := GetProcAddress(Lib, 'whisper_model_n_audio_ctx');
    whisper_model_n_audio_state := GetProcAddress(Lib, 'whisper_model_n_audio_state');
    whisper_model_n_audio_head := GetProcAddress(Lib, 'whisper_model_n_audio_head');
    whisper_model_n_audio_layer := GetProcAddress(Lib, 'whisper_model_n_audio_layer');
    whisper_model_n_text_ctx := GetProcAddress(Lib, 'whisper_model_n_text_ctx');
    whisper_model_n_text_state := GetProcAddress(Lib, 'whisper_model_n_text_state');
    whisper_model_n_text_head := GetProcAddress(Lib, 'whisper_model_n_text_head');
    whisper_model_n_text_layer := GetProcAddress(Lib, 'whisper_model_n_text_layer');
    whisper_model_n_mels := GetProcAddress(Lib, 'whisper_model_n_mels');
    whisper_model_ftype := GetProcAddress(Lib, 'whisper_model_ftype');
    whisper_model_type := GetProcAddress(Lib, 'whisper_model_type');

    whisper_get_logits := GetProcAddress(Lib, 'whisper_get_logits');
    whisper_get_logits_from_state := GetProcAddress(Lib, 'whisper_get_logits_from_state');

    whisper_token_to_str := GetProcAddress(Lib, 'whisper_token_to_str');
    whisper_model_type_readable := GetProcAddress(Lib, 'whisper_model_type_readable');

    whisper_token_eot := GetProcAddress(Lib, 'whisper_token_eot');
    whisper_token_sot := GetProcAddress(Lib, 'whisper_token_sot');
    whisper_token_solm := GetProcAddress(Lib, 'whisper_token_solm');
    whisper_token_prev := GetProcAddress(Lib, 'whisper_token_prev');
    whisper_token_nosp := GetProcAddress(Lib, 'whisper_token_nosp');
    whisper_token_not := GetProcAddress(Lib, 'whisper_token_not');
    whisper_token_beg := GetProcAddress(Lib, 'whisper_token_beg');
    whisper_token_lang := GetProcAddress(Lib, 'whisper_token_lang');

    whisper_token_translate := GetProcAddress(Lib, 'whisper_token_translate');
    whisper_token_transcribe := GetProcAddress(Lib, 'whisper_token_transcribe');

    whisper_print_timings := GetProcAddress(Lib, 'whisper_print_timings');
    whisper_reset_timings := GetProcAddress(Lib, 'whisper_reset_timings');

    whisper_print_system_info := GetProcAddress(Lib, 'whisper_print_system_info');

    whisper_full_default_params_by_ref := GetProcAddress(Lib, 'whisper_full_default_params_by_ref');
    whisper_full_default_params := GetProcAddress(Lib, 'whisper_full_default_params');
    whisper_full := GetProcAddress(Lib, 'whisper_full');
    whisper_full_with_state := GetProcAddress(Lib, 'whisper_full_with_state');
    whisper_full_parallel := GetProcAddress(Lib, 'whisper_full_parallel');
    whisper_full_n_segments := GetProcAddress(Lib, 'whisper_full_n_segments');
    whisper_full_n_segments_from_state := GetProcAddress(Lib, 'whisper_full_n_segments_from_state');
    whisper_full_lang_id := GetProcAddress(Lib, 'whisper_full_lang_id');
    whisper_full_lang_id_from_state := GetProcAddress(Lib, 'whisper_full_lang_id_from_state');
    whisper_full_get_segment_t0 := GetProcAddress(Lib, 'whisper_full_get_segment_t0');
    whisper_full_get_segment_t0_from_state := GetProcAddress(Lib, 'whisper_full_get_segment_t0_from_state');
    whisper_full_get_segment_t1 := GetProcAddress(Lib, 'whisper_full_get_segment_t1');
    whisper_full_get_segment_t1_from_state := GetProcAddress(Lib, 'whisper_full_get_segment_t1_from_state');
    whisper_full_get_segment_speaker_turn_next := GetProcAddress(Lib, 'whisper_full_get_segment_speaker_turn_next');
    whisper_full_get_segment_text := GetProcAddress(Lib, 'whisper_full_get_segment_text');
    whisper_full_get_segment_text_from_state := GetProcAddress(Lib, 'whisper_full_get_segment_text_from_state');
    whisper_full_n_tokens := GetProcAddress(Lib, 'whisper_full_n_tokens');
    whisper_full_n_tokens_from_state := GetProcAddress(Lib, 'whisper_full_n_tokens_from_state');
    whisper_full_get_token_text := GetProcAddress(Lib, 'whisper_full_get_token_text');
    whisper_full_get_token_text_from_state := GetProcAddress(Lib, 'whisper_full_get_token_text_from_state');
    whisper_full_get_token_id := GetProcAddress(Lib, 'whisper_full_get_token_id');
    whisper_full_get_token_id_from_state := GetProcAddress(Lib, 'whisper_full_get_token_id_from_state');
    whisper_full_get_token_data := GetProcAddress(Lib, 'whisper_full_get_token_data');
    whisper_full_get_token_data_from_state := GetProcAddress(Lib, 'whisper_full_get_token_data_from_state');
    whisper_full_get_token_p := GetProcAddress(Lib, 'whisper_full_get_token_p');
    whisper_full_get_token_p_from_state := GetProcAddress(Lib, 'whisper_full_get_token_p_from_state');
  end;
end;

initialization
  SetExceptionMask(GetExceptionMask + [exOverflow, exInvalidOp]);
  LoadLibrary;

end.
