unit whisper;

{$mode objfpc}{$H+}
{$macro on}
{$ifdef windows}
  {$define WHISPERCALL:=cdecl}
  {$define WHISPERLIB:='libwhisper.dll'}
{$else}
  {$define WHISPERCALL:=cdecl}
  {$define WHISPERLIB:='libwhisper.so'}
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

  Twhisper_model_loader_func_read = function(ctx: Pointer; output: Pointer; read_size: size_t): size_t; WHISPERCALL;
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

function whisper_init_from_file(path_model: PChar): Pwhisper_context; WHISPERCALL; external WHISPERLIB;
function whisper_init_from_buffer(buffer: Pointer; buffer_size: size_t): Pwhisper_context; WHISPERCALL; external WHISPERLIB;
function whisper_init(loader: Pwhisper_model_loader): Pwhisper_context; WHISPERCALL; external WHISPERLIB;

function whisper_init_from_file_no_state(const path_model: PChar): Pwhisper_context; WHISPERCALL; external WHISPERLIB;
function whisper_init_from_buffer_no_state(buffer: Pointer; buffer_size: SizeUInt): Pwhisper_context; WHISPERCALL; external WHISPERLIB;
function whisper_init_no_state(loader: Pwhisper_model_loader): Pwhisper_context; WHISPERCALL; external WHISPERLIB;

function whisper_init_state(ctx: Pwhisper_context): Pwhisper_state; WHISPERCALL; external WHISPERLIB;

function whisper_ctx_init_openvino_encoder(ctx: Pwhisper_context; model_path, device, cache_dir: PChar): cint; WHISPERCALL; external WHISPERLIB;

procedure whisper_free(ctx: Pwhisper_context); WHISPERCALL; external WHISPERLIB;
procedure whisper_free_state(state: Pwhisper_state); WHISPERCALL; external WHISPERLIB;
procedure whisper_free_params(params: Pwhisper_full_params); WHISPERCALL; external WHISPERLIB;

function whisper_pcm_to_mel(ctx: Pwhisper_context; samples: pcfloat; n_samples, n_threads: cint): cint; WHISPERCALL; external WHISPERLIB;
function whisper_pcm_to_mel_with_state(ctx: Pwhisper_context; state: Pwhisper_state; samples: pcfloat; n_samples, n_threads: cint): cint; WHISPERCALL; external WHISPERLIB;

function whisper_pcm_to_mel_phase_vocoder(ctx: Pwhisper_context; samples: pcfloat; n_samples, n_threads: cint): cint; WHISPERCALL; external WHISPERLIB;
function whisper_pcm_to_mel_phase_vocoder_with_state(ctx: Pwhisper_context; state: Pwhisper_state; samples: pcfloat; n_samples, n_threads: cint): cint; WHISPERCALL; external WHISPERLIB;

function whisper_set_mel(ctx: Pwhisper_context; data: pcfloat; n_len, n_mel: cint): cint; WHISPERCALL; external WHISPERLIB;
function whisper_set_mel_with_state(ctx: Pwhisper_context; state: Pwhisper_state; data: pcfloat; n_len, n_mel: cint): cint; WHISPERCALL; external WHISPERLIB;

function whisper_encode(ctx: Pwhisper_context; offset, n_threads: cint): cint; WHISPERCALL; external WHISPERLIB;
function whisper_encode_with_state(ctx: Pwhisper_context; state: Pwhisper_state; offset, n_threads: cint): cint; WHISPERCALL; external WHISPERLIB;

function whisper_decode(ctx: Pwhisper_context; tokens: Pwhisper_token; n_tokens, n_past, n_threads: cint): cint; WHISPERCALL; external WHISPERLIB;
function whisper_decode_with_state(ctx: Pwhisper_context; state: Pwhisper_state; tokens: Pwhisper_token; n_tokens, n_past, n_threads: cint): cint; WHISPERCALL; external WHISPERLIB;

function whisper_tokenize(ctx: Pwhisper_context; text: PChar; tokens: Pwhisper_token; n_max_tokens: cint): cint; WHISPERCALL; external WHISPERLIB;

function whisper_lang_max_id: cint; WHISPERCALL; external WHISPERLIB;
function whisper_lang_id(lang: PChar): cint; WHISPERCALL; external WHISPERLIB;
function whisper_lang_str(id: cint): PChar; WHISPERCALL; external WHISPERLIB;

function whisper_lang_auto_detect(ctx: Pwhisper_context; offset_ms, n_threads: cint; lang_probs: pcfloat): cint; WHISPERCALL; external WHISPERLIB;
function whisper_lang_auto_detect_with_state(ctx: Pwhisper_context; state: Pwhisper_state; offset_ms, n_threads: cint; lang_probs: pcfloat): cint; WHISPERCALL; external WHISPERLIB;

function whisper_n_len(ctx: Pwhisper_context): cint; WHISPERCALL; external WHISPERLIB;
function whisper_n_len_from_state(state: Pwhisper_state): cint; WHISPERCALL; external WHISPERLIB;
function whisper_n_vocab(ctx: Pwhisper_context): cint; WHISPERCALL; external WHISPERLIB;
function whisper_n_text_ctx(ctx: Pwhisper_context): cint; WHISPERCALL; external WHISPERLIB;
function whisper_n_audio_ctx(ctx: Pwhisper_context): cint; WHISPERCALL; external WHISPERLIB;
function whisper_is_multilingual(ctx: Pwhisper_context): cint; WHISPERCALL; external WHISPERLIB;
function whisper_model_n_vocab(ctx: Pwhisper_context): cint; WHISPERCALL; external WHISPERLIB;
function whisper_model_n_audio_ctx(ctx: Pwhisper_context): cint; WHISPERCALL; external WHISPERLIB;
function whisper_model_n_audio_state(ctx: Pwhisper_context): cint; WHISPERCALL; external WHISPERLIB;
function whisper_model_n_audio_head(ctx: Pwhisper_context): cint; WHISPERCALL; external WHISPERLIB;
function whisper_model_n_audio_layer(ctx: Pwhisper_context): cint; WHISPERCALL; external WHISPERLIB;
function whisper_model_n_text_ctx(ctx: Pwhisper_context): cint; WHISPERCALL; external WHISPERLIB;
function whisper_model_n_text_state(ctx: Pwhisper_context): cint; WHISPERCALL; external WHISPERLIB;
function whisper_model_n_text_head(ctx: Pwhisper_context): cint; WHISPERCALL; external WHISPERLIB;
function whisper_model_n_text_layer(ctx: Pwhisper_context): cint; WHISPERCALL; external WHISPERLIB;
function whisper_model_n_mels(ctx: Pwhisper_context): cint; WHISPERCALL; external WHISPERLIB;
function whisper_model_ftype(ctx: Pwhisper_context): cint; WHISPERCALL; external WHISPERLIB;
function whisper_model_type(ctx: Pwhisper_context): cint; WHISPERCALL; external WHISPERLIB;

function whisper_get_logits(ctx: Pwhisper_context): pcfloat; WHISPERCALL; external WHISPERLIB;
function whisper_get_logits_from_state(state: Pwhisper_state): pcfloat; WHISPERCALL; external WHISPERLIB;

function whisper_token_to_str(ctx: Pwhisper_context; token: Twhisper_token): PChar; WHISPERCALL; external WHISPERLIB;
function whisper_model_type_readable(ctx: Pwhisper_context): PChar; WHISPERCALL; external WHISPERLIB;

function whisper_token_eot(ctx: Pwhisper_context): Twhisper_token; WHISPERCALL; external WHISPERLIB;
function whisper_token_sot(ctx: Pwhisper_context): Twhisper_token; WHISPERCALL; external WHISPERLIB;
function whisper_token_solm(ctx: Pwhisper_context): Twhisper_token; WHISPERCALL; external WHISPERLIB;
function whisper_token_prev(ctx: Pwhisper_context): Twhisper_token; WHISPERCALL; external WHISPERLIB;
function whisper_token_nosp(ctx: Pwhisper_context): Twhisper_token; WHISPERCALL; external WHISPERLIB;
function whisper_token_not(ctx: Pwhisper_context): Twhisper_token; WHISPERCALL; external WHISPERLIB;
function whisper_token_beg(ctx: Pwhisper_context): Twhisper_token; WHISPERCALL; external WHISPERLIB;
function whisper_token_lang(ctx: Pwhisper_context; lang_id: cint): Twhisper_token; WHISPERCALL; external WHISPERLIB;

function whisper_token_translate(ctx: Pwhisper_context): Twhisper_token; WHISPERCALL; external WHISPERLIB;
function whisper_token_transcribe(ctx: Pwhisper_context): Twhisper_token; WHISPERCALL; external WHISPERLIB;

procedure whisper_print_timings(ctx: Pwhisper_context); WHISPERCALL; external WHISPERLIB;
procedure whisper_reset_timings(ctx: Pwhisper_context); WHISPERCALL; external WHISPERLIB;

function whisper_print_system_info: PChar; WHISPERCALL; external WHISPERLIB;

//

function whisper_full_default_params_by_ref(strategy: Twhisper_sampling_strategy): Pwhisper_full_params; WHISPERCALL; external WHISPERLIB;
function whisper_full_default_params(strategy: Twhisper_sampling_strategy): Twhisper_full_params; WHISPERCALL; external WHISPERLIB;
function whisper_full(ctx: Pwhisper_context; params: Twhisper_full_params; samples: pcfloat; n_samples: cint): cint; WHISPERCALL; external WHISPERLIB;
function whisper_full_with_state(ctx: Pwhisper_context; state: Pwhisper_state; params: Twhisper_full_params; samples: pcfloat; n_samples: cint): cint; WHISPERCALL; external WHISPERLIB;
function whisper_full_parallel(ctx: Pwhisper_context; params: Twhisper_full_params; samples: pcfloat; n_samples: cint; n_processors: cint): cint; WHISPERCALL; external WHISPERLIB;
function whisper_full_n_segments(ctx: Pwhisper_context): cint; WHISPERCALL; external WHISPERLIB;
function whisper_full_n_segments_from_state(state: Pwhisper_state): cint; WHISPERCALL; external WHISPERLIB;
function whisper_full_lang_id(ctx: Pwhisper_context): cint; WHISPERCALL; external WHISPERLIB;
function whisper_full_lang_id_from_state(state: Pwhisper_state): cint; WHISPERCALL; external WHISPERLIB;
function whisper_full_get_segment_t0(ctx: Pwhisper_context; i_segment: cint): cint64; WHISPERCALL; external WHISPERLIB;
function whisper_full_get_segment_t0_from_state(state: Pwhisper_state; i_segment: cint): cint64; WHISPERCALL; external WHISPERLIB;
function whisper_full_get_segment_t1(ctx: Pwhisper_context; i_segment: cint): cint64; WHISPERCALL; external WHISPERLIB;
function whisper_full_get_segment_t1_from_state(state: Pwhisper_state; i_segment: cint): cint64; WHISPERCALL; external WHISPERLIB;
function whisper_full_get_segment_speaker_turn_next(ctx: Pwhisper_context; i_segment: cint): cbool; WHISPERCALL; external WHISPERLIB;
function whisper_full_get_segment_text(ctx: Pwhisper_context; i_segment: cint): PChar; WHISPERCALL; external WHISPERLIB;
function whisper_full_get_segment_text_from_state(state: Pwhisper_state; i_segment: cint): PChar; WHISPERCALL; external WHISPERLIB;
function whisper_full_n_tokens(ctx: Pwhisper_context; i_segment: cint): cint; WHISPERCALL; external WHISPERLIB;
function whisper_full_n_tokens_from_state(state: Pwhisper_state; i_segment: cint): cint; WHISPERCALL; external WHISPERLIB;
function whisper_full_get_token_text(ctx: Pwhisper_context; i_segment, i_token: cint): PChar; WHISPERCALL; external WHISPERLIB;
function whisper_full_get_token_text_from_state(ctx: Pwhisper_context; state: Pwhisper_state; i_segment, i_token: cint): PChar; WHISPERCALL; external WHISPERLIB;
function whisper_full_get_token_id(ctx: Pwhisper_context; i_segment, i_token: cint): Twhisper_token; WHISPERCALL; external WHISPERLIB;
function whisper_full_get_token_id_from_state(state: Pwhisper_state; i_segment, i_token: cint): Twhisper_token; WHISPERCALL; external WHISPERLIB;
function whisper_full_get_token_data(ctx: Pwhisper_context; i_segment, i_token: cint): Twhisper_token_data; WHISPERCALL; external WHISPERLIB;
function whisper_full_get_token_data_from_state(state: Pwhisper_state; i_segment, i_token: cint): Twhisper_token_data; WHISPERCALL; external WHISPERLIB;
function whisper_full_get_token_p(ctx: Pwhisper_context; i_segment, i_token: cint): cfloat; WHISPERCALL; external WHISPERLIB;
function whisper_full_get_token_p_from_state(state: Pwhisper_state; i_segment, i_token: cint): cfloat; WHISPERCALL; external WHISPERLIB;

implementation

initialization
  SetExceptionMask(GetExceptionMask + [exOverflow, exInvalidOp]);

end.