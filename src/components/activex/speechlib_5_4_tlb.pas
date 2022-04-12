Unit SpeechLib_5_4_TLB;

//  Imported SpeechLib on 2/28/2022 0:35:12 from C:\Windows\SysWOW64\Speech\Common\sapi.dll

{$mode delphi}{$H+}

interface

//  Warning: renamed record member 'Type' in tagSTATSTG to 'Type_'
// Dependency: stdole v2 (stdole2.pas)
//  Warning: 'GUID' not automatable in ISpeechDataKeydisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechDataKeydisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechDataKeydisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechDataKeydisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechDataKeydisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechDataKeydisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechDataKeydisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechDataKeydisp.Invoke
//  Warning: renamed parameter 'Object' in ISpeechObjectToken.IsUISupported to 'Object_'
//  Warning: renamed parameter 'Object' in ISpeechObjectToken.DisplayUI to 'Object_'
//  Warning: 'GUID' not automatable in ISpeechObjectTokendisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechObjectTokendisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechObjectTokendisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechObjectTokendisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechObjectTokendisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechObjectTokendisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechObjectTokendisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechObjectTokendisp.Invoke
//  Warning: renamed parameter 'Object' in ISpeechObjectToken.IsUISupported to 'Object_'
//  Warning: renamed parameter 'Object' in ISpeechObjectToken.DisplayUI to 'Object_'
//  Warning: 'GUID' not automatable in ISpeechObjectTokenCategorydisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechObjectTokenCategorydisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechObjectTokenCategorydisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechObjectTokenCategorydisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechObjectTokenCategorydisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechObjectTokenCategorydisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechObjectTokenCategorydisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechObjectTokenCategorydisp.Invoke
//  Warning: 'GUID' not automatable in ISpeechObjectTokensdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechObjectTokensdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechObjectTokensdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechObjectTokensdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechObjectTokensdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechObjectTokensdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechObjectTokensdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechObjectTokensdisp.Invoke
//  Warning: 'GUID' not automatable in ISpeechAudioBufferInfodisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechAudioBufferInfodisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechAudioBufferInfodisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechAudioBufferInfodisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechAudioBufferInfodisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechAudioBufferInfodisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechAudioBufferInfodisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechAudioBufferInfodisp.Invoke
//  Warning: 'GUID' not automatable in ISpeechAudioStatusdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechAudioStatusdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechAudioStatusdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechAudioStatusdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechAudioStatusdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechAudioStatusdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechAudioStatusdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechAudioStatusdisp.Invoke
//  Warning: renamed property 'Type' in ISpeechAudioFormat to 'Type_'
//  Warning: 'GUID' not automatable in ISpeechAudioFormatdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechAudioFormatdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechAudioFormatdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechAudioFormatdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechAudioFormatdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechAudioFormatdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechAudioFormatdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechAudioFormatdisp.Invoke
//  Warning: 'GUID' not automatable in ISpeechWaveFormatExdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechWaveFormatExdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechWaveFormatExdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechWaveFormatExdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechWaveFormatExdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechWaveFormatExdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechWaveFormatExdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechWaveFormatExdisp.Invoke
//  Warning: renamed method 'Read' in ISpeechBaseStream to 'Read_'
//  Warning: renamed method 'Write' in ISpeechBaseStream to 'Write_'
//  Warning: 'GUID' not automatable in ISpeechBaseStreamdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechBaseStreamdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechBaseStreamdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechBaseStreamdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechBaseStreamdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechBaseStreamdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechBaseStreamdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechBaseStreamdisp.Invoke
//  Warning: renamed method 'Read' in ISpeechBaseStream to 'Read_'
//  Warning: renamed method 'Write' in ISpeechBaseStream to 'Write_'
//  Warning: 'GUID' not automatable in ISpeechFileStreamdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechFileStreamdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechFileStreamdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechFileStreamdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechFileStreamdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechFileStreamdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechFileStreamdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechFileStreamdisp.Invoke
//  Warning: renamed method 'Read' in ISpeechFileStream to 'Read_'
//  Warning: renamed method 'Write' in ISpeechFileStream to 'Write_'
//  Warning: 'GUID' not automatable in ISpeechMemoryStreamdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechMemoryStreamdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechMemoryStreamdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechMemoryStreamdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechMemoryStreamdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechMemoryStreamdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechMemoryStreamdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechMemoryStreamdisp.Invoke
//  Warning: renamed method 'Read' in ISpeechMemoryStream to 'Read_'
//  Warning: renamed method 'Write' in ISpeechMemoryStream to 'Write_'
//  Warning: 'GUID' not automatable in ISpeechCustomStreamdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechCustomStreamdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechCustomStreamdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechCustomStreamdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechCustomStreamdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechCustomStreamdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechCustomStreamdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechCustomStreamdisp.Invoke
//  Warning: renamed method 'Read' in ISpeechCustomStream to 'Read_'
//  Warning: renamed method 'Write' in ISpeechCustomStream to 'Write_'
//  Warning: 'GUID' not automatable in ISpeechAudiodisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechAudiodisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechAudiodisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechAudiodisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechAudiodisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechAudiodisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechAudiodisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechAudiodisp.Invoke
//  Warning: renamed method 'Read' in ISpeechAudio to 'Read_'
//  Warning: renamed method 'Write' in ISpeechAudio to 'Write_'
//  Warning: 'GUID' not automatable in ISpeechMMSysAudiodisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechMMSysAudiodisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechMMSysAudiodisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechMMSysAudiodisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechMMSysAudiodisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechMMSysAudiodisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechMMSysAudiodisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechMMSysAudiodisp.Invoke
//  Warning: renamed method 'Read' in ISpeechMMSysAudio to 'Read_'
//  Warning: renamed method 'Write' in ISpeechMMSysAudio to 'Write_'
//  Warning: renamed parameter 'Text' in ISpeechVoice.Speak to 'Text_'
//  Warning: renamed parameter 'Type' in ISpeechVoice.Skip to 'Type_'
//  Warning: 'GUID' not automatable in ISpeechVoicedisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechVoicedisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechVoicedisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechVoicedisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechVoicedisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechVoicedisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechVoicedisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechVoicedisp.Invoke
//  Warning: renamed parameter 'Text' in ISpeechVoice.Speak to 'Text_'
//  Warning: renamed parameter 'Type' in ISpeechVoice.Skip to 'Type_'
//  Warning: 'GUID' not automatable in ISpeechVoiceStatusdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechVoiceStatusdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechVoiceStatusdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechVoiceStatusdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechVoiceStatusdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechVoiceStatusdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechVoiceStatusdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechVoiceStatusdisp.Invoke
//  Warning: renamed parameter 'Type' in ISpeechRecognizer.GetFormat to 'Type_'
//  Warning: 'GUID' not automatable in ISpeechRecognizerdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechRecognizerdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechRecognizerdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechRecognizerdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechRecognizerdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechRecognizerdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechRecognizerdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechRecognizerdisp.Invoke
//  Warning: renamed parameter 'Type' in ISpeechRecognizer.GetFormat to 'Type_'
//  Warning: 'GUID' not automatable in ISpeechRecognizerStatusdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechRecognizerStatusdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechRecognizerStatusdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechRecognizerStatusdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechRecognizerStatusdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechRecognizerStatusdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechRecognizerStatusdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechRecognizerStatusdisp.Invoke
//  Warning: 'GUID' not automatable in ISpeechRecoContextdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechRecoContextdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechRecoContextdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechRecoContextdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechRecoContextdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechRecoContextdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechRecoContextdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechRecoContextdisp.Invoke
//  Warning: renamed method 'Reset' in ISpeechRecoGrammar to 'Reset_'
//  Warning: renamed parameter 'Text' in ISpeechRecoGrammar.SetWordSequenceData to 'Text_'
//  Warning: 'GUID' not automatable in ISpeechRecoGrammardisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechRecoGrammardisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechRecoGrammardisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechRecoGrammardisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechRecoGrammardisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechRecoGrammardisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechRecoGrammardisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechRecoGrammardisp.Invoke
//  Warning: renamed method 'Reset' in ISpeechRecoGrammar to 'Reset_'
//  Warning: renamed parameter 'Text' in ISpeechRecoGrammar.SetWordSequenceData to 'Text_'
//  Warning: 'GUID' not automatable in ISpeechGrammarRulesdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechGrammarRulesdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechGrammarRulesdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechGrammarRulesdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechGrammarRulesdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechGrammarRulesdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechGrammarRulesdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechGrammarRulesdisp.Invoke
//  Warning: 'GUID' not automatable in ISpeechGrammarRuledisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechGrammarRuledisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechGrammarRuledisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechGrammarRuledisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechGrammarRuledisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechGrammarRuledisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechGrammarRuledisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechGrammarRuledisp.Invoke
//  Warning: renamed parameter 'Type' in ISpeechGrammarRuleState.AddWordTransition to 'Type_'
//  Warning: renamed parameter 'Type' in ISpeechGrammarRuleState.AddSpecialTransition to 'Type_'
//  Warning: 'GUID' not automatable in ISpeechGrammarRuleStatedisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechGrammarRuleStatedisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechGrammarRuleStatedisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechGrammarRuleStatedisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechGrammarRuleStatedisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechGrammarRuleStatedisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechGrammarRuleStatedisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechGrammarRuleStatedisp.Invoke
//  Warning: renamed parameter 'Type' in ISpeechGrammarRuleState.AddWordTransition to 'Type_'
//  Warning: renamed parameter 'Type' in ISpeechGrammarRuleState.AddSpecialTransition to 'Type_'
//  Warning: 'GUID' not automatable in ISpeechGrammarRuleStateTransitionsdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechGrammarRuleStateTransitionsdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechGrammarRuleStateTransitionsdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechGrammarRuleStateTransitionsdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechGrammarRuleStateTransitionsdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechGrammarRuleStateTransitionsdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechGrammarRuleStateTransitionsdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechGrammarRuleStateTransitionsdisp.Invoke
//  Warning: renamed property 'Type' in ISpeechGrammarRuleStateTransition to 'Type_'
//  Warning: renamed property 'Text' in ISpeechGrammarRuleStateTransition to 'Text_'
//  Warning: 'GUID' not automatable in ISpeechGrammarRuleStateTransitiondisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechGrammarRuleStateTransitiondisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechGrammarRuleStateTransitiondisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechGrammarRuleStateTransitiondisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechGrammarRuleStateTransitiondisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechGrammarRuleStateTransitiondisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechGrammarRuleStateTransitiondisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechGrammarRuleStateTransitiondisp.Invoke
//  Warning: 'GUID' not automatable in ISpeechTextSelectionInformationdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechTextSelectionInformationdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechTextSelectionInformationdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechTextSelectionInformationdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechTextSelectionInformationdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechTextSelectionInformationdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechTextSelectionInformationdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechTextSelectionInformationdisp.Invoke
//  Warning: 'GUID' not automatable in ISpeechRecoResultdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechRecoResultdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechRecoResultdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechRecoResultdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechRecoResultdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechRecoResultdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechRecoResultdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechRecoResultdisp.Invoke
//  Warning: 'GUID' not automatable in ISpeechRecoResultTimesdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechRecoResultTimesdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechRecoResultTimesdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechRecoResultTimesdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechRecoResultTimesdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechRecoResultTimesdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechRecoResultTimesdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechRecoResultTimesdisp.Invoke
//  Warning: 'GUID' not automatable in ISpeechPhraseInfodisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechPhraseInfodisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechPhraseInfodisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechPhraseInfodisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechPhraseInfodisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechPhraseInfodisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechPhraseInfodisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechPhraseInfodisp.Invoke
//  Warning: 'GUID' not automatable in ISpeechPhraseRuledisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechPhraseRuledisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechPhraseRuledisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechPhraseRuledisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechPhraseRuledisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechPhraseRuledisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechPhraseRuledisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechPhraseRuledisp.Invoke
//  Warning: 'GUID' not automatable in ISpeechPhraseRulesdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechPhraseRulesdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechPhraseRulesdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechPhraseRulesdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechPhraseRulesdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechPhraseRulesdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechPhraseRulesdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechPhraseRulesdisp.Invoke
//  Warning: 'GUID' not automatable in ISpeechPhrasePropertiesdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechPhrasePropertiesdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechPhrasePropertiesdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechPhrasePropertiesdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechPhrasePropertiesdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechPhrasePropertiesdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechPhrasePropertiesdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechPhrasePropertiesdisp.Invoke
//  Warning: 'GUID' not automatable in ISpeechPhrasePropertydisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechPhrasePropertydisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechPhrasePropertydisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechPhrasePropertydisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechPhrasePropertydisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechPhrasePropertydisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechPhrasePropertydisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechPhrasePropertydisp.Invoke
//  Warning: 'GUID' not automatable in ISpeechPhraseElementsdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechPhraseElementsdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechPhraseElementsdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechPhraseElementsdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechPhraseElementsdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechPhraseElementsdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechPhraseElementsdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechPhraseElementsdisp.Invoke
//  Warning: 'GUID' not automatable in ISpeechPhraseElementdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechPhraseElementdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechPhraseElementdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechPhraseElementdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechPhraseElementdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechPhraseElementdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechPhraseElementdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechPhraseElementdisp.Invoke
//  Warning: 'GUID' not automatable in ISpeechPhraseReplacementsdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechPhraseReplacementsdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechPhraseReplacementsdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechPhraseReplacementsdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechPhraseReplacementsdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechPhraseReplacementsdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechPhraseReplacementsdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechPhraseReplacementsdisp.Invoke
//  Warning: renamed property 'Text' in ISpeechPhraseReplacement to 'Text_'
//  Warning: 'GUID' not automatable in ISpeechPhraseReplacementdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechPhraseReplacementdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechPhraseReplacementdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechPhraseReplacementdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechPhraseReplacementdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechPhraseReplacementdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechPhraseReplacementdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechPhraseReplacementdisp.Invoke
//  Warning: 'GUID' not automatable in ISpeechPhraseAlternatesdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechPhraseAlternatesdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechPhraseAlternatesdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechPhraseAlternatesdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechPhraseAlternatesdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechPhraseAlternatesdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechPhraseAlternatesdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechPhraseAlternatesdisp.Invoke
//  Warning: 'GUID' not automatable in ISpeechPhraseAlternatedisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechPhraseAlternatedisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechPhraseAlternatedisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechPhraseAlternatedisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechPhraseAlternatedisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechPhraseAlternatedisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechPhraseAlternatedisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechPhraseAlternatedisp.Invoke
//  Warning: 'GUID' not automatable in ISpeechRecoResult2disp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechRecoResult2disp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechRecoResult2disp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechRecoResult2disp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechRecoResult2disp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechRecoResult2disp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechRecoResult2disp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechRecoResult2disp.Invoke
//  Warning: 'GUID' not automatable in ISpeechLexicondisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechLexicondisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechLexicondisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechLexicondisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechLexicondisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechLexicondisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechLexicondisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechLexicondisp.Invoke
//  Warning: 'GUID' not automatable in ISpeechLexiconWordsdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechLexiconWordsdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechLexiconWordsdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechLexiconWordsdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechLexiconWordsdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechLexiconWordsdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechLexiconWordsdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechLexiconWordsdisp.Invoke
//  Warning: renamed property 'Type' in ISpeechLexiconWord to 'Type_'
//  Warning: 'GUID' not automatable in ISpeechLexiconWorddisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechLexiconWorddisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechLexiconWorddisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechLexiconWorddisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechLexiconWorddisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechLexiconWorddisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechLexiconWorddisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechLexiconWorddisp.Invoke
//  Warning: 'GUID' not automatable in ISpeechLexiconPronunciationsdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechLexiconPronunciationsdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechLexiconPronunciationsdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechLexiconPronunciationsdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechLexiconPronunciationsdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechLexiconPronunciationsdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechLexiconPronunciationsdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechLexiconPronunciationsdisp.Invoke
//  Warning: renamed property 'Type' in ISpeechLexiconPronunciation to 'Type_'
//  Warning: 'GUID' not automatable in ISpeechLexiconPronunciationdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechLexiconPronunciationdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechLexiconPronunciationdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechLexiconPronunciationdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechLexiconPronunciationdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechLexiconPronunciationdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechLexiconPronunciationdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechLexiconPronunciationdisp.Invoke
//  Warning: 'GUID' not automatable in ISpeechXMLRecoResultdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechXMLRecoResultdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechXMLRecoResultdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechXMLRecoResultdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechXMLRecoResultdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechXMLRecoResultdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechXMLRecoResultdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechXMLRecoResultdisp.Invoke
//  Warning: 'GUID' not automatable in ISpeechRecoResultDispatchdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechRecoResultDispatchdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechRecoResultDispatchdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechRecoResultDispatchdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechRecoResultDispatchdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechRecoResultDispatchdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechRecoResultDispatchdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechRecoResultDispatchdisp.Invoke
//  Warning: 'GUID' not automatable in ISpeechPhraseInfoBuilderdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechPhraseInfoBuilderdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechPhraseInfoBuilderdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechPhraseInfoBuilderdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechPhraseInfoBuilderdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechPhraseInfoBuilderdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechPhraseInfoBuilderdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechPhraseInfoBuilderdisp.Invoke
//  Warning: 'GUID' not automatable in ISpeechPhoneConverterdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechPhoneConverterdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechPhoneConverterdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechPhoneConverterdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechPhoneConverterdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechPhoneConverterdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechPhoneConverterdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechPhoneConverterdisp.Invoke
//  Warning: renamed method 'Reset' in IEnumSpObjectTokens to 'Reset_'
//  Warning: 'GUID' not automatable in ISpeechResourceLoaderdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechResourceLoaderdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISpeechResourceLoaderdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISpeechResourceLoaderdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISpeechResourceLoaderdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISpeechResourceLoaderdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISpeechResourceLoaderdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISpeechResourceLoaderdisp.Invoke
//  Warning: renamed method 'Reset' in IEnumString to 'Reset_'
Uses
  Windows,ActiveX,Classes,Variants,stdole2,ActiveXContainer,EventSink;

Const
  SpeechLibMajorVersion = 5;
  SpeechLibMinorVersion = 4;
  SpeechLibLCID = 0;
  LIBID_SpeechLib : TGUID = '{C866CA3A-32F7-11D2-9602-00C04F8EE628}';

  IID_ISpeechDataKey : TGUID = '{CE17C09B-4EFA-44D5-A4C9-59D9585AB0CD}';
  IID_ISpeechObjectToken : TGUID = '{C74A3ADC-B727-4500-A84A-B526721C8B8C}';
  IID_ISpeechObjectTokenCategory : TGUID = '{CA7EAC50-2D01-4145-86D4-5AE7D70F4469}';
  IID_ISpeechObjectTokens : TGUID = '{9285B776-2E7B-4BC0-B53E-580EB6FA967F}';
  IID_ISpeechAudioBufferInfo : TGUID = '{11B103D8-1142-4EDF-A093-82FB3915F8CC}';
  IID_ISpeechAudioStatus : TGUID = '{C62D9C91-7458-47F6-862D-1EF86FB0B278}';
  IID_ISpeechAudioFormat : TGUID = '{E6E9C590-3E18-40E3-8299-061F98BDE7C7}';
  IID_ISpeechWaveFormatEx : TGUID = '{7A1EF0D5-1581-4741-88E4-209A49F11A10}';
  IID_ISpeechBaseStream : TGUID = '{6450336F-7D49-4CED-8097-49D6DEE37294}';
  IID_ISpeechFileStream : TGUID = '{AF67F125-AB39-4E93-B4A2-CC2E66E182A7}';
  IID_ISpeechMemoryStream : TGUID = '{EEB14B68-808B-4ABE-A5EA-B51DA7588008}';
  IID_ISpeechCustomStream : TGUID = '{1A9E9F4F-104F-4DB8-A115-EFD7FD0C97AE}';
  IID_ISpeechAudio : TGUID = '{CFF8E175-019E-11D3-A08E-00C04F8EF9B5}';
  IID_ISpeechMMSysAudio : TGUID = '{3C76AF6D-1FD7-4831-81D1-3B71D5A13C44}';
  IID_ISpeechVoice : TGUID = '{269316D8-57BD-11D2-9EEE-00C04F797396}';
  IID_ISpeechVoiceStatus : TGUID = '{8BE47B07-57F6-11D2-9EEE-00C04F797396}';
  IID__ISpeechVoiceEvents : TGUID = '{A372ACD1-3BEF-4BBD-8FFB-CB3E2B416AF8}';
  IID_ISpeechRecognizer : TGUID = '{2D5F1C0C-BD75-4B08-9478-3B11FEA2586C}';
  IID_ISpeechRecognizerStatus : TGUID = '{BFF9E781-53EC-484E-BB8A-0E1B5551E35C}';
  IID_ISpeechRecoContext : TGUID = '{580AA49D-7E1E-4809-B8E2-57DA806104B8}';
  IID_ISpeechRecoGrammar : TGUID = '{B6D6F79F-2158-4E50-B5BC-9A9CCD852A09}';
  IID_ISpeechGrammarRules : TGUID = '{6FFA3B44-FC2D-40D1-8AFC-32911C7F1AD1}';
  IID_ISpeechGrammarRule : TGUID = '{AFE719CF-5DD1-44F2-999C-7A399F1CFCCC}';
  IID_ISpeechGrammarRuleState : TGUID = '{D4286F2C-EE67-45AE-B928-28D695362EDA}';
  IID_ISpeechGrammarRuleStateTransitions : TGUID = '{EABCE657-75BC-44A2-AA7F-C56476742963}';
  IID_ISpeechGrammarRuleStateTransition : TGUID = '{CAFD1DB1-41D1-4A06-9863-E2E81DA17A9A}';
  IID_ISpeechTextSelectionInformation : TGUID = '{3B9C7E7A-6EEE-4DED-9092-11657279ADBE}';
  IID_ISpeechRecoResult : TGUID = '{ED2879CF-CED9-4EE6-A534-DE0191D5468D}';
  IID_ISpeechRecoResultTimes : TGUID = '{62B3B8FB-F6E7-41BE-BDCB-056B1C29EFC0}';
  IID_ISpeechPhraseInfo : TGUID = '{961559CF-4E67-4662-8BF0-D93F1FCD61B3}';
  IID_ISpeechPhraseRule : TGUID = '{A7BFE112-A4A0-48D9-B602-C313843F6964}';
  IID_ISpeechPhraseRules : TGUID = '{9047D593-01DD-4B72-81A3-E4A0CA69F407}';
  IID_ISpeechPhraseProperties : TGUID = '{08166B47-102E-4B23-A599-BDB98DBFD1F4}';
  IID_ISpeechPhraseProperty : TGUID = '{CE563D48-961E-4732-A2E1-378A42B430BE}';
  IID_ISpeechPhraseElements : TGUID = '{0626B328-3478-467D-A0B3-D0853B93DDA3}';
  IID_ISpeechPhraseElement : TGUID = '{E6176F96-E373-4801-B223-3B62C068C0B4}';
  IID_ISpeechPhraseReplacements : TGUID = '{38BC662F-2257-4525-959E-2069D2596C05}';
  IID_ISpeechPhraseReplacement : TGUID = '{2890A410-53A7-4FB5-94EC-06D4998E3D02}';
  IID_ISpeechPhraseAlternates : TGUID = '{B238B6D5-F276-4C3D-A6C1-2974801C3CC2}';
  IID_ISpeechPhraseAlternate : TGUID = '{27864A2A-2B9F-4CB8-92D3-0D2722FD1E73}';
  IID__ISpeechRecoContextEvents : TGUID = '{7B8FCB42-0E9D-4F00-A048-7B04D6179D3D}';
  IID_ISpeechRecoResult2 : TGUID = '{8E0A246D-D3C8-45DE-8657-04290C458C3C}';
  IID_ISpeechLexicon : TGUID = '{3DA7627A-C7AE-4B23-8708-638C50362C25}';
  IID_ISpeechLexiconWords : TGUID = '{8D199862-415E-47D5-AC4F-FAA608B424E6}';
  IID_ISpeechLexiconWord : TGUID = '{4E5B933C-C9BE-48ED-8842-1EE51BB1D4FF}';
  IID_ISpeechLexiconPronunciations : TGUID = '{72829128-5682-4704-A0D4-3E2BB6F2EAD3}';
  IID_ISpeechLexiconPronunciation : TGUID = '{95252C5D-9E43-4F4A-9899-48EE73352F9F}';
  IID_ISpeechXMLRecoResult : TGUID = '{AAEC54AF-8F85-4924-944D-B79D39D72E19}';
  IID_ISpeechRecoResultDispatch : TGUID = '{6D60EB64-ACED-40A6-BBF3-4E557F71DEE2}';
  IID_ISpeechPhraseInfoBuilder : TGUID = '{3B151836-DF3A-4E0A-846C-D2ADC9334333}';
  IID_ISpeechPhoneConverter : TGUID = '{C3E4F353-433F-43D6-89A1-6A62A7054C3D}';
  CLASS_SpNotifyTranslator : TGUID = '{E2AE5372-5D40-11D2-960E-00C04F8EE628}';
  IID_ISpNotifyTranslator : TGUID = '{ACA16614-5D3D-11D2-960E-00C04F8EE628}';
  IID_ISpNotifySink : TGUID = '{259684DC-37C3-11D2-9603-00C04F8EE628}';
  CLASS_SpObjectTokenCategory : TGUID = '{A910187F-0C7A-45AC-92CC-59EDAFB77B53}';
  IID_ISpObjectTokenCategory : TGUID = '{2D3D3845-39AF-4850-BBF9-40B49780011D}';
  IID_ISpDataKey : TGUID = '{14056581-E16C-11D2-BB90-00C04F8EE6C0}';
  IID_IEnumSpObjectTokens : TGUID = '{06B64F9E-7FDA-11D2-B4F2-00C04F797396}';
  IID_ISpObjectToken : TGUID = '{14056589-E16C-11D2-BB90-00C04F8EE6C0}';
  CLASS_SpObjectToken : TGUID = '{EF411752-3736-4CB4-9C8C-8EF4CCB58EFE}';
  CLASS_SpResourceManager : TGUID = '{96749373-3391-11D2-9EE3-00C04F797396}';
  IID_ISpResourceManager : TGUID = '{93384E18-5014-43D5-ADBB-A78E055926BD}';
  IID_IServiceProvider : TGUID = '{6D5140C1-7436-11CE-8034-00AA006009FA}';
  CLASS_SpStreamFormatConverter : TGUID = '{7013943A-E2EC-11D2-A086-00C04F8EF9B5}';
  IID_ISpStreamFormatConverter : TGUID = '{678A932C-EA71-4446-9B41-78FDA6280A29}';
  IID_ISpStreamFormat : TGUID = '{BED530BE-2606-4F4D-A1C0-54C5CDA5566F}';
  IID_IStream : TGUID = '{0000000C-0000-0000-C000-000000000046}';
  IID_ISequentialStream : TGUID = '{0C733A30-2A1C-11CE-ADE5-00AA0044773D}';
  CLASS_SpMMAudioEnum : TGUID = '{AB1890A0-E91F-11D2-BB91-00C04F8EE6C0}';
  CLASS_SpMMAudioIn : TGUID = '{CF3D2E50-53F2-11D2-960C-00C04F8EE628}';
  IID_ISpEventSource : TGUID = '{BE7A9CCE-5F9E-11D2-960F-00C04F8EE628}';
  IID_ISpNotifySource : TGUID = '{5EFF4AEF-8487-11D2-961C-00C04F8EE628}';
  IID_ISpEventSink : TGUID = '{BE7A9CC9-5F9E-11D2-960F-00C04F8EE628}';
  IID_ISpObjectWithToken : TGUID = '{5B559F40-E952-11D2-BB91-00C04F8EE6C0}';
  IID_ISpMMSysAudio : TGUID = '{15806F6E-1D70-4B48-98E6-3B1A007509AB}';
  IID_ISpAudio : TGUID = '{C05C768F-FAE8-4EC2-8E07-338321C12452}';
  CLASS_SpMMAudioOut : TGUID = '{A8C680EB-3D32-11D2-9EE7-00C04F797396}';
  CLASS_SpStream : TGUID = '{715D9C59-4442-11D2-9605-00C04F8EE628}';
  IID_ISpStream : TGUID = '{12E3CCA9-7518-44C5-A5E7-BA5A79CB929E}';
  CLASS_SpVoice : TGUID = '{96749377-3391-11D2-9EE3-00C04F797396}';
  IID_ISpVoice : TGUID = '{6C44DF74-72B9-4992-A1EC-EF996E0422D4}';
  IID_ISpPhoneticAlphabetSelection : TGUID = '{B2745EFD-42CE-48CA-81F1-A96E02538A90}';
  CLASS_SpSharedRecoContext : TGUID = '{47206204-5ECA-11D2-960F-00C04F8EE628}';
  IID_ISpRecoContext : TGUID = '{F740A62F-7C15-489E-8234-940A33D9272D}';
  IID_ISpRecognizer : TGUID = '{C2B5F241-DAA0-4507-9E16-5A1EAA2B7A5C}';
  IID_ISpProperties : TGUID = '{5B4FB971-B115-4DE1-AD97-E482E3BF6EE4}';
  IID_ISpPhrase : TGUID = '{1A5C0354-B621-4B5A-8791-D306ED379E53}';
  IID_ISpRecoGrammar : TGUID = '{2177DB29-7F45-47D0-8554-067E91C80502}';
  IID_ISpGrammarBuilder : TGUID = '{8137828F-591A-4A42-BE58-49EA7EBAAC68}';
  IID_ISpRecoResult : TGUID = '{20B053BE-E235-43CD-9A2A-8D17A48B7842}';
  IID_ISpPhraseAlt : TGUID = '{8FCEBC98-4E49-4067-9C6C-D86A0E092E3D}';
  IID_ISpRecoContext2 : TGUID = '{BEAD311C-52FF-437F-9464-6B21054CA73D}';
  CLASS_SpInprocRecognizer : TGUID = '{41B89B6B-9399-11D2-9623-00C04F8EE628}';
  IID_ISpRecognizer2 : TGUID = '{8FC6D974-C81E-4098-93C5-0147F61ED4D3}';
  IID_ISpRecognizer3 : TGUID = '{DF1B943C-5838-4AA2-8706-D7CD5B333499}';
  IID_ISpRecoCategory : TGUID = '{DA0CD0F9-14A2-4F09-8C2A-85CC48979345}';
  IID_ISpSerializeState : TGUID = '{21B501A0-0EC7-46C9-92C3-A2BC784C54B9}';
  CLASS_SpSharedRecognizer : TGUID = '{3BEE4890-4FE9-4A37-8C1E-5E7E12791C1F}';
  CLASS_SpLexicon : TGUID = '{0655E396-25D0-11D3-9C26-00C04F8EF87C}';
  IID_ISpLexicon : TGUID = '{DA41A7C2-5383-4DB2-916B-6C1719E3DB58}';
  CLASS_SpUnCompressedLexicon : TGUID = '{C9E37C15-DF92-4727-85D6-72E5EEB6995A}';
  CLASS_SpCompressedLexicon : TGUID = '{90903716-2F42-11D3-9C26-00C04F8EF87C}';
  CLASS_SpShortcut : TGUID = '{0D722F1A-9FCF-4E62-96D8-6DF8F01A26AA}';
  IID_ISpShortcut : TGUID = '{3DF681E2-EA56-11D9-8BDE-F66BAD1E3F3A}';
  CLASS_SpPhoneConverter : TGUID = '{9185F743-1143-4C28-86B5-BFF14F20E5C8}';
  IID_ISpPhoneConverter : TGUID = '{8445C581-0CAC-4A38-ABFE-9B2CE2826455}';
  CLASS_SpPhoneticAlphabetConverter : TGUID = '{4F414126-DFE3-4629-99EE-797978317EAD}';
  IID_ISpPhoneticAlphabetConverter : TGUID = '{133ADCD4-19B4-4020-9FDC-842E78253B17}';
  CLASS_SpNullPhoneConverter : TGUID = '{455F24E9-7396-4A16-9715-7C0FDBE3EFE3}';
  CLASS_SpTextSelectionInformation : TGUID = '{0F92030A-CBFD-4AB8-A164-FF5985547FF6}';
  CLASS_SpPhraseInfoBuilder : TGUID = '{C23FC28D-C55F-4720-8B32-91F73C2BD5D1}';
  CLASS_SpAudioFormat : TGUID = '{9EF96870-E160-4792-820D-48CF0649E4EC}';
  CLASS_SpWaveFormatEx : TGUID = '{C79A574C-63BE-44B9-801F-283F87F898BE}';
  CLASS_SpInProcRecoContext : TGUID = '{73AD6842-ACE0-45E8-A4DD-8795881A2C2A}';
  CLASS_SpCustomStream : TGUID = '{8DBEF13F-1948-4AA8-8CF0-048EEBED95D8}';
  CLASS_SpFileStream : TGUID = '{947812B3-2AE1-4644-BA86-9E90DED7EC91}';
  CLASS_SpMemoryStream : TGUID = '{5FB7EF7D-DFF4-468A-B6B7-2FCBD188F994}';
  IID_ISpXMLRecoResult : TGUID = '{AE39362B-45A8-4074-9B9E-CCF49AA2D0B6}';
  IID_ISpRecoGrammar2 : TGUID = '{4B37BC9E-9ED6-44A3-93D3-18F022B79EC3}';
  IID_ISpeechResourceLoader : TGUID = '{B9AC5783-FCD0-4B21-B119-B4F8DA8FD2C3}';
  IID_IInternetSecurityManager : TGUID = '{79EAC9EE-BAF9-11CE-8C82-00AA004BA90B}';
  IID_IInternetSecurityMgrSite : TGUID = '{79EAC9ED-BAF9-11CE-8C82-00AA004BA90B}';
  IID_IEnumString : TGUID = '{00000101-0000-0000-C000-000000000046}';

//Enums

Type
  SpeechDataKeyLocation = Integer;
Const
  SDKLDefaultLocation = $0000000000000000;
  SDKLCurrentUser = $0000000000000001;
  SDKLLocalMachine = $0000000000000002;
  SDKLCurrentConfig = $0000000000000005;

Type
  SpeechTokenContext = Integer;
Const
  STCInprocServer = $0000000000000001;
  STCInprocHandler = $0000000000000002;
  STCLocalServer = $0000000000000004;
  STCRemoteServer = $0000000000000010;
  STCAll = $0000000000000017;

Type
  SpeechTokenShellFolder = Integer;
Const
  STSF_AppData = $000000000000001A;
  STSF_LocalAppData = $000000000000001C;
  STSF_CommonAppData = $0000000000000023;
  STSF_FlagCreate = $0000000000008000;

Type
  SpeechAudioState = Integer;
Const
  SASClosed = $0000000000000000;
  SASStop = $0000000000000001;
  SASPause = $0000000000000002;
  SASRun = $0000000000000003;

Type
  SpeechAudioFormatType = Integer;
Const
  SAFTDefault = $FFFFFFFFFFFFFFFF;
  SAFTNoAssignedFormat = $0000000000000000;
  SAFTText = $0000000000000001;
  SAFTNonStandardFormat = $0000000000000002;
  SAFTExtendedAudioFormat = $0000000000000003;
  SAFT8kHz8BitMono = $0000000000000004;
  SAFT8kHz8BitStereo = $0000000000000005;
  SAFT8kHz16BitMono = $0000000000000006;
  SAFT8kHz16BitStereo = $0000000000000007;
  SAFT11kHz8BitMono = $0000000000000008;
  SAFT11kHz8BitStereo = $0000000000000009;
  SAFT11kHz16BitMono = $000000000000000A;
  SAFT11kHz16BitStereo = $000000000000000B;
  SAFT12kHz8BitMono = $000000000000000C;
  SAFT12kHz8BitStereo = $000000000000000D;
  SAFT12kHz16BitMono = $000000000000000E;
  SAFT12kHz16BitStereo = $000000000000000F;
  SAFT16kHz8BitMono = $0000000000000010;
  SAFT16kHz8BitStereo = $0000000000000011;
  SAFT16kHz16BitMono = $0000000000000012;
  SAFT16kHz16BitStereo = $0000000000000013;
  SAFT22kHz8BitMono = $0000000000000014;
  SAFT22kHz8BitStereo = $0000000000000015;
  SAFT22kHz16BitMono = $0000000000000016;
  SAFT22kHz16BitStereo = $0000000000000017;
  SAFT24kHz8BitMono = $0000000000000018;
  SAFT24kHz8BitStereo = $0000000000000019;
  SAFT24kHz16BitMono = $000000000000001A;
  SAFT24kHz16BitStereo = $000000000000001B;
  SAFT32kHz8BitMono = $000000000000001C;
  SAFT32kHz8BitStereo = $000000000000001D;
  SAFT32kHz16BitMono = $000000000000001E;
  SAFT32kHz16BitStereo = $000000000000001F;
  SAFT44kHz8BitMono = $0000000000000020;
  SAFT44kHz8BitStereo = $0000000000000021;
  SAFT44kHz16BitMono = $0000000000000022;
  SAFT44kHz16BitStereo = $0000000000000023;
  SAFT48kHz8BitMono = $0000000000000024;
  SAFT48kHz8BitStereo = $0000000000000025;
  SAFT48kHz16BitMono = $0000000000000026;
  SAFT48kHz16BitStereo = $0000000000000027;
  SAFTTrueSpeech_8kHz1BitMono = $0000000000000028;
  SAFTCCITT_ALaw_8kHzMono = $0000000000000029;
  SAFTCCITT_ALaw_8kHzStereo = $000000000000002A;
  SAFTCCITT_ALaw_11kHzMono = $000000000000002B;
  SAFTCCITT_ALaw_11kHzStereo = $000000000000002C;
  SAFTCCITT_ALaw_22kHzMono = $000000000000002D;
  SAFTCCITT_ALaw_22kHzStereo = $000000000000002E;
  SAFTCCITT_ALaw_44kHzMono = $000000000000002F;
  SAFTCCITT_ALaw_44kHzStereo = $0000000000000030;
  SAFTCCITT_uLaw_8kHzMono = $0000000000000031;
  SAFTCCITT_uLaw_8kHzStereo = $0000000000000032;
  SAFTCCITT_uLaw_11kHzMono = $0000000000000033;
  SAFTCCITT_uLaw_11kHzStereo = $0000000000000034;
  SAFTCCITT_uLaw_22kHzMono = $0000000000000035;
  SAFTCCITT_uLaw_22kHzStereo = $0000000000000036;
  SAFTCCITT_uLaw_44kHzMono = $0000000000000037;
  SAFTCCITT_uLaw_44kHzStereo = $0000000000000038;
  SAFTADPCM_8kHzMono = $0000000000000039;
  SAFTADPCM_8kHzStereo = $000000000000003A;
  SAFTADPCM_11kHzMono = $000000000000003B;
  SAFTADPCM_11kHzStereo = $000000000000003C;
  SAFTADPCM_22kHzMono = $000000000000003D;
  SAFTADPCM_22kHzStereo = $000000000000003E;
  SAFTADPCM_44kHzMono = $000000000000003F;
  SAFTADPCM_44kHzStereo = $0000000000000040;
  SAFTGSM610_8kHzMono = $0000000000000041;
  SAFTGSM610_11kHzMono = $0000000000000042;
  SAFTGSM610_22kHzMono = $0000000000000043;
  SAFTGSM610_44kHzMono = $0000000000000044;

Type
  SpeechStreamSeekPositionType = Integer;
Const
  SSSPTRelativeToStart = $0000000000000000;
  SSSPTRelativeToCurrentPosition = $0000000000000001;
  SSSPTRelativeToEnd = $0000000000000002;

Type
  SpeechStreamFileMode = Integer;
Const
  SSFMOpenForRead = $0000000000000000;
  SSFMOpenReadWrite = $0000000000000001;
  SSFMCreate = $0000000000000002;
  SSFMCreateForWrite = $0000000000000003;

Type
  SpeechRunState = Integer;
Const
  SRSEDone = $0000000000000001;
  SRSEIsSpeaking = $0000000000000002;

Type
  SpeechVoiceEvents = Integer;
Const
  SVEStartInputStream = $0000000000000002;
  SVEEndInputStream = $0000000000000004;
  SVEVoiceChange = $0000000000000008;
  SVEBookmark = $0000000000000010;
  SVEWordBoundary = $0000000000000020;
  SVEPhoneme = $0000000000000040;
  SVESentenceBoundary = $0000000000000080;
  SVEViseme = $0000000000000100;
  SVEAudioLevel = $0000000000000200;
  SVEPrivate = $0000000000008000;
  SVEAllEvents = $00000000000083FE;

Type
  SpeechVoicePriority = Integer;
Const
  SVPNormal = $0000000000000000;
  SVPAlert = $0000000000000001;
  SVPOver = $0000000000000002;

Type
  SpeechVoiceSpeakFlags = Integer;
Const
  SVSFDefault = $0000000000000000;
  SVSFlagsAsync = $0000000000000001;
  SVSFPurgeBeforeSpeak = $0000000000000002;
  SVSFIsFilename = $0000000000000004;
  SVSFIsXML = $0000000000000008;
  SVSFIsNotXML = $0000000000000010;
  SVSFPersistXML = $0000000000000020;
  SVSFNLPSpeakPunc = $0000000000000040;
  SVSFParseSapi = $0000000000000080;
  SVSFParseSsml = $0000000000000100;
  SVSFParseAutodetect = $0000000000000000;
  SVSFNLPMask = $0000000000000040;
  SVSFParseMask = $0000000000000180;
  SVSFVoiceMask = $00000000000001FF;
  SVSFUnusedFlags = $FFFFFFFFFFFFFE00;

Type
  SpeechVisemeFeature = Integer;
Const
  SVF_None = $0000000000000000;
  SVF_Stressed = $0000000000000001;
  SVF_Emphasis = $0000000000000002;

Type
  SpeechVisemeType = Integer;
Const
  SVP_0 = $0000000000000000;
  SVP_1 = $0000000000000001;
  SVP_2 = $0000000000000002;
  SVP_3 = $0000000000000003;
  SVP_4 = $0000000000000004;
  SVP_5 = $0000000000000005;
  SVP_6 = $0000000000000006;
  SVP_7 = $0000000000000007;
  SVP_8 = $0000000000000008;
  SVP_9 = $0000000000000009;
  SVP_10 = $000000000000000A;
  SVP_11 = $000000000000000B;
  SVP_12 = $000000000000000C;
  SVP_13 = $000000000000000D;
  SVP_14 = $000000000000000E;
  SVP_15 = $000000000000000F;
  SVP_16 = $0000000000000010;
  SVP_17 = $0000000000000011;
  SVP_18 = $0000000000000012;
  SVP_19 = $0000000000000013;
  SVP_20 = $0000000000000014;
  SVP_21 = $0000000000000015;

Type
  SpeechRecognizerState = Integer;
Const
  SRSInactive = $0000000000000000;
  SRSActive = $0000000000000001;
  SRSActiveAlways = $0000000000000002;
  SRSInactiveWithPurge = $0000000000000003;

Type
  SpeechInterference = Integer;
Const
  SINone = $0000000000000000;
  SINoise = $0000000000000001;
  SINoSignal = $0000000000000002;
  SITooLoud = $0000000000000003;
  SITooQuiet = $0000000000000004;
  SITooFast = $0000000000000005;
  SITooSlow = $0000000000000006;

Type
  SpeechRecoEvents = Integer;
Const
  SREStreamEnd = $0000000000000001;
  SRESoundStart = $0000000000000002;
  SRESoundEnd = $0000000000000004;
  SREPhraseStart = $0000000000000008;
  SRERecognition = $0000000000000010;
  SREHypothesis = $0000000000000020;
  SREBookmark = $0000000000000040;
  SREPropertyNumChange = $0000000000000080;
  SREPropertyStringChange = $0000000000000100;
  SREFalseRecognition = $0000000000000200;
  SREInterference = $0000000000000400;
  SRERequestUI = $0000000000000800;
  SREStateChange = $0000000000001000;
  SREAdaptation = $0000000000002000;
  SREStreamStart = $0000000000004000;
  SRERecoOtherContext = $0000000000008000;
  SREAudioLevel = $0000000000010000;
  SREPrivate = $0000000000040000;
  SREAllEvents = $000000000005FFFF;

Type
  SpeechRecoContextState = Integer;
Const
  SRCS_Disabled = $0000000000000000;
  SRCS_Enabled = $0000000000000001;

Type
  SpeechRetainedAudioOptions = Integer;
Const
  SRAONone = $0000000000000000;
  SRAORetainAudio = $0000000000000001;

Type
  SpeechGrammarState = Integer;
Const
  SGSEnabled = $0000000000000001;
  SGSDisabled = $0000000000000000;
  SGSExclusive = $0000000000000003;

Type
  SpeechRuleAttributes = Integer;
Const
  SRATopLevel = $0000000000000001;
  SRADefaultToActive = $0000000000000002;
  SRAExport = $0000000000000004;
  SRAImport = $0000000000000008;
  SRAInterpreter = $0000000000000010;
  SRADynamic = $0000000000000020;
  SRARoot = $0000000000000040;

Type
  SpeechGrammarRuleStateTransitionType = Integer;
Const
  SGRSTTEpsilon = $0000000000000000;
  SGRSTTWord = $0000000000000001;
  SGRSTTRule = $0000000000000002;
  SGRSTTDictation = $0000000000000003;
  SGRSTTWildcard = $0000000000000004;
  SGRSTTTextBuffer = $0000000000000005;

Type
  SpeechGrammarWordType = Integer;
Const
  SGDisplay = $0000000000000000;
  SGLexical = $0000000000000001;
  SGPronounciation = $0000000000000002;
  SGLexicalNoSpecialChars = $0000000000000003;

Type
  SpeechSpecialTransitionType = Integer;
Const
  SSTTWildcard = $0000000000000001;
  SSTTDictation = $0000000000000002;
  SSTTTextBuffer = $0000000000000003;

Type
  SpeechLoadOption = Integer;
Const
  SLOStatic = $0000000000000000;
  SLODynamic = $0000000000000001;

Type
  SpeechRuleState = Integer;
Const
  SGDSInactive = $0000000000000000;
  SGDSActive = $0000000000000001;
  SGDSActiveWithAutoPause = $0000000000000003;
  SGDSActiveUserDelimited = $0000000000000004;

Type
  SpeechWordPronounceable = Integer;
Const
  SWPUnknownWordUnpronounceable = $0000000000000000;
  SWPUnknownWordPronounceable = $0000000000000001;
  SWPKnownWordPronounceable = $0000000000000002;

Type
  SpeechEngineConfidence = Integer;
Const
  SECLowConfidence = $FFFFFFFFFFFFFFFF;
  SECNormalConfidence = $0000000000000000;
  SECHighConfidence = $0000000000000001;

Type
  SpeechDisplayAttributes = Integer;
Const
  SDA_No_Trailing_Space = $0000000000000000;
  SDA_One_Trailing_Space = $0000000000000002;
  SDA_Two_Trailing_Spaces = $0000000000000004;
  SDA_Consume_Leading_Spaces = $0000000000000008;

Type
  SpeechDiscardType = Integer;
Const
  SDTProperty = $0000000000000001;
  SDTReplacement = $0000000000000002;
  SDTRule = $0000000000000004;
  SDTDisplayText = $0000000000000008;
  SDTLexicalForm = $0000000000000010;
  SDTPronunciation = $0000000000000020;
  SDTAudio = $0000000000000040;
  SDTAlternates = $0000000000000080;
  SDTAll = $00000000000000FF;

Type
  SpeechBookmarkOptions = Integer;
Const
  SBONone = $0000000000000000;
  SBOPause = $0000000000000001;

Type
  SpeechFormatType = Integer;
Const
  SFTInput = $0000000000000000;
  SFTSREngine = $0000000000000001;

Type
  SpeechRecognitionType = Integer;
Const
  SRTStandard = $0000000000000000;
  SRTAutopause = $0000000000000001;
  SRTEmulated = $0000000000000002;
  SRTSMLTimeout = $0000000000000004;
  SRTExtendableParse = $0000000000000008;
  SRTReSent = $0000000000000010;

Type
  SpeechLexiconType = Integer;
Const
  SLTUser = $0000000000000001;
  SLTApp = $0000000000000002;

Type
  SpeechWordType = Integer;
Const
  SWTAdded = $0000000000000001;
  SWTDeleted = $0000000000000002;

Type
  SpeechPartOfSpeech = Integer;
Const
  SPSNotOverriden = $FFFFFFFFFFFFFFFF;
  SPSUnknown = $0000000000000000;
  SPSNoun = $0000000000001000;
  SPSVerb = $0000000000002000;
  SPSModifier = $0000000000003000;
  SPSFunction = $0000000000004000;
  SPSInterjection = $0000000000005000;
  SPSLMA = $0000000000007000;
  SPSSuppressWord = $000000000000F000;

Type
  DISPID_SpeechDataKey = Integer;
Const
  DISPID_SDKSetBinaryValue = $0000000000000001;
  DISPID_SDKGetBinaryValue = $0000000000000002;
  DISPID_SDKSetStringValue = $0000000000000003;
  DISPID_SDKGetStringValue = $0000000000000004;
  DISPID_SDKSetLongValue = $0000000000000005;
  DISPID_SDKGetlongValue = $0000000000000006;
  DISPID_SDKOpenKey = $0000000000000007;
  DISPID_SDKCreateKey = $0000000000000008;
  DISPID_SDKDeleteKey = $0000000000000009;
  DISPID_SDKDeleteValue = $000000000000000A;
  DISPID_SDKEnumKeys = $000000000000000B;
  DISPID_SDKEnumValues = $000000000000000C;

Type
  DISPID_SpeechObjectToken = Integer;
Const
  DISPID_SOTId = $0000000000000001;
  DISPID_SOTDataKey = $0000000000000002;
  DISPID_SOTCategory = $0000000000000003;
  DISPID_SOTGetDescription = $0000000000000004;
  DISPID_SOTSetId = $0000000000000005;
  DISPID_SOTGetAttribute = $0000000000000006;
  DISPID_SOTCreateInstance = $0000000000000007;
  DISPID_SOTRemove = $0000000000000008;
  DISPID_SOTGetStorageFileName = $0000000000000009;
  DISPID_SOTRemoveStorageFileName = $000000000000000A;
  DISPID_SOTIsUISupported = $000000000000000B;
  DISPID_SOTDisplayUI = $000000000000000C;
  DISPID_SOTMatchesAttributes = $000000000000000D;

Type
  DISPID_SpeechObjectTokens = Integer;
Const
  DISPID_SOTsCount = $0000000000000001;
  DISPID_SOTsItem = $0000000000000000;
  DISPID_SOTs_NewEnum = $FFFFFFFFFFFFFFFC;

Type
  DISPID_SpeechObjectTokenCategory = Integer;
Const
  DISPID_SOTCId = $0000000000000001;
  DISPID_SOTCDefault = $0000000000000002;
  DISPID_SOTCSetId = $0000000000000003;
  DISPID_SOTCGetDataKey = $0000000000000004;
  DISPID_SOTCEnumerateTokens = $0000000000000005;

Type
  DISPID_SpeechAudioFormat = Integer;
Const
  DISPID_SAFType = $0000000000000001;
  DISPID_SAFGuid = $0000000000000002;
  DISPID_SAFGetWaveFormatEx = $0000000000000003;
  DISPID_SAFSetWaveFormatEx = $0000000000000004;

Type
  DISPID_SpeechBaseStream = Integer;
Const
  DISPID_SBSFormat = $0000000000000001;
  DISPID_SBSRead = $0000000000000002;
  DISPID_SBSWrite = $0000000000000003;
  DISPID_SBSSeek = $0000000000000004;

Type
  DISPID_SpeechAudio = Integer;
Const
  DISPID_SAStatus = $00000000000000C8;
  DISPID_SABufferInfo = $00000000000000C9;
  DISPID_SADefaultFormat = $00000000000000CA;
  DISPID_SAVolume = $00000000000000CB;
  DISPID_SABufferNotifySize = $00000000000000CC;
  DISPID_SAEventHandle = $00000000000000CD;
  DISPID_SASetState = $00000000000000CE;

Type
  DISPID_SpeechMMSysAudio = Integer;
Const
  DISPID_SMSADeviceId = $000000000000012C;
  DISPID_SMSALineId = $000000000000012D;
  DISPID_SMSAMMHandle = $000000000000012E;

Type
  DISPID_SpeechFileStream = Integer;
Const
  DISPID_SFSOpen = $0000000000000064;
  DISPID_SFSClose = $0000000000000065;

Type
  DISPID_SpeechCustomStream = Integer;
Const
  DISPID_SCSBaseStream = $0000000000000064;

Type
  DISPID_SpeechMemoryStream = Integer;
Const
  DISPID_SMSSetData = $0000000000000064;
  DISPID_SMSGetData = $0000000000000065;

Type
  DISPID_SpeechAudioStatus = Integer;
Const
  DISPID_SASFreeBufferSpace = $0000000000000001;
  DISPID_SASNonBlockingIO = $0000000000000002;
  DISPID_SASState = $0000000000000003;
  DISPID_SASCurrentSeekPosition = $0000000000000004;
  DISPID_SASCurrentDevicePosition = $0000000000000005;

Type
  DISPID_SpeechAudioBufferInfo = Integer;
Const
  DISPID_SABIMinNotification = $0000000000000001;
  DISPID_SABIBufferSize = $0000000000000002;
  DISPID_SABIEventBias = $0000000000000003;

Type
  DISPID_SpeechWaveFormatEx = Integer;
Const
  DISPID_SWFEFormatTag = $0000000000000001;
  DISPID_SWFEChannels = $0000000000000002;
  DISPID_SWFESamplesPerSec = $0000000000000003;
  DISPID_SWFEAvgBytesPerSec = $0000000000000004;
  DISPID_SWFEBlockAlign = $0000000000000005;
  DISPID_SWFEBitsPerSample = $0000000000000006;
  DISPID_SWFEExtraData = $0000000000000007;

Type
  DISPID_SpeechVoice = Integer;
Const
  DISPID_SVStatus = $0000000000000001;
  DISPID_SVVoice = $0000000000000002;
  DISPID_SVAudioOutput = $0000000000000003;
  DISPID_SVAudioOutputStream = $0000000000000004;
  DISPID_SVRate = $0000000000000005;
  DISPID_SVVolume = $0000000000000006;
  DISPID_SVAllowAudioOuputFormatChangesOnNextSet = $0000000000000007;
  DISPID_SVEventInterests = $0000000000000008;
  DISPID_SVPriority = $0000000000000009;
  DISPID_SVAlertBoundary = $000000000000000A;
  DISPID_SVSyncronousSpeakTimeout = $000000000000000B;
  DISPID_SVSpeak = $000000000000000C;
  DISPID_SVSpeakStream = $000000000000000D;
  DISPID_SVPause = $000000000000000E;
  DISPID_SVResume = $000000000000000F;
  DISPID_SVSkip = $0000000000000010;
  DISPID_SVGetVoices = $0000000000000011;
  DISPID_SVGetAudioOutputs = $0000000000000012;
  DISPID_SVWaitUntilDone = $0000000000000013;
  DISPID_SVSpeakCompleteEvent = $0000000000000014;
  DISPID_SVIsUISupported = $0000000000000015;
  DISPID_SVDisplayUI = $0000000000000016;

Type
  DISPID_SpeechVoiceStatus = Integer;
Const
  DISPID_SVSCurrentStreamNumber = $0000000000000001;
  DISPID_SVSLastStreamNumberQueued = $0000000000000002;
  DISPID_SVSLastResult = $0000000000000003;
  DISPID_SVSRunningState = $0000000000000004;
  DISPID_SVSInputWordPosition = $0000000000000005;
  DISPID_SVSInputWordLength = $0000000000000006;
  DISPID_SVSInputSentencePosition = $0000000000000007;
  DISPID_SVSInputSentenceLength = $0000000000000008;
  DISPID_SVSLastBookmark = $0000000000000009;
  DISPID_SVSLastBookmarkId = $000000000000000A;
  DISPID_SVSPhonemeId = $000000000000000B;
  DISPID_SVSVisemeId = $000000000000000C;

Type
  DISPID_SpeechVoiceEvent = Integer;
Const
  DISPID_SVEStreamStart = $0000000000000001;
  DISPID_SVEStreamEnd = $0000000000000002;
  DISPID_SVEVoiceChange = $0000000000000003;
  DISPID_SVEBookmark = $0000000000000004;
  DISPID_SVEWord = $0000000000000005;
  DISPID_SVEPhoneme = $0000000000000006;
  DISPID_SVESentenceBoundary = $0000000000000007;
  DISPID_SVEViseme = $0000000000000008;
  DISPID_SVEAudioLevel = $0000000000000009;
  DISPID_SVEEnginePrivate = $000000000000000A;

Type
  DISPID_SpeechRecognizer = Integer;
Const
  DISPID_SRRecognizer = $0000000000000001;
  DISPID_SRAllowAudioInputFormatChangesOnNextSet = $0000000000000002;
  DISPID_SRAudioInput = $0000000000000003;
  DISPID_SRAudioInputStream = $0000000000000004;
  DISPID_SRIsShared = $0000000000000005;
  DISPID_SRState = $0000000000000006;
  DISPID_SRStatus = $0000000000000007;
  DISPID_SRProfile = $0000000000000008;
  DISPID_SREmulateRecognition = $0000000000000009;
  DISPID_SRCreateRecoContext = $000000000000000A;
  DISPID_SRGetFormat = $000000000000000B;
  DISPID_SRSetPropertyNumber = $000000000000000C;
  DISPID_SRGetPropertyNumber = $000000000000000D;
  DISPID_SRSetPropertyString = $000000000000000E;
  DISPID_SRGetPropertyString = $000000000000000F;
  DISPID_SRIsUISupported = $0000000000000010;
  DISPID_SRDisplayUI = $0000000000000011;
  DISPID_SRGetRecognizers = $0000000000000012;
  DISPID_SVGetAudioInputs = $0000000000000013;
  DISPID_SVGetProfiles = $0000000000000014;

Type
  SpeechEmulationCompareFlags = Integer;
Const
  SECFIgnoreCase = $0000000000000001;
  SECFIgnoreKanaType = $0000000000010000;
  SECFIgnoreWidth = $0000000000020000;
  SECFNoSpecialChars = $0000000020000000;
  SECFEmulateResult = $0000000040000000;
  SECFDefault = $0000000000030001;

Type
  DISPID_SpeechRecognizerStatus = Integer;
Const
  DISPID_SRSAudioStatus = $0000000000000001;
  DISPID_SRSCurrentStreamPosition = $0000000000000002;
  DISPID_SRSCurrentStreamNumber = $0000000000000003;
  DISPID_SRSNumberOfActiveRules = $0000000000000004;
  DISPID_SRSClsidEngine = $0000000000000005;
  DISPID_SRSSupportedLanguages = $0000000000000006;

Type
  DISPID_SpeechRecoContext = Integer;
Const
  DISPID_SRCRecognizer = $0000000000000001;
  DISPID_SRCAudioInInterferenceStatus = $0000000000000002;
  DISPID_SRCRequestedUIType = $0000000000000003;
  DISPID_SRCVoice = $0000000000000004;
  DISPID_SRAllowVoiceFormatMatchingOnNextSet = $0000000000000005;
  DISPID_SRCVoicePurgeEvent = $0000000000000006;
  DISPID_SRCEventInterests = $0000000000000007;
  DISPID_SRCCmdMaxAlternates = $0000000000000008;
  DISPID_SRCState = $0000000000000009;
  DISPID_SRCRetainedAudio = $000000000000000A;
  DISPID_SRCRetainedAudioFormat = $000000000000000B;
  DISPID_SRCPause = $000000000000000C;
  DISPID_SRCResume = $000000000000000D;
  DISPID_SRCCreateGrammar = $000000000000000E;
  DISPID_SRCCreateResultFromMemory = $000000000000000F;
  DISPID_SRCBookmark = $0000000000000010;
  DISPID_SRCSetAdaptationData = $0000000000000011;

Type
  DISPIDSPRG = Integer;
Const
  DISPID_SRGId = $0000000000000001;
  DISPID_SRGRecoContext = $0000000000000002;
  DISPID_SRGState = $0000000000000003;
  DISPID_SRGRules = $0000000000000004;
  DISPID_SRGReset = $0000000000000005;
  DISPID_SRGCommit = $0000000000000006;
  DISPID_SRGCmdLoadFromFile = $0000000000000007;
  DISPID_SRGCmdLoadFromObject = $0000000000000008;
  DISPID_SRGCmdLoadFromResource = $0000000000000009;
  DISPID_SRGCmdLoadFromMemory = $000000000000000A;
  DISPID_SRGCmdLoadFromProprietaryGrammar = $000000000000000B;
  DISPID_SRGCmdSetRuleState = $000000000000000C;
  DISPID_SRGCmdSetRuleIdState = $000000000000000D;
  DISPID_SRGDictationLoad = $000000000000000E;
  DISPID_SRGDictationUnload = $000000000000000F;
  DISPID_SRGDictationSetState = $0000000000000010;
  DISPID_SRGSetWordSequenceData = $0000000000000011;
  DISPID_SRGSetTextSelection = $0000000000000012;
  DISPID_SRGIsPronounceable = $0000000000000013;

Type
  DISPID_SpeechRecoContextEvents = Integer;
Const
  DISPID_SRCEStartStream = $0000000000000001;
  DISPID_SRCEEndStream = $0000000000000002;
  DISPID_SRCEBookmark = $0000000000000003;
  DISPID_SRCESoundStart = $0000000000000004;
  DISPID_SRCESoundEnd = $0000000000000005;
  DISPID_SRCEPhraseStart = $0000000000000006;
  DISPID_SRCERecognition = $0000000000000007;
  DISPID_SRCEHypothesis = $0000000000000008;
  DISPID_SRCEPropertyNumberChange = $0000000000000009;
  DISPID_SRCEPropertyStringChange = $000000000000000A;
  DISPID_SRCEFalseRecognition = $000000000000000B;
  DISPID_SRCEInterference = $000000000000000C;
  DISPID_SRCERequestUI = $000000000000000D;
  DISPID_SRCERecognizerStateChange = $000000000000000E;
  DISPID_SRCEAdaptation = $000000000000000F;
  DISPID_SRCERecognitionForOtherContext = $0000000000000010;
  DISPID_SRCEAudioLevel = $0000000000000011;
  DISPID_SRCEEnginePrivate = $0000000000000012;

Type
  DISPID_SpeechGrammarRule = Integer;
Const
  DISPID_SGRAttributes = $0000000000000001;
  DISPID_SGRInitialState = $0000000000000002;
  DISPID_SGRName = $0000000000000003;
  DISPID_SGRId = $0000000000000004;
  DISPID_SGRClear = $0000000000000005;
  DISPID_SGRAddResource = $0000000000000006;
  DISPID_SGRAddState = $0000000000000007;

Type
  DISPID_SpeechGrammarRules = Integer;
Const
  DISPID_SGRsCount = $0000000000000001;
  DISPID_SGRsDynamic = $0000000000000002;
  DISPID_SGRsAdd = $0000000000000003;
  DISPID_SGRsCommit = $0000000000000004;
  DISPID_SGRsCommitAndSave = $0000000000000005;
  DISPID_SGRsFindRule = $0000000000000006;
  DISPID_SGRsItem = $0000000000000000;
  DISPID_SGRs_NewEnum = $FFFFFFFFFFFFFFFC;

Type
  DISPID_SpeechGrammarRuleState = Integer;
Const
  DISPID_SGRSRule = $0000000000000001;
  DISPID_SGRSTransitions = $0000000000000002;
  DISPID_SGRSAddWordTransition = $0000000000000003;
  DISPID_SGRSAddRuleTransition = $0000000000000004;
  DISPID_SGRSAddSpecialTransition = $0000000000000005;

Type
  DISPID_SpeechGrammarRuleStateTransitions = Integer;
Const
  DISPID_SGRSTsCount = $0000000000000001;
  DISPID_SGRSTsItem = $0000000000000000;
  DISPID_SGRSTs_NewEnum = $FFFFFFFFFFFFFFFC;

Type
  DISPID_SpeechGrammarRuleStateTransition = Integer;
Const
  DISPID_SGRSTType = $0000000000000001;
  DISPID_SGRSTText = $0000000000000002;
  DISPID_SGRSTRule = $0000000000000003;
  DISPID_SGRSTWeight = $0000000000000004;
  DISPID_SGRSTPropertyName = $0000000000000005;
  DISPID_SGRSTPropertyId = $0000000000000006;
  DISPID_SGRSTPropertyValue = $0000000000000007;
  DISPID_SGRSTNextState = $0000000000000008;

Type
  DISPIDSPTSI = Integer;
Const
  DISPIDSPTSI_ActiveOffset = $0000000000000001;
  DISPIDSPTSI_ActiveLength = $0000000000000002;
  DISPIDSPTSI_SelectionOffset = $0000000000000003;
  DISPIDSPTSI_SelectionLength = $0000000000000004;

Type
  DISPID_SpeechRecoResult = Integer;
Const
  DISPID_SRRRecoContext = $0000000000000001;
  DISPID_SRRTimes = $0000000000000002;
  DISPID_SRRAudioFormat = $0000000000000003;
  DISPID_SRRPhraseInfo = $0000000000000004;
  DISPID_SRRAlternates = $0000000000000005;
  DISPID_SRRAudio = $0000000000000006;
  DISPID_SRRSpeakAudio = $0000000000000007;
  DISPID_SRRSaveToMemory = $0000000000000008;
  DISPID_SRRDiscardResultInfo = $0000000000000009;

Type
  DISPID_SpeechXMLRecoResult = Integer;
Const
  DISPID_SRRGetXMLResult = $000000000000000A;
  DISPID_SRRGetXMLErrorInfo = $000000000000000B;

Type
  SPXMLRESULTOPTIONS = Integer;
Const
  SPXRO_SML = $0000000000000000;
  SPXRO_Alternates_SML = $0000000000000001;

Type
  DISPID_SpeechRecoResult2 = Integer;
Const
  DISPID_SRRSetTextFeedback = $000000000000000C;

Type
  DISPID_SpeechPhraseBuilder = Integer;
Const
  DISPID_SPPBRestorePhraseFromMemory = $0000000000000001;

Type
  DISPID_SpeechRecoResultTimes = Integer;
Const
  DISPID_SRRTStreamTime = $0000000000000001;
  DISPID_SRRTLength = $0000000000000002;
  DISPID_SRRTTickCount = $0000000000000003;
  DISPID_SRRTOffsetFromStart = $0000000000000004;

Type
  DISPID_SpeechPhraseAlternate = Integer;
Const
  DISPID_SPARecoResult = $0000000000000001;
  DISPID_SPAStartElementInResult = $0000000000000002;
  DISPID_SPANumberOfElementsInResult = $0000000000000003;
  DISPID_SPAPhraseInfo = $0000000000000004;
  DISPID_SPACommit = $0000000000000005;

Type
  DISPID_SpeechPhraseAlternates = Integer;
Const
  DISPID_SPAsCount = $0000000000000001;
  DISPID_SPAsItem = $0000000000000000;
  DISPID_SPAs_NewEnum = $FFFFFFFFFFFFFFFC;

Type
  DISPID_SpeechPhraseInfo = Integer;
Const
  DISPID_SPILanguageId = $0000000000000001;
  DISPID_SPIGrammarId = $0000000000000002;
  DISPID_SPIStartTime = $0000000000000003;
  DISPID_SPIAudioStreamPosition = $0000000000000004;
  DISPID_SPIAudioSizeBytes = $0000000000000005;
  DISPID_SPIRetainedSizeBytes = $0000000000000006;
  DISPID_SPIAudioSizeTime = $0000000000000007;
  DISPID_SPIRule = $0000000000000008;
  DISPID_SPIProperties = $0000000000000009;
  DISPID_SPIElements = $000000000000000A;
  DISPID_SPIReplacements = $000000000000000B;
  DISPID_SPIEngineId = $000000000000000C;
  DISPID_SPIEnginePrivateData = $000000000000000D;
  DISPID_SPISaveToMemory = $000000000000000E;
  DISPID_SPIGetText = $000000000000000F;
  DISPID_SPIGetDisplayAttributes = $0000000000000010;

Type
  DISPID_SpeechPhraseElement = Integer;
Const
  DISPID_SPEAudioTimeOffset = $0000000000000001;
  DISPID_SPEAudioSizeTime = $0000000000000002;
  DISPID_SPEAudioStreamOffset = $0000000000000003;
  DISPID_SPEAudioSizeBytes = $0000000000000004;
  DISPID_SPERetainedStreamOffset = $0000000000000005;
  DISPID_SPERetainedSizeBytes = $0000000000000006;
  DISPID_SPEDisplayText = $0000000000000007;
  DISPID_SPELexicalForm = $0000000000000008;
  DISPID_SPEPronunciation = $0000000000000009;
  DISPID_SPEDisplayAttributes = $000000000000000A;
  DISPID_SPERequiredConfidence = $000000000000000B;
  DISPID_SPEActualConfidence = $000000000000000C;
  DISPID_SPEEngineConfidence = $000000000000000D;

Type
  DISPID_SpeechPhraseElements = Integer;
Const
  DISPID_SPEsCount = $0000000000000001;
  DISPID_SPEsItem = $0000000000000000;
  DISPID_SPEs_NewEnum = $FFFFFFFFFFFFFFFC;

Type
  DISPID_SpeechPhraseReplacement = Integer;
Const
  DISPID_SPRDisplayAttributes = $0000000000000001;
  DISPID_SPRText = $0000000000000002;
  DISPID_SPRFirstElement = $0000000000000003;
  DISPID_SPRNumberOfElements = $0000000000000004;

Type
  DISPID_SpeechPhraseReplacements = Integer;
Const
  DISPID_SPRsCount = $0000000000000001;
  DISPID_SPRsItem = $0000000000000000;
  DISPID_SPRs_NewEnum = $FFFFFFFFFFFFFFFC;

Type
  DISPID_SpeechPhraseProperty = Integer;
Const
  DISPID_SPPName = $0000000000000001;
  DISPID_SPPId = $0000000000000002;
  DISPID_SPPValue = $0000000000000003;
  DISPID_SPPFirstElement = $0000000000000004;
  DISPID_SPPNumberOfElements = $0000000000000005;
  DISPID_SPPEngineConfidence = $0000000000000006;
  DISPID_SPPConfidence = $0000000000000007;
  DISPID_SPPParent = $0000000000000008;
  DISPID_SPPChildren = $0000000000000009;

Type
  DISPID_SpeechPhraseProperties = Integer;
Const
  DISPID_SPPsCount = $0000000000000001;
  DISPID_SPPsItem = $0000000000000000;
  DISPID_SPPs_NewEnum = $FFFFFFFFFFFFFFFC;

Type
  DISPID_SpeechPhraseRule = Integer;
Const
  DISPID_SPRuleName = $0000000000000001;
  DISPID_SPRuleId = $0000000000000002;
  DISPID_SPRuleFirstElement = $0000000000000003;
  DISPID_SPRuleNumberOfElements = $0000000000000004;
  DISPID_SPRuleParent = $0000000000000005;
  DISPID_SPRuleChildren = $0000000000000006;
  DISPID_SPRuleConfidence = $0000000000000007;
  DISPID_SPRuleEngineConfidence = $0000000000000008;

Type
  DISPID_SpeechPhraseRules = Integer;
Const
  DISPID_SPRulesCount = $0000000000000001;
  DISPID_SPRulesItem = $0000000000000000;
  DISPID_SPRules_NewEnum = $FFFFFFFFFFFFFFFC;

Type
  DISPID_SpeechLexicon = Integer;
Const
  DISPID_SLGenerationId = $0000000000000001;
  DISPID_SLGetWords = $0000000000000002;
  DISPID_SLAddPronunciation = $0000000000000003;
  DISPID_SLAddPronunciationByPhoneIds = $0000000000000004;
  DISPID_SLRemovePronunciation = $0000000000000005;
  DISPID_SLRemovePronunciationByPhoneIds = $0000000000000006;
  DISPID_SLGetPronunciations = $0000000000000007;
  DISPID_SLGetGenerationChange = $0000000000000008;

Type
  DISPID_SpeechLexiconWords = Integer;
Const
  DISPID_SLWsCount = $0000000000000001;
  DISPID_SLWsItem = $0000000000000000;
  DISPID_SLWs_NewEnum = $FFFFFFFFFFFFFFFC;

Type
  DISPID_SpeechLexiconWord = Integer;
Const
  DISPID_SLWLangId = $0000000000000001;
  DISPID_SLWType = $0000000000000002;
  DISPID_SLWWord = $0000000000000003;
  DISPID_SLWPronunciations = $0000000000000004;

Type
  DISPID_SpeechLexiconProns = Integer;
Const
  DISPID_SLPsCount = $0000000000000001;
  DISPID_SLPsItem = $0000000000000000;
  DISPID_SLPs_NewEnum = $FFFFFFFFFFFFFFFC;

Type
  DISPID_SpeechLexiconPronunciation = Integer;
Const
  DISPID_SLPType = $0000000000000001;
  DISPID_SLPLangId = $0000000000000002;
  DISPID_SLPPartOfSpeech = $0000000000000003;
  DISPID_SLPPhoneIds = $0000000000000004;
  DISPID_SLPSymbolic = $0000000000000005;

Type
  DISPID_SpeechPhoneConverter = Integer;
Const
  DISPID_SPCLangId = $0000000000000001;
  DISPID_SPCPhoneToId = $0000000000000002;
  DISPID_SPCIdToPhone = $0000000000000003;

Type
  SPDATAKEYLOCATION = Integer;
Const
  SPDKL_DefaultLocation = $0000000000000000;
  SPDKL_CurrentUser = $0000000000000001;
  SPDKL_LocalMachine = $0000000000000002;
  SPDKL_CurrentConfig = $0000000000000005;

Type
  _SPAUDIOSTATE = Integer;
Const
  SPAS_CLOSED = $0000000000000000;
  SPAS_STOP = $0000000000000001;
  SPAS_PAUSE = $0000000000000002;
  SPAS_RUN = $0000000000000003;

Type
  SPFILEMODE = Integer;
Const
  SPFM_OPEN_READONLY = $0000000000000000;
  SPFM_OPEN_READWRITE = $0000000000000001;
  SPFM_CREATE = $0000000000000002;
  SPFM_CREATE_ALWAYS = $0000000000000003;
  SPFM_NUM_MODES = $0000000000000004;

Type
  SPVISEMES = Integer;
Const
  SP_VISEME_0 = $0000000000000000;
  SP_VISEME_1 = $0000000000000001;
  SP_VISEME_2 = $0000000000000002;
  SP_VISEME_3 = $0000000000000003;
  SP_VISEME_4 = $0000000000000004;
  SP_VISEME_5 = $0000000000000005;
  SP_VISEME_6 = $0000000000000006;
  SP_VISEME_7 = $0000000000000007;
  SP_VISEME_8 = $0000000000000008;
  SP_VISEME_9 = $0000000000000009;
  SP_VISEME_10 = $000000000000000A;
  SP_VISEME_11 = $000000000000000B;
  SP_VISEME_12 = $000000000000000C;
  SP_VISEME_13 = $000000000000000D;
  SP_VISEME_14 = $000000000000000E;
  SP_VISEME_15 = $000000000000000F;
  SP_VISEME_16 = $0000000000000010;
  SP_VISEME_17 = $0000000000000011;
  SP_VISEME_18 = $0000000000000012;
  SP_VISEME_19 = $0000000000000013;
  SP_VISEME_20 = $0000000000000014;
  SP_VISEME_21 = $0000000000000015;

Type
  SPVPRIORITY = Integer;
Const
  SPVPRI_NORMAL = $0000000000000000;
  SPVPRI_ALERT = $0000000000000001;
  SPVPRI_OVER = $0000000000000002;

Type
  SPEVENTENUM = Integer;
Const
  SPEI_UNDEFINED = $0000000000000000;
  SPEI_START_INPUT_STREAM = $0000000000000001;
  SPEI_END_INPUT_STREAM = $0000000000000002;
  SPEI_VOICE_CHANGE = $0000000000000003;
  SPEI_TTS_BOOKMARK = $0000000000000004;
  SPEI_WORD_BOUNDARY = $0000000000000005;
  SPEI_PHONEME = $0000000000000006;
  SPEI_SENTENCE_BOUNDARY = $0000000000000007;
  SPEI_VISEME = $0000000000000008;
  SPEI_TTS_AUDIO_LEVEL = $0000000000000009;
  SPEI_TTS_PRIVATE = $000000000000000F;
  SPEI_MIN_TTS = $0000000000000001;
  SPEI_MAX_TTS = $000000000000000F;
  SPEI_END_SR_STREAM = $0000000000000022;
  SPEI_SOUND_START = $0000000000000023;
  SPEI_SOUND_END = $0000000000000024;
  SPEI_PHRASE_START = $0000000000000025;
  SPEI_RECOGNITION = $0000000000000026;
  SPEI_HYPOTHESIS = $0000000000000027;
  SPEI_SR_BOOKMARK = $0000000000000028;
  SPEI_PROPERTY_NUM_CHANGE = $0000000000000029;
  SPEI_PROPERTY_STRING_CHANGE = $000000000000002A;
  SPEI_FALSE_RECOGNITION = $000000000000002B;
  SPEI_INTERFERENCE = $000000000000002C;
  SPEI_REQUEST_UI = $000000000000002D;
  SPEI_RECO_STATE_CHANGE = $000000000000002E;
  SPEI_ADAPTATION = $000000000000002F;
  SPEI_START_SR_STREAM = $0000000000000030;
  SPEI_RECO_OTHER_CONTEXT = $0000000000000031;
  SPEI_SR_AUDIO_LEVEL = $0000000000000032;
  SPEI_SR_RETAINEDAUDIO = $0000000000000033;
  SPEI_SR_PRIVATE = $0000000000000034;
  SPEI_ACTIVE_CATEGORY_CHANGED = $0000000000000035;
  SPEI_RESERVED5 = $0000000000000036;
  SPEI_RESERVED6 = $0000000000000037;
  SPEI_MIN_SR = $0000000000000022;
  SPEI_MAX_SR = $0000000000000037;
  SPEI_RESERVED1 = $000000000000001E;
  SPEI_RESERVED2 = $0000000000000021;
  SPEI_RESERVED3 = $000000000000003F;

Type
  SPRECOSTATE = Integer;
Const
  SPRST_INACTIVE = $0000000000000000;
  SPRST_ACTIVE = $0000000000000001;
  SPRST_ACTIVE_ALWAYS = $0000000000000002;
  SPRST_INACTIVE_WITH_PURGE = $0000000000000003;
  SPRST_NUM_STATES = $0000000000000004;

Type
  SPWAVEFORMATTYPE = Integer;
Const
  SPWF_INPUT = $0000000000000000;
  SPWF_SRENGINE = $0000000000000001;

Type
  SPSEMANTICFORMAT = Integer;
Const
  SPSMF_SAPI_PROPERTIES = $0000000000000000;
  SPSMF_SRGS_SEMANTICINTERPRETATION_MS = $0000000000000001;
  SPSMF_SRGS_SAPIPROPERTIES = $0000000000000002;
  SPSMF_UPS = $0000000000000004;
  SPSMF_SRGS_SEMANTICINTERPRETATION_W3C = $0000000000000008;

Type
  SPGRAMMARWORDTYPE = Integer;
Const
  SPWT_DISPLAY = $0000000000000000;
  SPWT_LEXICAL = $0000000000000001;
  SPWT_PRONUNCIATION = $0000000000000002;
  SPWT_LEXICAL_NO_SPECIAL_CHARS = $0000000000000003;

Type
  SPLOADOPTIONS = Integer;
Const
  SPLO_STATIC = $0000000000000000;
  SPLO_DYNAMIC = $0000000000000001;

Type
  SPRULESTATE = Integer;
Const
  SPRS_INACTIVE = $0000000000000000;
  SPRS_ACTIVE = $0000000000000001;
  SPRS_ACTIVE_WITH_AUTO_PAUSE = $0000000000000003;
  SPRS_ACTIVE_USER_DELIMITED = $0000000000000004;

Type
  SPWORDPRONOUNCEABLE = Integer;
Const
  SPWP_UNKNOWN_WORD_UNPRONOUNCEABLE = $0000000000000000;
  SPWP_UNKNOWN_WORD_PRONOUNCEABLE = $0000000000000001;
  SPWP_KNOWN_WORD_PRONOUNCEABLE = $0000000000000002;

Type
  SPGRAMMARSTATE = Integer;
Const
  SPGS_DISABLED = $0000000000000000;
  SPGS_ENABLED = $0000000000000001;
  SPGS_EXCLUSIVE = $0000000000000003;

Type
  SPINTERFERENCE = Integer;
Const
  SPINTERFERENCE_NONE = $0000000000000000;
  SPINTERFERENCE_NOISE = $0000000000000001;
  SPINTERFERENCE_NOSIGNAL = $0000000000000002;
  SPINTERFERENCE_TOOLOUD = $0000000000000003;
  SPINTERFERENCE_TOOQUIET = $0000000000000004;
  SPINTERFERENCE_TOOFAST = $0000000000000005;
  SPINTERFERENCE_TOOSLOW = $0000000000000006;
  SPINTERFERENCE_LATENCY_WARNING = $0000000000000007;
  SPINTERFERENCE_LATENCY_TRUNCATE_BEGIN = $0000000000000008;
  SPINTERFERENCE_LATENCY_TRUNCATE_END = $0000000000000009;

Type
  SPAUDIOOPTIONS = Integer;
Const
  SPAO_NONE = $0000000000000000;
  SPAO_RETAIN_AUDIO = $0000000000000001;

Type
  SPBOOKMARKOPTIONS = Integer;
Const
  SPBO_NONE = $0000000000000000;
  SPBO_PAUSE = $0000000000000001;
  SPBO_AHEAD = $0000000000000002;
  SPBO_TIME_UNITS = $0000000000000004;

Type
  SPCONTEXTSTATE = Integer;
Const
  SPCS_DISABLED = $0000000000000000;
  SPCS_ENABLED = $0000000000000001;

Type
  SPADAPTATIONRELEVANCE = Integer;
Const
  SPAR_Unknown = $0000000000000000;
  SPAR_Low = $0000000000000001;
  SPAR_Medium = $0000000000000002;
  SPAR_High = $0000000000000003;

Type
  SPCATEGORYTYPE = Integer;
Const
  SPCT_COMMAND = $0000000000000000;
  SPCT_DICTATION = $0000000000000001;
  SPCT_SLEEP = $0000000000000002;
  SPCT_SUB_COMMAND = $0000000000000003;
  SPCT_SUB_DICTATION = $0000000000000004;

Type
  SPLEXICONTYPE = Integer;
Const
  eLEXTYPE_USER = $0000000000000001;
  eLEXTYPE_APP = $0000000000000002;
  eLEXTYPE_VENDORLEXICON = $0000000000000004;
  eLEXTYPE_LETTERTOSOUND = $0000000000000008;
  eLEXTYPE_MORPHOLOGY = $0000000000000010;
  eLEXTYPE_RESERVED4 = $0000000000000020;
  eLEXTYPE_USER_SHORTCUT = $0000000000000040;
  eLEXTYPE_RESERVED6 = $0000000000000080;
  eLEXTYPE_RESERVED7 = $0000000000000100;
  eLEXTYPE_RESERVED8 = $0000000000000200;
  eLEXTYPE_RESERVED9 = $0000000000000400;
  eLEXTYPE_RESERVED10 = $0000000000000800;
  eLEXTYPE_PRIVATE1 = $0000000000001000;
  eLEXTYPE_PRIVATE2 = $0000000000002000;
  eLEXTYPE_PRIVATE3 = $0000000000004000;
  eLEXTYPE_PRIVATE4 = $0000000000008000;
  eLEXTYPE_PRIVATE5 = $0000000000010000;
  eLEXTYPE_PRIVATE6 = $0000000000020000;
  eLEXTYPE_PRIVATE7 = $0000000000040000;
  eLEXTYPE_PRIVATE8 = $0000000000080000;
  eLEXTYPE_PRIVATE9 = $0000000000100000;
  eLEXTYPE_PRIVATE10 = $0000000000200000;
  eLEXTYPE_PRIVATE11 = $0000000000400000;
  eLEXTYPE_PRIVATE12 = $0000000000800000;
  eLEXTYPE_PRIVATE13 = $0000000001000000;
  eLEXTYPE_PRIVATE14 = $0000000002000000;
  eLEXTYPE_PRIVATE15 = $0000000004000000;
  eLEXTYPE_PRIVATE16 = $0000000008000000;
  eLEXTYPE_PRIVATE17 = $0000000010000000;
  eLEXTYPE_PRIVATE18 = $0000000020000000;
  eLEXTYPE_PRIVATE19 = $0000000040000000;
  eLEXTYPE_PRIVATE20 = $FFFFFFFF80000000;

Type
  SPPARTOFSPEECH = Integer;
Const
  SPPS_NotOverriden = $FFFFFFFFFFFFFFFF;
  SPPS_Unknown = $0000000000000000;
  SPPS_Noun = $0000000000001000;
  SPPS_Verb = $0000000000002000;
  SPPS_Modifier = $0000000000003000;
  SPPS_Function = $0000000000004000;
  SPPS_Interjection = $0000000000005000;
  SPPS_Noncontent = $0000000000006000;
  SPPS_LMA = $0000000000007000;
  SPPS_SuppressWord = $000000000000F000;

Type
  SPWORDTYPE = Integer;
Const
  eWORDTYPE_ADDED = $0000000000000001;
  eWORDTYPE_DELETED = $0000000000000002;

Type
  SPSHORTCUTTYPE = Integer;
Const
  SPSHT_NotOverriden = $FFFFFFFFFFFFFFFF;
  SPSHT_Unknown = $0000000000000000;
  SPSHT_EMAIL = $0000000000001000;
  SPSHT_OTHER = $0000000000002000;
  SPPS_RESERVED1 = $0000000000003000;
  SPPS_RESERVED2 = $0000000000004000;
  SPPS_RESERVED3 = $0000000000005000;
  SPPS_RESERVED4 = $000000000000F000;

//Forward declarations

Type
 ISpeechDataKey = interface;
 ISpeechDataKeyDisp = dispinterface;
 ISpeechObjectToken = interface;
 ISpeechObjectTokenDisp = dispinterface;
 ISpeechObjectTokenCategory = interface;
 ISpeechObjectTokenCategoryDisp = dispinterface;
 ISpeechObjectTokens = interface;
 ISpeechObjectTokensDisp = dispinterface;
 ISpeechAudioBufferInfo = interface;
 ISpeechAudioBufferInfoDisp = dispinterface;
 ISpeechAudioStatus = interface;
 ISpeechAudioStatusDisp = dispinterface;
 ISpeechAudioFormat = interface;
 ISpeechAudioFormatDisp = dispinterface;
 ISpeechWaveFormatEx = interface;
 ISpeechWaveFormatExDisp = dispinterface;
 ISpeechBaseStream = interface;
 ISpeechBaseStreamDisp = dispinterface;
 ISpeechFileStream = interface;
 ISpeechFileStreamDisp = dispinterface;
 ISpeechMemoryStream = interface;
 ISpeechMemoryStreamDisp = dispinterface;
 ISpeechCustomStream = interface;
 ISpeechCustomStreamDisp = dispinterface;
 ISpeechAudio = interface;
 ISpeechAudioDisp = dispinterface;
 ISpeechMMSysAudio = interface;
 ISpeechMMSysAudioDisp = dispinterface;
 ISpeechVoice = interface;
 ISpeechVoiceDisp = dispinterface;
 ISpeechVoiceStatus = interface;
 ISpeechVoiceStatusDisp = dispinterface;
 _ISpeechVoiceEvents = dispinterface;
 ISpeechRecognizer = interface;
 ISpeechRecognizerDisp = dispinterface;
 ISpeechRecognizerStatus = interface;
 ISpeechRecognizerStatusDisp = dispinterface;
 ISpeechRecoContext = interface;
 ISpeechRecoContextDisp = dispinterface;
 ISpeechRecoGrammar = interface;
 ISpeechRecoGrammarDisp = dispinterface;
 ISpeechGrammarRules = interface;
 ISpeechGrammarRulesDisp = dispinterface;
 ISpeechGrammarRule = interface;
 ISpeechGrammarRuleDisp = dispinterface;
 ISpeechGrammarRuleState = interface;
 ISpeechGrammarRuleStateDisp = dispinterface;
 ISpeechGrammarRuleStateTransitions = interface;
 ISpeechGrammarRuleStateTransitionsDisp = dispinterface;
 ISpeechGrammarRuleStateTransition = interface;
 ISpeechGrammarRuleStateTransitionDisp = dispinterface;
 ISpeechTextSelectionInformation = interface;
 ISpeechTextSelectionInformationDisp = dispinterface;
 ISpeechRecoResult = interface;
 ISpeechRecoResultDisp = dispinterface;
 ISpeechRecoResultTimes = interface;
 ISpeechRecoResultTimesDisp = dispinterface;
 ISpeechPhraseInfo = interface;
 ISpeechPhraseInfoDisp = dispinterface;
 ISpeechPhraseRule = interface;
 ISpeechPhraseRuleDisp = dispinterface;
 ISpeechPhraseRules = interface;
 ISpeechPhraseRulesDisp = dispinterface;
 ISpeechPhraseProperties = interface;
 ISpeechPhrasePropertiesDisp = dispinterface;
 ISpeechPhraseProperty = interface;
 ISpeechPhrasePropertyDisp = dispinterface;
 ISpeechPhraseElements = interface;
 ISpeechPhraseElementsDisp = dispinterface;
 ISpeechPhraseElement = interface;
 ISpeechPhraseElementDisp = dispinterface;
 ISpeechPhraseReplacements = interface;
 ISpeechPhraseReplacementsDisp = dispinterface;
 ISpeechPhraseReplacement = interface;
 ISpeechPhraseReplacementDisp = dispinterface;
 ISpeechPhraseAlternates = interface;
 ISpeechPhraseAlternatesDisp = dispinterface;
 ISpeechPhraseAlternate = interface;
 ISpeechPhraseAlternateDisp = dispinterface;
 _ISpeechRecoContextEvents = dispinterface;
 ISpeechRecoResult2 = interface;
 ISpeechRecoResult2Disp = dispinterface;
 ISpeechLexicon = interface;
 ISpeechLexiconDisp = dispinterface;
 ISpeechLexiconWords = interface;
 ISpeechLexiconWordsDisp = dispinterface;
 ISpeechLexiconWord = interface;
 ISpeechLexiconWordDisp = dispinterface;
 ISpeechLexiconPronunciations = interface;
 ISpeechLexiconPronunciationsDisp = dispinterface;
 ISpeechLexiconPronunciation = interface;
 ISpeechLexiconPronunciationDisp = dispinterface;
 ISpeechXMLRecoResult = interface;
 ISpeechXMLRecoResultDisp = dispinterface;
 ISpeechRecoResultDispatch = interface;
 ISpeechRecoResultDispatchDisp = dispinterface;
 ISpeechPhraseInfoBuilder = interface;
 ISpeechPhraseInfoBuilderDisp = dispinterface;
 ISpeechPhoneConverter = interface;
 ISpeechPhoneConverterDisp = dispinterface;
 ISpNotifyTranslator = interface;
 ISpNotifySink = interface;
 ISpObjectTokenCategory = interface;
 ISpDataKey = interface;
 IEnumSpObjectTokens = interface;
 ISpObjectToken = interface;
 ISpResourceManager = interface;
 IServiceProvider = interface;
 ISpStreamFormatConverter = interface;
 ISpStreamFormat = interface;
 IStream = interface;
 ISequentialStream = interface;
 ISpEventSource = interface;
 ISpNotifySource = interface;
 ISpEventSink = interface;
 ISpObjectWithToken = interface;
 ISpMMSysAudio = interface;
 ISpAudio = interface;
 ISpStream = interface;
 ISpVoice = interface;
 ISpPhoneticAlphabetSelection = interface;
 ISpRecoContext = interface;
 ISpRecognizer = interface;
 ISpProperties = interface;
 ISpPhrase = interface;
 ISpRecoGrammar = interface;
 ISpGrammarBuilder = interface;
 ISpRecoResult = interface;
 ISpPhraseAlt = interface;
 ISpRecoContext2 = interface;
 ISpRecognizer2 = interface;
 ISpRecognizer3 = interface;
 ISpRecoCategory = interface;
 ISpSerializeState = interface;
 ISpLexicon = interface;
 ISpShortcut = interface;
 ISpPhoneConverter = interface;
 ISpPhoneticAlphabetConverter = interface;
 ISpXMLRecoResult = interface;
 ISpRecoGrammar2 = interface;
 ISpeechResourceLoader = interface;
 ISpeechResourceLoaderDisp = dispinterface;
 IInternetSecurityManager = interface;
 IInternetSecurityMgrSite = interface;
 IEnumString = interface;

//Map CoClass to its default interface

 SpNotifyTranslator = ISpNotifyTranslator;
 SpObjectTokenCategory = ISpeechObjectTokenCategory;
 SpObjectToken = ISpeechObjectToken;
 SpResourceManager = ISpResourceManager;
 SpStreamFormatConverter = ISpStreamFormatConverter;
 SpMMAudioEnum = IEnumSpObjectTokens;
 SpMMAudioIn = ISpeechMMSysAudio;
 SpMMAudioOut = ISpeechMMSysAudio;
 SpStream = ISpStream;
 SpVoice = ISpeechVoice;
 SpSharedRecoContext = ISpeechRecoContext;
 SpInprocRecognizer = ISpeechRecognizer;
 SpSharedRecognizer = ISpeechRecognizer;
 SpLexicon = ISpeechLexicon;
 SpUnCompressedLexicon = ISpeechLexicon;
 SpCompressedLexicon = ISpLexicon;
 SpShortcut = ISpShortcut;
 SpPhoneConverter = ISpeechPhoneConverter;
 SpPhoneticAlphabetConverter = ISpPhoneticAlphabetConverter;
 SpNullPhoneConverter = ISpPhoneConverter;
 SpTextSelectionInformation = ISpeechTextSelectionInformation;
 SpPhraseInfoBuilder = ISpeechPhraseInfoBuilder;
 SpAudioFormat = ISpeechAudioFormat;
 SpWaveFormatEx = ISpeechWaveFormatEx;
 SpInProcRecoContext = ISpeechRecoContext;
 SpCustomStream = ISpeechCustomStream;
 SpFileStream = ISpeechFileStream;
 SpMemoryStream = ISpeechMemoryStream;

//records, unions, aliases

 P_RemotableHandle = ^_RemotableHandle;

 wireHWND = P_RemotableHandle;
 P__MIDL_IWinTypes_0009 = ^__MIDL_IWinTypes_0009;

 __MIDL_IWinTypes_0009 =  record
    case Integer of
     0: (hInproc : Integer);
     1: (hRemote : Integer);
 end;
 _RemotableHandle = packed record
     fContext : Integer;
     u : __MIDL_IWinTypes_0009;
 end;
 UINT_PTR = LongWord;
 LONG_PTR = Integer;
 P_LARGE_INTEGER = ^_LARGE_INTEGER;

 _LARGE_INTEGER = packed record
     QuadPart : Int64;
 end;
 P_ULARGE_INTEGER = ^_ULARGE_INTEGER;

 _ULARGE_INTEGER = packed record
     QuadPart : QWord;
 end;
 PtagSTATSTG = ^tagSTATSTG;

 P_FILETIME = ^_FILETIME;

 _FILETIME = packed record
     dwLowDateTime : LongWord;
     dwHighDateTime : LongWord;
 end;
 tagSTATSTG = packed record
     pwcsName : PWideChar;
     Type_ : LongWord;
     cbSize : _ULARGE_INTEGER;
     mtime : _FILETIME;
     ctime : _FILETIME;
     atime : _FILETIME;
     grfMode : LongWord;
     grfLocksSupported : LongWord;
     clsid : TGUID;
     grfStateBits : LongWord;
     reserved : LongWord;
 end;
 PWAVEFORMATEX = ^WAVEFORMATEX;

 WAVEFORMATEX = packed record
     wFormatTag : Word;
     nChannels : Word;
     nSamplesPerSec : LongWord;
     nAvgBytesPerSec : LongWord;
     nBlockAlign : Word;
     wBitsPerSample : Word;
     cbSize : Word;
 end;
 PSPEVENT = ^SPEVENT;

 SPEVENT = packed record
     eEventId : Word;
     elParamType : Word;
     ulStreamNum : LongWord;
     ullAudioStreamOffset : QWord;
     wParam : UINT_PTR;
     lParam : LONG_PTR;
 end;
 PSPEVENTSOURCEINFO = ^SPEVENTSOURCEINFO;

 SPEVENTSOURCEINFO = packed record
     ullEventInterest : QWord;
     ullQueuedInterest : QWord;
     ulCount : LongWord;
 end;
 SPAUDIOSTATE = _SPAUDIOSTATE;
 PSPAUDIOSTATUS = ^SPAUDIOSTATUS;

 SPAUDIOSTATUS = packed record
     cbFreeBuffSpace : Integer;
     cbNonBlockingIO : LongWord;
     State : SPAUDIOSTATE;
     CurSeekPos : QWord;
     CurDevicePos : QWord;
     dwAudioLevel : LongWord;
     dwReserved2 : LongWord;
 end;
 PSPAUDIOBUFFERINFO = ^SPAUDIOBUFFERINFO;

 SPAUDIOBUFFERINFO = packed record
     ulMsMinNotification : LongWord;
     ulMsBufferSize : LongWord;
     ulMsEventBias : LongWord;
 end;
 PSPVOICESTATUS = ^SPVOICESTATUS;

 SPVOICESTATUS = packed record
     ulCurrentStream : LongWord;
     ulLastStreamQueued : LongWord;
     hrLastResult : HResult;
     dwRunningState : LongWord;
     ulInputWordPos : LongWord;
     ulInputWordLen : LongWord;
     ulInputSentPos : LongWord;
     ulInputSentLen : LongWord;
     lBookmarkId : Integer;
     PhonemeId : Word;
     VisemeId : SPVISEMES;
     dwReserved1 : LongWord;
     dwReserved2 : LongWord;
 end;
 PSPRECOGNIZERSTATUS = ^SPRECOGNIZERSTATUS;

 SPRECOGNIZERSTATUS = packed record
     AudioStatus : SPAUDIOSTATUS;
     ullRecognitionStreamPos : QWord;
     ulStreamNumber : LongWord;
     ulNumActive : LongWord;
     ClsidEngine : TGUID;
     cLangIDs : LongWord;
     aLangID : array[0..19] of Word;
     ullRecognitionStreamTime : QWord;
 end;
 SPSTREAMFORMATTYPE = SPWAVEFORMATTYPE;
 PSPPHRASE = ^SPPHRASE;

 PSPPHRASERULE = ^SPPHRASERULE;

 SPPHRASERULE = packed record
     pszName : PWideChar;
     ulId : LongWord;
     ulFirstElement : LongWord;
     ulCountOfElements : LongWord;
     pNextSibling : PSPPHRASERULE;
     pFirstChild : PSPPHRASERULE;
     SREngineConfidence : Single;
     Confidence : ShortInt;
 end;
 PSPPHRASEPROPERTY = ^SPPHRASEPROPERTY;

 P__MIDL___MIDL_itf_sapi_0000_0020_0001 = ^__MIDL___MIDL_itf_sapi_0000_0020_0001;

 P__MIDL___MIDL_itf_sapi_0000_0020_0002 = ^__MIDL___MIDL_itf_sapi_0000_0020_0002;

 __MIDL___MIDL_itf_sapi_0000_0020_0002 = packed record
     bType : Byte;
     bReserved : Byte;
     usArrayIndex : Word;
 end;
 __MIDL___MIDL_itf_sapi_0000_0020_0001 =  record
    case Integer of
     0: (ulId : LongWord);
     1: (__MIDL____MIDL_itf_sapi_0000_00200000 : __MIDL___MIDL_itf_sapi_0000_0020_0002);
 end;
 SPPHRASEPROPERTY = packed record
     pszName : PWideChar;
     __MIDL____MIDL_itf_sapi_0000_00200001 : __MIDL___MIDL_itf_sapi_0000_0020_0001;
     pszValue : PWideChar;
     vValue : OleVariant;
     ulFirstElement : LongWord;
     ulCountOfElements : LongWord;
     pNextSibling : PSPPHRASEPROPERTY;
     pFirstChild : PSPPHRASEPROPERTY;
     SREngineConfidence : Single;
     Confidence : ShortInt;
 end;
 PSPPHRASEELEMENT = ^SPPHRASEELEMENT;

 SPPHRASEELEMENT = packed record
     ulAudioTimeOffset : LongWord;
     ulAudioSizeTime : LongWord;
     ulAudioStreamOffset : LongWord;
     ulAudioSizeBytes : LongWord;
     ulRetainedStreamOffset : LongWord;
     ulRetainedSizeBytes : LongWord;
     pszDisplayText : PWideChar;
     pszLexicalForm : PWideChar;
     pszPronunciation : PWord;
     bDisplayAttributes : Byte;
     RequiredConfidence : ShortInt;
     ActualConfidence : ShortInt;
     reserved : Byte;
     SREngineConfidence : Single;
 end;
 PSPPHRASEREPLACEMENT = ^SPPHRASEREPLACEMENT;

 SPPHRASEREPLACEMENT = packed record
     bDisplayAttributes : Byte;
     pszReplacementText : PWideChar;
     ulFirstElement : LongWord;
     ulCountOfElements : LongWord;
 end;
 PSPSEMANTICERRORINFO = ^SPSEMANTICERRORINFO;

 SPPHRASE = packed record
     cbSize : LongWord;
     LangId : Word;
     wHomophoneGroupId : Word;
     ullGrammarID : QWord;
     ftStartTime : QWord;
     ullAudioStreamPosition : QWord;
     ulAudioSizeBytes : LongWord;
     ulRetainedSizeBytes : LongWord;
     ulAudioSizeTime : LongWord;
     Rule : SPPHRASERULE;
     pProperties : PSPPHRASEPROPERTY;
     pElements : PSPPHRASEELEMENT;
     cReplacements : LongWord;
     pReplacements : PSPPHRASEREPLACEMENT;
     SREngineID : TGUID;
     ulSREnginePrivateDataSize : LongWord;
     pSREnginePrivateData : PByte;
     pSML : PWideChar;
     pSemanticErrorInfo : PSPSEMANTICERRORINFO;
     SemanticTagFormat : SPSEMANTICFORMAT;
 end;
 SPSEMANTICERRORINFO = packed record
     ulLineNumber : LongWord;
     pszScriptLine : PWideChar;
     pszSource : PWideChar;
     pszDescription : PWideChar;
     hrResultCode : HResult;
 end;
 PSPSERIALIZEDPHRASE = ^SPSERIALIZEDPHRASE;

 SPSERIALIZEDPHRASE = packed record
     ulSerializedSize : LongWord;
 end;
 PtagSPPROPERTYINFO = ^tagSPPROPERTYINFO;

 tagSPPROPERTYINFO = packed record
     pszName : PWideChar;
     ulId : LongWord;
     pszValue : PWideChar;
     vValue : OleVariant;
 end;
 SPPROPERTYINFO = tagSPPROPERTYINFO;
 PSPBINARYGRAMMAR = ^SPBINARYGRAMMAR;

 SPBINARYGRAMMAR = packed record
     ulTotalSerializedSize : LongWord;
 end;
 PtagSPTEXTSELECTIONINFO = ^tagSPTEXTSELECTIONINFO;

 tagSPTEXTSELECTIONINFO = packed record
     ulStartActiveOffset : LongWord;
     cchActiveChars : LongWord;
     ulStartSelection : LongWord;
     cchSelection : LongWord;
 end;
 SPTEXTSELECTIONINFO = tagSPTEXTSELECTIONINFO;
 PSPRECOCONTEXTSTATUS = ^SPRECOCONTEXTSTATUS;

 SPRECOCONTEXTSTATUS = packed record
     eInterference : SPINTERFERENCE;
     szRequestTypeOfUI : array[0..254] of Word;
     dwReserved1 : LongWord;
     dwReserved2 : LongWord;
 end;
 PSPSERIALIZEDRESULT = ^SPSERIALIZEDRESULT;

 SPSERIALIZEDRESULT = packed record
     ulSerializedSize : LongWord;
 end;
 PSPRECORESULTTIMES = ^SPRECORESULTTIMES;

 SPRECORESULTTIMES = packed record
     ftStreamTime : _FILETIME;
     ullLength : QWord;
     dwTickCount : LongWord;
     ullStart : QWord;
 end;
 PSPWORDPRONUNCIATIONLIST = ^SPWORDPRONUNCIATIONLIST;

 PSPWORDPRONUNCIATION = ^SPWORDPRONUNCIATION;

 SPWORDPRONUNCIATIONLIST = packed record
     ulSize : LongWord;
     pvBuffer : PByte;
     pFirstWordPronunciation : PSPWORDPRONUNCIATION;
 end;
 SPWORDPRONUNCIATION = packed record
     pNextWordPronunciation : PSPWORDPRONUNCIATION;
     eLexiconType : SPLEXICONTYPE;
     LangId : Word;
     wPronunciationFlags : Word;
     ePartOfSpeech : SPPARTOFSPEECH;
     szPronunciation : array[0..0] of Word;
 end;
 PSPWORDLIST = ^SPWORDLIST;

 PSPWORD = ^SPWORD;

 SPWORDLIST = packed record
     ulSize : LongWord;
     pvBuffer : PByte;
     pFirstWord : PSPWORD;
 end;
 SPWORD = packed record
     pNextWord : PSPWORD;
     LangId : Word;
     wReserved : Word;
     eWordType : SPWORDTYPE;
     pszWord : PWideChar;
     pFirstWordPronunciation : PSPWORDPRONUNCIATION;
 end;
 PSPSHORTCUTPAIRLIST = ^SPSHORTCUTPAIRLIST;

 PSPSHORTCUTPAIR = ^SPSHORTCUTPAIR;

 SPSHORTCUTPAIRLIST = packed record
     ulSize : LongWord;
     pvBuffer : PByte;
     pFirstShortcutPair : PSPSHORTCUTPAIR;
 end;
 SPSHORTCUTPAIR = packed record
     pNextSHORTCUTPAIR : PSPSHORTCUTPAIR;
     LangId : Word;
     shType : SPSHORTCUTTYPE;
     pszDisplay : PWideChar;
     pszSpoken : PWideChar;
 end;
 PSPRULE = ^SPRULE;

 SPRULE = packed record
     pszRuleName : PWideChar;
     ulRuleId : LongWord;
     dwAttributes : LongWord;
 end;
 ULONG_PTR = LongWord;

//interface declarations

// ISpeechDataKey : ISpeechDataKey Interface

 ISpeechDataKey = interface(IDispatch)
   ['{CE17C09B-4EFA-44D5-A4C9-59D9585AB0CD}']
    // SetBinaryValue : SetBinaryValue
   procedure SetBinaryValue(ValueName:WideString;Value:OleVariant);safecall;
    // GetBinaryValue : GetBinaryValue
   function GetBinaryValue(ValueName:WideString): OleVariant;safecall;
    // SetStringValue : SetStringValue
   procedure SetStringValue(ValueName:WideString;Value:WideString);safecall;
    // GetStringValue : GetStringValue
   function GetStringValue(ValueName:WideString): WideString;safecall;
    // SetLongValue : SetLongValue
   procedure SetLongValue(ValueName:WideString;Value:Integer);safecall;
    // GetLongValue : GetlongValue
   function GetLongValue(ValueName:WideString): Integer;safecall;
    // OpenKey : OpenKey
   function OpenKey(SubKeyName:WideString): ISpeechDataKey;safecall;
    // CreateKey : CreateKey
   function CreateKey(SubKeyName:WideString): ISpeechDataKey;safecall;
    // DeleteKey : DeleteKey
   procedure DeleteKey(SubKeyName:WideString);safecall;
    // DeleteValue : DeleteValue
   procedure DeleteValue(ValueName:WideString);safecall;
    // EnumKeys : EnumKeys
   function EnumKeys(Index:Integer): WideString;safecall;
    // EnumValues : EnumValues
   function EnumValues(Index:Integer): WideString;safecall;
  end;


// ISpeechDataKey : ISpeechDataKey Interface

 ISpeechDataKeyDisp = dispinterface
   ['{CE17C09B-4EFA-44D5-A4C9-59D9585AB0CD}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // SetBinaryValue : SetBinaryValue
   procedure SetBinaryValue(ValueName:WideString;Value:OleVariant);dispid 1;
    // GetBinaryValue : GetBinaryValue
   function GetBinaryValue(ValueName:WideString): OleVariant;dispid 2;
    // SetStringValue : SetStringValue
   procedure SetStringValue(ValueName:WideString;Value:WideString);dispid 3;
    // GetStringValue : GetStringValue
   function GetStringValue(ValueName:WideString): WideString;dispid 4;
    // SetLongValue : SetLongValue
   procedure SetLongValue(ValueName:WideString;Value:Integer);dispid 5;
    // GetLongValue : GetlongValue
   function GetLongValue(ValueName:WideString): Integer;dispid 6;
    // OpenKey : OpenKey
   function OpenKey(SubKeyName:WideString): ISpeechDataKey;dispid 7;
    // CreateKey : CreateKey
   function CreateKey(SubKeyName:WideString): ISpeechDataKey;dispid 8;
    // DeleteKey : DeleteKey
   procedure DeleteKey(SubKeyName:WideString);dispid 9;
    // DeleteValue : DeleteValue
   procedure DeleteValue(ValueName:WideString);dispid 10;
    // EnumKeys : EnumKeys
   function EnumKeys(Index:Integer): WideString;dispid 11;
    // EnumValues : EnumValues
   function EnumValues(Index:Integer): WideString;dispid 12;
  end;


// ISpeechObjectToken : ISpeechObjectToken Interface

 ISpeechObjectToken = interface(IDispatch)
   ['{C74A3ADC-B727-4500-A84A-B526721C8B8C}']
   function Get_Id : WideString; safecall;
   function Get_DataKey : ISpeechDataKey; safecall;
   function Get_Category : ISpeechObjectTokenCategory; safecall;
    // GetDescription : GetDescription
   function GetDescription(Locale:Integer): WideString;safecall;
    // SetId : SetId
   procedure SetId(Id:WideString;CategoryID:WideString;CreateIfNotExist:WordBool);safecall;
    // GetAttribute : GetAttribute
   function GetAttribute(AttributeName:WideString): WideString;safecall;
    // CreateInstance : CreateInstance
   function CreateInstance(pUnkOuter:IUnknown;ClsContext:SpeechTokenContext): IUnknown;safecall;
    // Remove : Remove
   procedure Remove(ObjectStorageCLSID:WideString);safecall;
    // GetStorageFileName : GetStorageFileName
   function GetStorageFileName(ObjectStorageCLSID:WideString;KeyName:WideString;FileName:WideString;Folder:SpeechTokenShellFolder): WideString;safecall;
    // RemoveStorageFileName : RemoveStorageFileName
   procedure RemoveStorageFileName(ObjectStorageCLSID:WideString;KeyName:WideString;DeleteFile:WordBool);safecall;
    // IsUISupported : IsUISupported
   function IsUISupported(TypeOfUI:WideString;var ExtraData:OleVariant;Object_:IUnknown): WordBool;safecall;
    // DisplayUI : DisplayUI
   procedure DisplayUI(hWnd:Integer;Title:WideString;TypeOfUI:WideString;var ExtraData:OleVariant;Object_:IUnknown);safecall;
    // MatchesAttributes : MatchesAttributes
   function MatchesAttributes(Attributes:WideString): WordBool;safecall;
    // Id : Id
   property Id: WideString read Get_Id;
    // DataKey : DataKey
   property DataKey: ISpeechDataKey read Get_DataKey;
    // Category : Category
   property Category: ISpeechObjectTokenCategory read Get_Category;
  end;


// ISpeechObjectToken : ISpeechObjectToken Interface

 ISpeechObjectTokenDisp = dispinterface
   ['{C74A3ADC-B727-4500-A84A-B526721C8B8C}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // GetDescription : GetDescription
   function GetDescription(Locale:Integer): WideString;dispid 4;
    // SetId : SetId
   procedure SetId(Id:WideString;CategoryID:WideString;CreateIfNotExist:WordBool);dispid 5;
    // GetAttribute : GetAttribute
   function GetAttribute(AttributeName:WideString): WideString;dispid 6;
    // CreateInstance : CreateInstance
   function CreateInstance(pUnkOuter:IUnknown;ClsContext:SpeechTokenContext): IUnknown;dispid 7;
    // Remove : Remove
   procedure Remove(ObjectStorageCLSID:WideString);dispid 8;
    // GetStorageFileName : GetStorageFileName
   function GetStorageFileName(ObjectStorageCLSID:WideString;KeyName:WideString;FileName:WideString;Folder:SpeechTokenShellFolder): WideString;dispid 9;
    // RemoveStorageFileName : RemoveStorageFileName
   procedure RemoveStorageFileName(ObjectStorageCLSID:WideString;KeyName:WideString;DeleteFile:WordBool);dispid 10;
    // IsUISupported : IsUISupported
   function IsUISupported(TypeOfUI:WideString;var ExtraData:OleVariant;Object_:IUnknown): WordBool;dispid 11;
    // DisplayUI : DisplayUI
   procedure DisplayUI(hWnd:Integer;Title:WideString;TypeOfUI:WideString;var ExtraData:OleVariant;Object_:IUnknown);dispid 12;
    // MatchesAttributes : MatchesAttributes
   function MatchesAttributes(Attributes:WideString): WordBool;dispid 13;
    // Id : Id
   property Id: WideString  readonly dispid 1;
    // DataKey : DataKey
   property DataKey: ISpeechDataKey  readonly dispid 2;
    // Category : Category
   property Category: ISpeechObjectTokenCategory  readonly dispid 3;
  end;


// ISpeechObjectTokenCategory : ISpeechObjectTokenCategory Interface

 ISpeechObjectTokenCategory = interface(IDispatch)
   ['{CA7EAC50-2D01-4145-86D4-5AE7D70F4469}']
   function Get_Id : WideString; safecall;
   procedure Set_Default(const TokenId:WideString); safecall;
   function Get_Default : WideString; safecall;
    // SetId : SetId
   procedure SetId(Id:WideString;CreateIfNotExist:WordBool);safecall;
    // GetDataKey : GetDataKey
   function GetDataKey(Location:SpeechDataKeyLocation): ISpeechDataKey;safecall;
    // EnumerateTokens : EnumerateTokens
   function EnumerateTokens(RequiredAttributes:WideString;OptionalAttributes:WideString): ISpeechObjectTokens;safecall;
    // Id : Id
   property Id: WideString read Get_Id;
    // Default : Default
   property Default: WideString read Get_Default write Set_Default;
  end;


// ISpeechObjectTokenCategory : ISpeechObjectTokenCategory Interface

 ISpeechObjectTokenCategoryDisp = dispinterface
   ['{CA7EAC50-2D01-4145-86D4-5AE7D70F4469}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // SetId : SetId
   procedure SetId(Id:WideString;CreateIfNotExist:WordBool);dispid 3;
    // GetDataKey : GetDataKey
   function GetDataKey(Location:SpeechDataKeyLocation): ISpeechDataKey;dispid 4;
    // EnumerateTokens : EnumerateTokens
   function EnumerateTokens(RequiredAttributes:WideString;OptionalAttributes:WideString): ISpeechObjectTokens;dispid 5;
    // Id : Id
   property Id: WideString  readonly dispid 1;
    // Default : Default
   property Default: WideString dispid 2;
  end;


// ISpeechObjectTokens : ISpeechObjectTokens Interface

 ISpeechObjectTokens = interface(IDispatch)
   ['{9285B776-2E7B-4BC0-B53E-580EB6FA967F}']
   function Get_Count : Integer; safecall;
    // Item : Item
   function Item(Index:Integer): ISpeechObjectToken;safecall;
   function Get__NewEnum : IUnknown; safecall;
    // Count : Count
   property Count: Integer read Get_Count;
    // _NewEnum : Enumerates the tokens
   property _NewEnum: IUnknown read Get__NewEnum;
  end;


// ISpeechObjectTokens : ISpeechObjectTokens Interface

 ISpeechObjectTokensDisp = dispinterface
   ['{9285B776-2E7B-4BC0-B53E-580EB6FA967F}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Item : Item
   function Item(Index:Integer): ISpeechObjectToken;dispid 0;
    // Count : Count
   property Count: Integer  readonly dispid 1;
    // _NewEnum : Enumerates the tokens
   property _NewEnum: IUnknown  readonly dispid -4;
  end;


// ISpeechAudioBufferInfo : ISpeechAudioBufferInfo Interface

 ISpeechAudioBufferInfo = interface(IDispatch)
   ['{11B103D8-1142-4EDF-A093-82FB3915F8CC}']
   function Get_MinNotification : Integer; safecall;
   procedure Set_MinNotification(const MinNotification:Integer); safecall;
   function Get_BufferSize : Integer; safecall;
   procedure Set_BufferSize(const BufferSize:Integer); safecall;
   function Get_EventBias : Integer; safecall;
   procedure Set_EventBias(const EventBias:Integer); safecall;
    // MinNotification : MinNotification
   property MinNotification: Integer read Get_MinNotification write Set_MinNotification;
    // BufferSize : BufferSize
   property BufferSize: Integer read Get_BufferSize write Set_BufferSize;
    // EventBias : EventBias
   property EventBias: Integer read Get_EventBias write Set_EventBias;
  end;


// ISpeechAudioBufferInfo : ISpeechAudioBufferInfo Interface

 ISpeechAudioBufferInfoDisp = dispinterface
   ['{11B103D8-1142-4EDF-A093-82FB3915F8CC}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // MinNotification : MinNotification
   property MinNotification: Integer dispid 1;
    // BufferSize : BufferSize
   property BufferSize: Integer dispid 2;
    // EventBias : EventBias
   property EventBias: Integer dispid 3;
  end;


// ISpeechAudioStatus : ISpeechAudioStatus Interface

 ISpeechAudioStatus = interface(IDispatch)
   ['{C62D9C91-7458-47F6-862D-1EF86FB0B278}']
   function Get_FreeBufferSpace : Integer; safecall;
   function Get_NonBlockingIO : Integer; safecall;
   function Get_State : SpeechAudioState; safecall;
   function Get_CurrentSeekPosition : OleVariant; safecall;
   function Get_CurrentDevicePosition : OleVariant; safecall;
    // FreeBufferSpace : FreeBufferSpace
   property FreeBufferSpace: Integer read Get_FreeBufferSpace;
    // NonBlockingIO : NonBlockingIO
   property NonBlockingIO: Integer read Get_NonBlockingIO;
    // State : State
   property State: SpeechAudioState read Get_State;
    // CurrentSeekPosition : CurrentSeekPosition
   property CurrentSeekPosition: OleVariant read Get_CurrentSeekPosition;
    // CurrentDevicePosition : CurrentDevicePosition
   property CurrentDevicePosition: OleVariant read Get_CurrentDevicePosition;
  end;


// ISpeechAudioStatus : ISpeechAudioStatus Interface

 ISpeechAudioStatusDisp = dispinterface
   ['{C62D9C91-7458-47F6-862D-1EF86FB0B278}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // FreeBufferSpace : FreeBufferSpace
   property FreeBufferSpace: Integer  readonly dispid 1;
    // NonBlockingIO : NonBlockingIO
   property NonBlockingIO: Integer  readonly dispid 2;
    // State : State
   property State: SpeechAudioState  readonly dispid 3;
    // CurrentSeekPosition : CurrentSeekPosition
   property CurrentSeekPosition: OleVariant  readonly dispid 4;
    // CurrentDevicePosition : CurrentDevicePosition
   property CurrentDevicePosition: OleVariant  readonly dispid 5;
  end;


// ISpeechAudioFormat : ISpeechAudioFormat Interface

 ISpeechAudioFormat = interface(IDispatch)
   ['{E6E9C590-3E18-40E3-8299-061F98BDE7C7}']
   function Get_Type_ : SpeechAudioFormatType; safecall;
   procedure Set_Type_(const AudioFormat:SpeechAudioFormatType); safecall;
   function Get_Guid : WideString; safecall;
   procedure Set_Guid(const Guid:WideString); safecall;
    // GetWaveFormatEx : GetWaveFormatEx
   function GetWaveFormatEx: ISpeechWaveFormatEx;safecall;
    // SetWaveFormatEx : SetWaveFormatEx
   procedure SetWaveFormatEx(SpeechWaveFormatEx:ISpeechWaveFormatEx);safecall;
    // Type : Type
   property Type_: SpeechAudioFormatType read Get_Type_ write Set_Type_;
    // Guid : Guid
   property Guid: WideString read Get_Guid write Set_Guid;
  end;


// ISpeechAudioFormat : ISpeechAudioFormat Interface

 ISpeechAudioFormatDisp = dispinterface
   ['{E6E9C590-3E18-40E3-8299-061F98BDE7C7}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // GetWaveFormatEx : GetWaveFormatEx
   function GetWaveFormatEx: ISpeechWaveFormatEx;dispid 3;
    // SetWaveFormatEx : SetWaveFormatEx
   procedure SetWaveFormatEx(SpeechWaveFormatEx:ISpeechWaveFormatEx);dispid 4;
    // Type : Type
   property Type_: SpeechAudioFormatType dispid 1;
    // Guid : Guid
   property Guid: WideString dispid 2;
  end;


// ISpeechWaveFormatEx : ISpeechWaveFormatEx Interface

 ISpeechWaveFormatEx = interface(IDispatch)
   ['{7A1EF0D5-1581-4741-88E4-209A49F11A10}']
   function Get_FormatTag : Smallint; safecall;
   procedure Set_FormatTag(const FormatTag:Smallint); safecall;
   function Get_Channels : Smallint; safecall;
   procedure Set_Channels(const Channels:Smallint); safecall;
   function Get_SamplesPerSec : Integer; safecall;
   procedure Set_SamplesPerSec(const SamplesPerSec:Integer); safecall;
   function Get_AvgBytesPerSec : Integer; safecall;
   procedure Set_AvgBytesPerSec(const AvgBytesPerSec:Integer); safecall;
   function Get_BlockAlign : Smallint; safecall;
   procedure Set_BlockAlign(const BlockAlign:Smallint); safecall;
   function Get_BitsPerSample : Smallint; safecall;
   procedure Set_BitsPerSample(const BitsPerSample:Smallint); safecall;
   function Get_ExtraData : OleVariant; safecall;
   procedure Set_ExtraData(const ExtraData:OleVariant); safecall;
    // FormatTag : FormatTag
   property FormatTag: Smallint read Get_FormatTag write Set_FormatTag;
    // Channels : Channels
   property Channels: Smallint read Get_Channels write Set_Channels;
    // SamplesPerSec : SamplesPerSec
   property SamplesPerSec: Integer read Get_SamplesPerSec write Set_SamplesPerSec;
    // AvgBytesPerSec : AvgBytesPerSec
   property AvgBytesPerSec: Integer read Get_AvgBytesPerSec write Set_AvgBytesPerSec;
    // BlockAlign : BlockAlign
   property BlockAlign: Smallint read Get_BlockAlign write Set_BlockAlign;
    // BitsPerSample : BitsPerSample
   property BitsPerSample: Smallint read Get_BitsPerSample write Set_BitsPerSample;
    // ExtraData : ExtraData
   property ExtraData: OleVariant read Get_ExtraData write Set_ExtraData;
  end;


// ISpeechWaveFormatEx : ISpeechWaveFormatEx Interface

 ISpeechWaveFormatExDisp = dispinterface
   ['{7A1EF0D5-1581-4741-88E4-209A49F11A10}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // FormatTag : FormatTag
   property FormatTag: Smallint dispid 1;
    // Channels : Channels
   property Channels: Smallint dispid 2;
    // SamplesPerSec : SamplesPerSec
   property SamplesPerSec: Integer dispid 3;
    // AvgBytesPerSec : AvgBytesPerSec
   property AvgBytesPerSec: Integer dispid 4;
    // BlockAlign : BlockAlign
   property BlockAlign: Smallint dispid 5;
    // BitsPerSample : BitsPerSample
   property BitsPerSample: Smallint dispid 6;
    // ExtraData : ExtraData
   property ExtraData: OleVariant dispid 7;
  end;


// ISpeechBaseStream : ISpeechBaseStream Interface

 ISpeechBaseStream = interface(IDispatch)
   ['{6450336F-7D49-4CED-8097-49D6DEE37294}']
   function Get_Format : ISpeechAudioFormat; safecall;
   procedure Set_Format(const AudioFormat:ISpeechAudioFormat); safecall;
    // Read_ : Read
   function Read_(out Buffer:OleVariant;NumberOfBytes:Integer): Integer;safecall;
    // Write_ : Write
   function Write_(Buffer:OleVariant): Integer;safecall;
    // Seek : Seek
   function Seek(Position:OleVariant;Origin:SpeechStreamSeekPositionType): OleVariant;safecall;
    // Format : Format
   property Format: ISpeechAudioFormat read Get_Format write Set_Format;
  end;


// ISpeechBaseStream : ISpeechBaseStream Interface

 ISpeechBaseStreamDisp = dispinterface
   ['{6450336F-7D49-4CED-8097-49D6DEE37294}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Read_ : Read
   function Read_(out Buffer:OleVariant;NumberOfBytes:Integer): Integer;dispid 2;
    // Write_ : Write
   function Write_(Buffer:OleVariant): Integer;dispid 3;
    // Seek : Seek
   function Seek(Position:OleVariant;Origin:SpeechStreamSeekPositionType): OleVariant;dispid 4;
    // Format : Format
   property Format: ISpeechAudioFormat dispid 1;
  end;


// ISpeechFileStream : ISpeechFileStream Interface

 ISpeechFileStream = interface(ISpeechBaseStream)
   ['{AF67F125-AB39-4E93-B4A2-CC2E66E182A7}']
    // Open : Open
   procedure Open(FileName:WideString;FileMode:SpeechStreamFileMode;DoEvents:WordBool);safecall;
    // Close : Close
   procedure Close;safecall;
  end;


// ISpeechFileStream : ISpeechFileStream Interface

 ISpeechFileStreamDisp = dispinterface
   ['{AF67F125-AB39-4E93-B4A2-CC2E66E182A7}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Read_ : Read
   function Read_(out Buffer:OleVariant;NumberOfBytes:Integer): Integer;dispid 2;
    // Write_ : Write
   function Write_(Buffer:OleVariant): Integer;dispid 3;
    // Seek : Seek
   function Seek(Position:OleVariant;Origin:SpeechStreamSeekPositionType): OleVariant;dispid 4;
    // Open : Open
   procedure Open(FileName:WideString;FileMode:SpeechStreamFileMode;DoEvents:WordBool);dispid 100;
    // Close : Close
   procedure Close;dispid 101;
    // Format : Format
   property Format: ISpeechAudioFormat dispid 1;
  end;


// ISpeechMemoryStream : ISpeechMemoryStream Interface

 ISpeechMemoryStream = interface(ISpeechBaseStream)
   ['{EEB14B68-808B-4ABE-A5EA-B51DA7588008}']
    // SetData : SetData
   procedure SetData(Data:OleVariant);safecall;
    // GetData : GetData
   function GetData: OleVariant;safecall;
  end;


// ISpeechMemoryStream : ISpeechMemoryStream Interface

 ISpeechMemoryStreamDisp = dispinterface
   ['{EEB14B68-808B-4ABE-A5EA-B51DA7588008}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Read_ : Read
   function Read_(out Buffer:OleVariant;NumberOfBytes:Integer): Integer;dispid 2;
    // Write_ : Write
   function Write_(Buffer:OleVariant): Integer;dispid 3;
    // Seek : Seek
   function Seek(Position:OleVariant;Origin:SpeechStreamSeekPositionType): OleVariant;dispid 4;
    // SetData : SetData
   procedure SetData(Data:OleVariant);dispid 100;
    // GetData : GetData
   function GetData: OleVariant;dispid 101;
    // Format : Format
   property Format: ISpeechAudioFormat dispid 1;
  end;


// ISpeechCustomStream : ISpeechCustomStream Interface

 ISpeechCustomStream = interface(ISpeechBaseStream)
   ['{1A9E9F4F-104F-4DB8-A115-EFD7FD0C97AE}']
   function Get_BaseStream : IUnknown; safecall;
   procedure Set_BaseStream(const ppUnkStream:IUnknown); safecall;
    // BaseStream : BaseStream
   property BaseStream: IUnknown read Get_BaseStream write Set_BaseStream;
  end;


// ISpeechCustomStream : ISpeechCustomStream Interface

 ISpeechCustomStreamDisp = dispinterface
   ['{1A9E9F4F-104F-4DB8-A115-EFD7FD0C97AE}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Read_ : Read
   function Read_(out Buffer:OleVariant;NumberOfBytes:Integer): Integer;dispid 2;
    // Write_ : Write
   function Write_(Buffer:OleVariant): Integer;dispid 3;
    // Seek : Seek
   function Seek(Position:OleVariant;Origin:SpeechStreamSeekPositionType): OleVariant;dispid 4;
    // Format : Format
   property Format: ISpeechAudioFormat dispid 1;
    // BaseStream : BaseStream
   property BaseStream: IUnknown dispid 100;
  end;


// ISpeechAudio : ISpeechAudio Interface

 ISpeechAudio = interface(ISpeechBaseStream)
   ['{CFF8E175-019E-11D3-A08E-00C04F8EF9B5}']
   function Get_Status : ISpeechAudioStatus; safecall;
   function Get_BufferInfo : ISpeechAudioBufferInfo; safecall;
   function Get_DefaultFormat : ISpeechAudioFormat; safecall;
   function Get_Volume : Integer; safecall;
   procedure Set_Volume(const Volume:Integer); safecall;
   function Get_BufferNotifySize : Integer; safecall;
   procedure Set_BufferNotifySize(const BufferNotifySize:Integer); safecall;
   function Get_EventHandle : Integer; safecall;
    // SetState : SetState
   procedure SetState(State:SpeechAudioState);safecall;
    // Status : Status
   property Status: ISpeechAudioStatus read Get_Status;
    // BufferInfo : BufferInfo
   property BufferInfo: ISpeechAudioBufferInfo read Get_BufferInfo;
    // DefaultFormat : DefaultFormat
   property DefaultFormat: ISpeechAudioFormat read Get_DefaultFormat;
    // Volume : Volume
   property Volume: Integer read Get_Volume write Set_Volume;
    // BufferNotifySize : BufferNotifySize
   property BufferNotifySize: Integer read Get_BufferNotifySize write Set_BufferNotifySize;
    // EventHandle : EventHandle
   property EventHandle: Integer read Get_EventHandle;
  end;


// ISpeechAudio : ISpeechAudio Interface

 ISpeechAudioDisp = dispinterface
   ['{CFF8E175-019E-11D3-A08E-00C04F8EF9B5}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Read_ : Read
   function Read_(out Buffer:OleVariant;NumberOfBytes:Integer): Integer;dispid 2;
    // Write_ : Write
   function Write_(Buffer:OleVariant): Integer;dispid 3;
    // Seek : Seek
   function Seek(Position:OleVariant;Origin:SpeechStreamSeekPositionType): OleVariant;dispid 4;
    // SetState : SetState
   procedure SetState(State:SpeechAudioState);dispid 206;
    // Format : Format
   property Format: ISpeechAudioFormat dispid 1;
    // Status : Status
   property Status: ISpeechAudioStatus  readonly dispid 200;
    // BufferInfo : BufferInfo
   property BufferInfo: ISpeechAudioBufferInfo  readonly dispid 201;
    // DefaultFormat : DefaultFormat
   property DefaultFormat: ISpeechAudioFormat  readonly dispid 202;
    // Volume : Volume
   property Volume: Integer dispid 203;
    // BufferNotifySize : BufferNotifySize
   property BufferNotifySize: Integer dispid 204;
    // EventHandle : EventHandle
   property EventHandle: Integer  readonly dispid 205;
  end;


// ISpeechMMSysAudio : ISpeechMMSysAudio Interface

 ISpeechMMSysAudio = interface(ISpeechAudio)
   ['{3C76AF6D-1FD7-4831-81D1-3B71D5A13C44}']
   function Get_DeviceId : Integer; safecall;
   procedure Set_DeviceId(const DeviceId:Integer); safecall;
   function Get_LineId : Integer; safecall;
   procedure Set_LineId(const LineId:Integer); safecall;
   function Get_MMHandle : Integer; safecall;
    // DeviceId : DeviceId
   property DeviceId: Integer read Get_DeviceId write Set_DeviceId;
    // LineId : LineId
   property LineId: Integer read Get_LineId write Set_LineId;
    // MMHandle : MMHandle
   property MMHandle: Integer read Get_MMHandle;
  end;


// ISpeechMMSysAudio : ISpeechMMSysAudio Interface

 ISpeechMMSysAudioDisp = dispinterface
   ['{3C76AF6D-1FD7-4831-81D1-3B71D5A13C44}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Read_ : Read
   function Read_(out Buffer:OleVariant;NumberOfBytes:Integer): Integer;dispid 2;
    // Write_ : Write
   function Write_(Buffer:OleVariant): Integer;dispid 3;
    // Seek : Seek
   function Seek(Position:OleVariant;Origin:SpeechStreamSeekPositionType): OleVariant;dispid 4;
    // SetState : SetState
   procedure SetState(State:SpeechAudioState);dispid 206;
    // Format : Format
   property Format: ISpeechAudioFormat dispid 1;
    // Status : Status
   property Status: ISpeechAudioStatus  readonly dispid 200;
    // BufferInfo : BufferInfo
   property BufferInfo: ISpeechAudioBufferInfo  readonly dispid 201;
    // DefaultFormat : DefaultFormat
   property DefaultFormat: ISpeechAudioFormat  readonly dispid 202;
    // Volume : Volume
   property Volume: Integer dispid 203;
    // BufferNotifySize : BufferNotifySize
   property BufferNotifySize: Integer dispid 204;
    // EventHandle : EventHandle
   property EventHandle: Integer  readonly dispid 205;
    // DeviceId : DeviceId
   property DeviceId: Integer dispid 300;
    // LineId : LineId
   property LineId: Integer dispid 301;
    // MMHandle : MMHandle
   property MMHandle: Integer  readonly dispid 302;
  end;


// ISpeechVoice : ISpeechVoice Interface

 ISpeechVoice = interface(IDispatch)
   ['{269316D8-57BD-11D2-9EEE-00C04F797396}']
   function Get_Status : ISpeechVoiceStatus; safecall;
   function Get_Voice : ISpeechObjectToken; safecall;
   procedure Set_Voice(const Voice:ISpeechObjectToken); safecall;
   function Get_AudioOutput : ISpeechObjectToken; safecall;
   procedure Set_AudioOutput(const AudioOutput:ISpeechObjectToken); safecall;
   function Get_AudioOutputStream : ISpeechBaseStream; safecall;
   procedure Set_AudioOutputStream(const AudioOutputStream:ISpeechBaseStream); safecall;
   function Get_Rate : Integer; safecall;
   procedure Set_Rate(const Rate:Integer); safecall;
   function Get_Volume : Integer; safecall;
   procedure Set_Volume(const Volume:Integer); safecall;
   procedure Set_AllowAudioOutputFormatChangesOnNextSet(const Allow:WordBool); safecall;
   function Get_AllowAudioOutputFormatChangesOnNextSet : WordBool; safecall;
   function Get_EventInterests : SpeechVoiceEvents; safecall;
   procedure Set_EventInterests(const EventInterestFlags:SpeechVoiceEvents); safecall;
   procedure Set_Priority(const Priority:SpeechVoicePriority); safecall;
   function Get_Priority : SpeechVoicePriority; safecall;
   procedure Set_AlertBoundary(const Boundary:SpeechVoiceEvents); safecall;
   function Get_AlertBoundary : SpeechVoiceEvents; safecall;
   procedure Set_SynchronousSpeakTimeout(const msTimeout:Integer); safecall;
   function Get_SynchronousSpeakTimeout : Integer; safecall;
    // Speak : Speak
   function Speak(Text_:WideString;Flags:SpeechVoiceSpeakFlags): Integer;safecall;
    // SpeakStream : SpeakStream
   function SpeakStream(Stream:ISpeechBaseStream;Flags:SpeechVoiceSpeakFlags): Integer;safecall;
    // Pause : Pauses the voices rendering.
   procedure Pause;safecall;
    // Resume : Resumes the voices rendering.
   procedure Resume;safecall;
    // Skip : Skips rendering the specified number of items.
   function Skip(Type_:WideString;NumItems:Integer): Integer;safecall;
    // GetVoices : GetVoices
   function GetVoices(RequiredAttributes:WideString;OptionalAttributes:WideString): ISpeechObjectTokens;safecall;
    // GetAudioOutputs : GetAudioOutputs
   function GetAudioOutputs(RequiredAttributes:WideString;OptionalAttributes:WideString): ISpeechObjectTokens;safecall;
    // WaitUntilDone : WaitUntilDone
   function WaitUntilDone(msTimeout:Integer): WordBool;safecall;
    // SpeakCompleteEvent : SpeakCompleteEvent
   function SpeakCompleteEvent: Integer;safecall;
    // IsUISupported : IsUISupported
   function IsUISupported(TypeOfUI:WideString;var ExtraData:OleVariant): WordBool;safecall;
    // DisplayUI : DisplayUI
   procedure DisplayUI(hWndParent:Integer;Title:WideString;TypeOfUI:WideString;var ExtraData:OleVariant);safecall;
    // Status : Status
   property Status: ISpeechVoiceStatus read Get_Status;
    // Voice : Voice
   property Voice: ISpeechObjectToken read Get_Voice write Set_Voice;
    // AudioOutput : Gets the audio output object
   property AudioOutput: ISpeechObjectToken read Get_AudioOutput write Set_AudioOutput;
    // AudioOutputStream : Gets the audio output stream
   property AudioOutputStream: ISpeechBaseStream read Get_AudioOutputStream write Set_AudioOutputStream;
    // Rate : Rate
   property Rate: Integer read Get_Rate write Set_Rate;
    // Volume : Volume
   property Volume: Integer read Get_Volume write Set_Volume;
    // AllowAudioOutputFormatChangesOnNextSet : AllowAudioOutputFormatChangesOnNextSet
   property AllowAudioOutputFormatChangesOnNextSet: WordBool read Get_AllowAudioOutputFormatChangesOnNextSet write Set_AllowAudioOutputFormatChangesOnNextSet;
    // EventInterests : EventInterests
   property EventInterests: SpeechVoiceEvents read Get_EventInterests write Set_EventInterests;
    // Priority : Priority
   property Priority: SpeechVoicePriority read Get_Priority write Set_Priority;
    // AlertBoundary : AlertBoundary
   property AlertBoundary: SpeechVoiceEvents read Get_AlertBoundary write Set_AlertBoundary;
    // SynchronousSpeakTimeout : SyncSpeakTimeout
   property SynchronousSpeakTimeout: Integer read Get_SynchronousSpeakTimeout write Set_SynchronousSpeakTimeout;
  end;


// ISpeechVoice : ISpeechVoice Interface

 ISpeechVoiceDisp = dispinterface
   ['{269316D8-57BD-11D2-9EEE-00C04F797396}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Speak : Speak
   function Speak(Text_:WideString;Flags:SpeechVoiceSpeakFlags): Integer;dispid 12;
    // SpeakStream : SpeakStream
   function SpeakStream(Stream:ISpeechBaseStream;Flags:SpeechVoiceSpeakFlags): Integer;dispid 13;
    // Pause : Pauses the voices rendering.
   procedure Pause;dispid 14;
    // Resume : Resumes the voices rendering.
   procedure Resume;dispid 15;
    // Skip : Skips rendering the specified number of items.
   function Skip(Type_:WideString;NumItems:Integer): Integer;dispid 16;
    // GetVoices : GetVoices
   function GetVoices(RequiredAttributes:WideString;OptionalAttributes:WideString): ISpeechObjectTokens;dispid 17;
    // GetAudioOutputs : GetAudioOutputs
   function GetAudioOutputs(RequiredAttributes:WideString;OptionalAttributes:WideString): ISpeechObjectTokens;dispid 18;
    // WaitUntilDone : WaitUntilDone
   function WaitUntilDone(msTimeout:Integer): WordBool;dispid 19;
    // SpeakCompleteEvent : SpeakCompleteEvent
   function SpeakCompleteEvent: Integer;dispid 20;
    // IsUISupported : IsUISupported
   function IsUISupported(TypeOfUI:WideString;var ExtraData:OleVariant): WordBool;dispid 21;
    // DisplayUI : DisplayUI
   procedure DisplayUI(hWndParent:Integer;Title:WideString;TypeOfUI:WideString;var ExtraData:OleVariant);dispid 22;
    // Status : Status
   property Status: ISpeechVoiceStatus  readonly dispid 1;
    // Voice : Voice
   property Voice: ISpeechObjectToken dispid 2;
    // AudioOutput : Gets the audio output object
   property AudioOutput: ISpeechObjectToken dispid 3;
    // AudioOutputStream : Gets the audio output stream
   property AudioOutputStream: ISpeechBaseStream dispid 4;
    // Rate : Rate
   property Rate: Integer dispid 5;
    // Volume : Volume
   property Volume: Integer dispid 6;
    // AllowAudioOutputFormatChangesOnNextSet : AllowAudioOutputFormatChangesOnNextSet
   property AllowAudioOutputFormatChangesOnNextSet: WordBool dispid 7;
    // EventInterests : EventInterests
   property EventInterests: SpeechVoiceEvents dispid 8;
    // Priority : Priority
   property Priority: SpeechVoicePriority dispid 9;
    // AlertBoundary : AlertBoundary
   property AlertBoundary: SpeechVoiceEvents dispid 10;
    // SynchronousSpeakTimeout : SyncSpeakTimeout
   property SynchronousSpeakTimeout: Integer dispid 11;
  end;


// ISpeechVoiceStatus : ISpeechVoiceStatus Interface

 ISpeechVoiceStatus = interface(IDispatch)
   ['{8BE47B07-57F6-11D2-9EEE-00C04F797396}']
   function Get_CurrentStreamNumber : Integer; safecall;
   function Get_LastStreamNumberQueued : Integer; safecall;
   function Get_LastHResult : Integer; safecall;
   function Get_RunningState : SpeechRunState; safecall;
   function Get_InputWordPosition : Integer; safecall;
   function Get_InputWordLength : Integer; safecall;
   function Get_InputSentencePosition : Integer; safecall;
   function Get_InputSentenceLength : Integer; safecall;
   function Get_LastBookmark : WideString; safecall;
   function Get_LastBookmarkId : Integer; safecall;
   function Get_PhonemeId : Smallint; safecall;
   function Get_VisemeId : Smallint; safecall;
    // CurrentStreamNumber : CurrentStreamNumber
   property CurrentStreamNumber: Integer read Get_CurrentStreamNumber;
    // LastStreamNumberQueued : LastStreamNumberQueued
   property LastStreamNumberQueued: Integer read Get_LastStreamNumberQueued;
    // LastHResult : LastHResult
   property LastHResult: Integer read Get_LastHResult;
    // RunningState : RunningState
   property RunningState: SpeechRunState read Get_RunningState;
    // InputWordPosition : InputWordPosition
   property InputWordPosition: Integer read Get_InputWordPosition;
    // InputWordLength : InputWordLength
   property InputWordLength: Integer read Get_InputWordLength;
    // InputSentencePosition : InputSentencePosition
   property InputSentencePosition: Integer read Get_InputSentencePosition;
    // InputSentenceLength : InputSentenceLength
   property InputSentenceLength: Integer read Get_InputSentenceLength;
    // LastBookmark : LastBookmark
   property LastBookmark: WideString read Get_LastBookmark;
    // LastBookmarkId : LastBookmarkId
   property LastBookmarkId: Integer read Get_LastBookmarkId;
    // PhonemeId : PhonemeId
   property PhonemeId: Smallint read Get_PhonemeId;
    // VisemeId : VisemeId
   property VisemeId: Smallint read Get_VisemeId;
  end;


// ISpeechVoiceStatus : ISpeechVoiceStatus Interface

 ISpeechVoiceStatusDisp = dispinterface
   ['{8BE47B07-57F6-11D2-9EEE-00C04F797396}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // CurrentStreamNumber : CurrentStreamNumber
   property CurrentStreamNumber: Integer  readonly dispid 1;
    // LastStreamNumberQueued : LastStreamNumberQueued
   property LastStreamNumberQueued: Integer  readonly dispid 2;
    // LastHResult : LastHResult
   property LastHResult: Integer  readonly dispid 3;
    // RunningState : RunningState
   property RunningState: SpeechRunState  readonly dispid 4;
    // InputWordPosition : InputWordPosition
   property InputWordPosition: Integer  readonly dispid 5;
    // InputWordLength : InputWordLength
   property InputWordLength: Integer  readonly dispid 6;
    // InputSentencePosition : InputSentencePosition
   property InputSentencePosition: Integer  readonly dispid 7;
    // InputSentenceLength : InputSentenceLength
   property InputSentenceLength: Integer  readonly dispid 8;
    // LastBookmark : LastBookmark
   property LastBookmark: WideString  readonly dispid 9;
    // LastBookmarkId : LastBookmarkId
   property LastBookmarkId: Integer  readonly dispid 10;
    // PhonemeId : PhonemeId
   property PhonemeId: Smallint  readonly dispid 11;
    // VisemeId : VisemeId
   property VisemeId: Smallint  readonly dispid 12;
  end;


// _ISpeechVoiceEvents :

 _ISpeechVoiceEvents = dispinterface
   ['{A372ACD1-3BEF-4BBD-8FFB-CB3E2B416AF8}']
    // StartStream : StartStream
   procedure StartStream(StreamNumber:Integer;StreamPosition:OleVariant);dispid 1;
    // EndStream : EndStream
   procedure EndStream(StreamNumber:Integer;StreamPosition:OleVariant);dispid 2;
    // VoiceChange : VoiceChange
   procedure VoiceChange(StreamNumber:Integer;StreamPosition:OleVariant;VoiceObjectToken:ISpeechObjectToken);dispid 3;
    // Bookmark : Bookmark
   procedure Bookmark(StreamNumber:Integer;StreamPosition:OleVariant;Bookmark:WideString;BookmarkId:Integer);dispid 4;
    // Word : Word
   procedure Word(StreamNumber:Integer;StreamPosition:OleVariant;CharacterPosition:Integer;Length:Integer);dispid 5;
    // Sentence : Sentence
   procedure Sentence(StreamNumber:Integer;StreamPosition:OleVariant;CharacterPosition:Integer;Length:Integer);dispid 7;
    // Phoneme : Phoneme
   procedure Phoneme(StreamNumber:Integer;StreamPosition:OleVariant;Duration:Integer;NextPhoneId:Smallint;Feature:SpeechVisemeFeature;CurrentPhoneId:Smallint);dispid 6;
    // Viseme : Viseme
   procedure Viseme(StreamNumber:Integer;StreamPosition:OleVariant;Duration:Integer;NextVisemeId:SpeechVisemeType;Feature:SpeechVisemeFeature;CurrentVisemeId:SpeechVisemeType);dispid 8;
    // AudioLevel : AudioLevel
   procedure AudioLevel(StreamNumber:Integer;StreamPosition:OleVariant;AudioLevel:Integer);dispid 9;
    // EnginePrivate : EnginePrivate
   procedure EnginePrivate(StreamNumber:Integer;StreamPosition:Integer;EngineData:OleVariant);dispid 10;
  end;


// ISpeechRecognizer : ISpeechRecognizer Interface

 ISpeechRecognizer = interface(IDispatch)
   ['{2D5F1C0C-BD75-4B08-9478-3B11FEA2586C}']
   procedure Set_Recognizer(const Recognizer:ISpeechObjectToken); safecall;
   function Get_Recognizer : ISpeechObjectToken; safecall;
   procedure Set_AllowAudioInputFormatChangesOnNextSet(const Allow:WordBool); safecall;
   function Get_AllowAudioInputFormatChangesOnNextSet : WordBool; safecall;
   procedure Set_AudioInput(const AudioInput:ISpeechObjectToken); safecall;
   function Get_AudioInput : ISpeechObjectToken; safecall;
   procedure Set_AudioInputStream(const AudioInputStream:ISpeechBaseStream); safecall;
   function Get_AudioInputStream : ISpeechBaseStream; safecall;
   function Get_IsShared : WordBool; safecall;
   procedure Set_State(const State:SpeechRecognizerState); safecall;
   function Get_State : SpeechRecognizerState; safecall;
   function Get_Status : ISpeechRecognizerStatus; safecall;
   procedure Set_Profile(const Profile:ISpeechObjectToken); safecall;
   function Get_Profile : ISpeechObjectToken; safecall;
    // EmulateRecognition : EmulateRecognition
   procedure EmulateRecognition(TextElements:OleVariant;var ElementDisplayAttributes:OleVariant;LanguageId:Integer);safecall;
    // CreateRecoContext : CreateRecoContext
   function CreateRecoContext: ISpeechRecoContext;safecall;
    // GetFormat : GetFormat
   function GetFormat(Type_:SpeechFormatType): ISpeechAudioFormat;safecall;
    // SetPropertyNumber : SetPropertyNumber
   function SetPropertyNumber(Name:WideString;Value:Integer): WordBool;safecall;
    // GetPropertyNumber : GetPropertyNumber
   function GetPropertyNumber(Name:WideString;var Value:Integer): WordBool;safecall;
    // SetPropertyString : SetPropertyString
   function SetPropertyString(Name:WideString;Value:WideString): WordBool;safecall;
    // GetPropertyString : GetPropertyString
   function GetPropertyString(Name:WideString;var Value:WideString): WordBool;safecall;
    // IsUISupported : IsUISupported
   function IsUISupported(TypeOfUI:WideString;var ExtraData:OleVariant): WordBool;safecall;
    // DisplayUI : DisplayUI
   procedure DisplayUI(hWndParent:Integer;Title:WideString;TypeOfUI:WideString;var ExtraData:OleVariant);safecall;
    // GetRecognizers : GetRecognizers
   function GetRecognizers(RequiredAttributes:WideString;OptionalAttributes:WideString): ISpeechObjectTokens;safecall;
    // GetAudioInputs : GetAudioInputs
   function GetAudioInputs(RequiredAttributes:WideString;OptionalAttributes:WideString): ISpeechObjectTokens;safecall;
    // GetProfiles : GetProfiles
   function GetProfiles(RequiredAttributes:WideString;OptionalAttributes:WideString): ISpeechObjectTokens;safecall;
    // Recognizer : Recognizer
   property Recognizer: ISpeechObjectToken read Get_Recognizer write Set_Recognizer;
    // AllowAudioInputFormatChangesOnNextSet : AllowAudioInputFormatChangesOnNextSet
   property AllowAudioInputFormatChangesOnNextSet: WordBool read Get_AllowAudioInputFormatChangesOnNextSet write Set_AllowAudioInputFormatChangesOnNextSet;
    // AudioInput : AudioInput
   property AudioInput: ISpeechObjectToken read Get_AudioInput write Set_AudioInput;
    // AudioInputStream : AudioInputStream
   property AudioInputStream: ISpeechBaseStream read Get_AudioInputStream write Set_AudioInputStream;
    // IsShared : IsShared
   property IsShared: WordBool read Get_IsShared;
    // State : State
   property State: SpeechRecognizerState read Get_State write Set_State;
    // Status : Status
   property Status: ISpeechRecognizerStatus read Get_Status;
    // Profile : Profile
   property Profile: ISpeechObjectToken read Get_Profile write Set_Profile;
  end;


// ISpeechRecognizer : ISpeechRecognizer Interface

 ISpeechRecognizerDisp = dispinterface
   ['{2D5F1C0C-BD75-4B08-9478-3B11FEA2586C}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // EmulateRecognition : EmulateRecognition
   procedure EmulateRecognition(TextElements:OleVariant;var ElementDisplayAttributes:OleVariant;LanguageId:Integer);dispid 9;
    // CreateRecoContext : CreateRecoContext
   function CreateRecoContext: ISpeechRecoContext;dispid 10;
    // GetFormat : GetFormat
   function GetFormat(Type_:SpeechFormatType): ISpeechAudioFormat;dispid 11;
    // SetPropertyNumber : SetPropertyNumber
   function SetPropertyNumber(Name:WideString;Value:Integer): WordBool;dispid 12;
    // GetPropertyNumber : GetPropertyNumber
   function GetPropertyNumber(Name:WideString;var Value:Integer): WordBool;dispid 13;
    // SetPropertyString : SetPropertyString
   function SetPropertyString(Name:WideString;Value:WideString): WordBool;dispid 14;
    // GetPropertyString : GetPropertyString
   function GetPropertyString(Name:WideString;var Value:WideString): WordBool;dispid 15;
    // IsUISupported : IsUISupported
   function IsUISupported(TypeOfUI:WideString;var ExtraData:OleVariant): WordBool;dispid 16;
    // DisplayUI : DisplayUI
   procedure DisplayUI(hWndParent:Integer;Title:WideString;TypeOfUI:WideString;var ExtraData:OleVariant);dispid 17;
    // GetRecognizers : GetRecognizers
   function GetRecognizers(RequiredAttributes:WideString;OptionalAttributes:WideString): ISpeechObjectTokens;dispid 18;
    // GetAudioInputs : GetAudioInputs
   function GetAudioInputs(RequiredAttributes:WideString;OptionalAttributes:WideString): ISpeechObjectTokens;dispid 19;
    // GetProfiles : GetProfiles
   function GetProfiles(RequiredAttributes:WideString;OptionalAttributes:WideString): ISpeechObjectTokens;dispid 20;
    // Recognizer : Recognizer
   property Recognizer: ISpeechObjectToken dispid 1;
    // AllowAudioInputFormatChangesOnNextSet : AllowAudioInputFormatChangesOnNextSet
   property AllowAudioInputFormatChangesOnNextSet: WordBool dispid 2;
    // AudioInput : AudioInput
   property AudioInput: ISpeechObjectToken dispid 3;
    // AudioInputStream : AudioInputStream
   property AudioInputStream: ISpeechBaseStream dispid 4;
    // IsShared : IsShared
   property IsShared: WordBool  readonly dispid 5;
    // State : State
   property State: SpeechRecognizerState dispid 6;
    // Status : Status
   property Status: ISpeechRecognizerStatus  readonly dispid 7;
    // Profile : Profile
   property Profile: ISpeechObjectToken dispid 8;
  end;


// ISpeechRecognizerStatus : ISpeechRecognizerStatus Interface

 ISpeechRecognizerStatus = interface(IDispatch)
   ['{BFF9E781-53EC-484E-BB8A-0E1B5551E35C}']
   function Get_AudioStatus : ISpeechAudioStatus; safecall;
   function Get_CurrentStreamPosition : OleVariant; safecall;
   function Get_CurrentStreamNumber : Integer; safecall;
   function Get_NumberOfActiveRules : Integer; safecall;
   function Get_ClsidEngine : WideString; safecall;
   function Get_SupportedLanguages : OleVariant; safecall;
    // AudioStatus : AudioStatus
   property AudioStatus: ISpeechAudioStatus read Get_AudioStatus;
    // CurrentStreamPosition : CurrentStreamPosition
   property CurrentStreamPosition: OleVariant read Get_CurrentStreamPosition;
    // CurrentStreamNumber : CurrentStreamNumber
   property CurrentStreamNumber: Integer read Get_CurrentStreamNumber;
    // NumberOfActiveRules : NumberOfActiveRules
   property NumberOfActiveRules: Integer read Get_NumberOfActiveRules;
    // ClsidEngine : ClsidEngine
   property ClsidEngine: WideString read Get_ClsidEngine;
    // SupportedLanguages : SupportedLanguages
   property SupportedLanguages: OleVariant read Get_SupportedLanguages;
  end;


// ISpeechRecognizerStatus : ISpeechRecognizerStatus Interface

 ISpeechRecognizerStatusDisp = dispinterface
   ['{BFF9E781-53EC-484E-BB8A-0E1B5551E35C}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // AudioStatus : AudioStatus
   property AudioStatus: ISpeechAudioStatus  readonly dispid 1;
    // CurrentStreamPosition : CurrentStreamPosition
   property CurrentStreamPosition: OleVariant  readonly dispid 2;
    // CurrentStreamNumber : CurrentStreamNumber
   property CurrentStreamNumber: Integer  readonly dispid 3;
    // NumberOfActiveRules : NumberOfActiveRules
   property NumberOfActiveRules: Integer  readonly dispid 4;
    // ClsidEngine : ClsidEngine
   property ClsidEngine: WideString  readonly dispid 5;
    // SupportedLanguages : SupportedLanguages
   property SupportedLanguages: OleVariant  readonly dispid 6;
  end;


// ISpeechRecoContext : ISpeechRecoContext Interface

 ISpeechRecoContext = interface(IDispatch)
   ['{580AA49D-7E1E-4809-B8E2-57DA806104B8}']
   function Get_Recognizer : ISpeechRecognizer; safecall;
   function Get_AudioInputInterferenceStatus : SpeechInterference; safecall;
   function Get_RequestedUIType : WideString; safecall;
   procedure Set_Voice(const Voice:ISpeechVoice); safecall;
   function Get_Voice : ISpeechVoice; safecall;
   procedure Set_AllowVoiceFormatMatchingOnNextSet(const pAllow:WordBool); safecall;
   function Get_AllowVoiceFormatMatchingOnNextSet : WordBool; safecall;
   procedure Set_VoicePurgeEvent(const EventInterest:SpeechRecoEvents); safecall;
   function Get_VoicePurgeEvent : SpeechRecoEvents; safecall;
   procedure Set_EventInterests(const EventInterest:SpeechRecoEvents); safecall;
   function Get_EventInterests : SpeechRecoEvents; safecall;
   procedure Set_CmdMaxAlternates(const MaxAlternates:Integer); safecall;
   function Get_CmdMaxAlternates : Integer; safecall;
   procedure Set_State(const State:SpeechRecoContextState); safecall;
   function Get_State : SpeechRecoContextState; safecall;
   procedure Set_RetainedAudio(const Option:SpeechRetainedAudioOptions); safecall;
   function Get_RetainedAudio : SpeechRetainedAudioOptions; safecall;
   procedure Set_RetainedAudioFormat(const Format:ISpeechAudioFormat); safecall;
   function Get_RetainedAudioFormat : ISpeechAudioFormat; safecall;
    // Pause : Pause
   procedure Pause;safecall;
    // Resume : Resume
   procedure Resume;safecall;
    // CreateGrammar : CreateGrammar
   function CreateGrammar(GrammarId:OleVariant): ISpeechRecoGrammar;safecall;
    // CreateResultFromMemory : CreateResultFromMemory
   function CreateResultFromMemory(var ResultBlock:OleVariant): ISpeechRecoResult;safecall;
    // Bookmark : Bookmark
   procedure Bookmark(Options:SpeechBookmarkOptions;StreamPos:OleVariant;BookmarkId:OleVariant);safecall;
    // SetAdaptationData : SetAdaptationData
   procedure SetAdaptationData(AdaptationString:WideString);safecall;
    // Recognizer : Recognizer
   property Recognizer: ISpeechRecognizer read Get_Recognizer;
    // AudioInputInterferenceStatus : AudioInInterferenceStatus
   property AudioInputInterferenceStatus: SpeechInterference read Get_AudioInputInterferenceStatus;
    // RequestedUIType : RequestedUIType
   property RequestedUIType: WideString read Get_RequestedUIType;
    // Voice : Voice
   property Voice: ISpeechVoice read Get_Voice write Set_Voice;
    // AllowVoiceFormatMatchingOnNextSet : AllowVoiceFormatMatchingOnNextSet
   property AllowVoiceFormatMatchingOnNextSet: WordBool read Get_AllowVoiceFormatMatchingOnNextSet write Set_AllowVoiceFormatMatchingOnNextSet;
    // VoicePurgeEvent : VoicePurgeEvent
   property VoicePurgeEvent: SpeechRecoEvents read Get_VoicePurgeEvent write Set_VoicePurgeEvent;
    // EventInterests : EventInterests
   property EventInterests: SpeechRecoEvents read Get_EventInterests write Set_EventInterests;
    // CmdMaxAlternates : CmdMaxAlternates
   property CmdMaxAlternates: Integer read Get_CmdMaxAlternates write Set_CmdMaxAlternates;
    // State : State
   property State: SpeechRecoContextState read Get_State write Set_State;
    // RetainedAudio : RetainedAudio
   property RetainedAudio: SpeechRetainedAudioOptions read Get_RetainedAudio write Set_RetainedAudio;
    // RetainedAudioFormat : RetainedAudioFormat
   property RetainedAudioFormat: ISpeechAudioFormat read Get_RetainedAudioFormat write Set_RetainedAudioFormat;
  end;


// ISpeechRecoContext : ISpeechRecoContext Interface

 ISpeechRecoContextDisp = dispinterface
   ['{580AA49D-7E1E-4809-B8E2-57DA806104B8}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Pause : Pause
   procedure Pause;dispid 12;
    // Resume : Resume
   procedure Resume;dispid 13;
    // CreateGrammar : CreateGrammar
   function CreateGrammar(GrammarId:OleVariant): ISpeechRecoGrammar;dispid 14;
    // CreateResultFromMemory : CreateResultFromMemory
   function CreateResultFromMemory(var ResultBlock:OleVariant): ISpeechRecoResult;dispid 15;
    // Bookmark : Bookmark
   procedure Bookmark(Options:SpeechBookmarkOptions;StreamPos:OleVariant;BookmarkId:OleVariant);dispid 16;
    // SetAdaptationData : SetAdaptationData
   procedure SetAdaptationData(AdaptationString:WideString);dispid 17;
    // Recognizer : Recognizer
   property Recognizer: ISpeechRecognizer  readonly dispid 1;
    // AudioInputInterferenceStatus : AudioInInterferenceStatus
   property AudioInputInterferenceStatus: SpeechInterference  readonly dispid 2;
    // RequestedUIType : RequestedUIType
   property RequestedUIType: WideString  readonly dispid 3;
    // Voice : Voice
   property Voice: ISpeechVoice dispid 4;
    // AllowVoiceFormatMatchingOnNextSet : AllowVoiceFormatMatchingOnNextSet
   property AllowVoiceFormatMatchingOnNextSet: WordBool dispid 5;
    // VoicePurgeEvent : VoicePurgeEvent
   property VoicePurgeEvent: SpeechRecoEvents dispid 6;
    // EventInterests : EventInterests
   property EventInterests: SpeechRecoEvents dispid 7;
    // CmdMaxAlternates : CmdMaxAlternates
   property CmdMaxAlternates: Integer dispid 8;
    // State : State
   property State: SpeechRecoContextState dispid 9;
    // RetainedAudio : RetainedAudio
   property RetainedAudio: SpeechRetainedAudioOptions dispid 10;
    // RetainedAudioFormat : RetainedAudioFormat
   property RetainedAudioFormat: ISpeechAudioFormat dispid 11;
  end;


// ISpeechRecoGrammar : ISpeechRecoGrammar Interface

 ISpeechRecoGrammar = interface(IDispatch)
   ['{B6D6F79F-2158-4E50-B5BC-9A9CCD852A09}']
   function Get_Id : OleVariant; safecall;
   function Get_RecoContext : ISpeechRecoContext; safecall;
   procedure Set_State(const State:SpeechGrammarState); safecall;
   function Get_State : SpeechGrammarState; safecall;
   function Get_Rules : ISpeechGrammarRules; safecall;
    // Reset_ : Reset
   procedure Reset_(NewLanguage:Integer);safecall;
    // CmdLoadFromFile : CmdLoadFromFile
   procedure CmdLoadFromFile(FileName:WideString;LoadOption:SpeechLoadOption);safecall;
    // CmdLoadFromObject : CmdLoadFromObject
   procedure CmdLoadFromObject(ClassId:WideString;GrammarName:WideString;LoadOption:SpeechLoadOption);safecall;
    // CmdLoadFromResource : CmdLoadFromResource
   procedure CmdLoadFromResource(hModule:Integer;ResourceName:OleVariant;ResourceType:OleVariant;LanguageId:Integer;LoadOption:SpeechLoadOption);safecall;
    // CmdLoadFromMemory : CmdLoadFromMemory
   procedure CmdLoadFromMemory(GrammarData:OleVariant;LoadOption:SpeechLoadOption);safecall;
    // CmdLoadFromProprietaryGrammar : CmdLoadFromProprietaryGrammar
   procedure CmdLoadFromProprietaryGrammar(ProprietaryGuid:WideString;ProprietaryString:WideString;ProprietaryData:OleVariant;LoadOption:SpeechLoadOption);safecall;
    // CmdSetRuleState : CmdSetRuleState
   procedure CmdSetRuleState(Name:WideString;State:SpeechRuleState);safecall;
    // CmdSetRuleIdState : CmdSetRuleIdState
   procedure CmdSetRuleIdState(RuleId:Integer;State:SpeechRuleState);safecall;
    // DictationLoad : DictationLoad
   procedure DictationLoad(TopicName:WideString;LoadOption:SpeechLoadOption);safecall;
    // DictationUnload : DictationUnload
   procedure DictationUnload;safecall;
    // DictationSetState : DictationSetState
   procedure DictationSetState(State:SpeechRuleState);safecall;
    // SetWordSequenceData : SetWordSequenceData
   procedure SetWordSequenceData(Text_:WideString;TextLength:Integer;Info:ISpeechTextSelectionInformation);safecall;
    // SetTextSelection : SetTextSelection
   procedure SetTextSelection(Info:ISpeechTextSelectionInformation);safecall;
    // IsPronounceable : IsPronounceable
   function IsPronounceable(Word:WideString): SpeechWordPronounceable;safecall;
    // Id : Id
   property Id: OleVariant read Get_Id;
    // RecoContext : RecoContext
   property RecoContext: ISpeechRecoContext read Get_RecoContext;
    // State : State
   property State: SpeechGrammarState read Get_State write Set_State;
    // Rules : Rules
   property Rules: ISpeechGrammarRules read Get_Rules;
  end;


// ISpeechRecoGrammar : ISpeechRecoGrammar Interface

 ISpeechRecoGrammarDisp = dispinterface
   ['{B6D6F79F-2158-4E50-B5BC-9A9CCD852A09}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Reset_ : Reset
   procedure Reset_(NewLanguage:Integer);dispid 5;
    // CmdLoadFromFile : CmdLoadFromFile
   procedure CmdLoadFromFile(FileName:WideString;LoadOption:SpeechLoadOption);dispid 7;
    // CmdLoadFromObject : CmdLoadFromObject
   procedure CmdLoadFromObject(ClassId:WideString;GrammarName:WideString;LoadOption:SpeechLoadOption);dispid 8;
    // CmdLoadFromResource : CmdLoadFromResource
   procedure CmdLoadFromResource(hModule:Integer;ResourceName:OleVariant;ResourceType:OleVariant;LanguageId:Integer;LoadOption:SpeechLoadOption);dispid 9;
    // CmdLoadFromMemory : CmdLoadFromMemory
   procedure CmdLoadFromMemory(GrammarData:OleVariant;LoadOption:SpeechLoadOption);dispid 10;
    // CmdLoadFromProprietaryGrammar : CmdLoadFromProprietaryGrammar
   procedure CmdLoadFromProprietaryGrammar(ProprietaryGuid:WideString;ProprietaryString:WideString;ProprietaryData:OleVariant;LoadOption:SpeechLoadOption);dispid 11;
    // CmdSetRuleState : CmdSetRuleState
   procedure CmdSetRuleState(Name:WideString;State:SpeechRuleState);dispid 12;
    // CmdSetRuleIdState : CmdSetRuleIdState
   procedure CmdSetRuleIdState(RuleId:Integer;State:SpeechRuleState);dispid 13;
    // DictationLoad : DictationLoad
   procedure DictationLoad(TopicName:WideString;LoadOption:SpeechLoadOption);dispid 14;
    // DictationUnload : DictationUnload
   procedure DictationUnload;dispid 15;
    // DictationSetState : DictationSetState
   procedure DictationSetState(State:SpeechRuleState);dispid 16;
    // SetWordSequenceData : SetWordSequenceData
   procedure SetWordSequenceData(Text_:WideString;TextLength:Integer;Info:ISpeechTextSelectionInformation);dispid 17;
    // SetTextSelection : SetTextSelection
   procedure SetTextSelection(Info:ISpeechTextSelectionInformation);dispid 18;
    // IsPronounceable : IsPronounceable
   function IsPronounceable(Word:WideString): SpeechWordPronounceable;dispid 19;
    // Id : Id
   property Id: OleVariant  readonly dispid 1;
    // RecoContext : RecoContext
   property RecoContext: ISpeechRecoContext  readonly dispid 2;
    // State : State
   property State: SpeechGrammarState dispid 3;
    // Rules : Rules
   property Rules: ISpeechGrammarRules  readonly dispid 4;
  end;


// ISpeechGrammarRules : ISpeechGrammarRules Interface

 ISpeechGrammarRules = interface(IDispatch)
   ['{6FFA3B44-FC2D-40D1-8AFC-32911C7F1AD1}']
   function Get_Count : Integer; safecall;
    // FindRule : FindRule
   function FindRule(RuleNameOrId:OleVariant): ISpeechGrammarRule;safecall;
    // Item : Item
   function Item(Index:Integer): ISpeechGrammarRule;safecall;
   function Get__NewEnum : IUnknown; safecall;
   function Get_Dynamic : WordBool; safecall;
    // Add : Add
   function Add(RuleName:WideString;Attributes:SpeechRuleAttributes;RuleId:Integer): ISpeechGrammarRule;safecall;
    // Commit : Commit
   procedure Commit;safecall;
    // CommitAndSave : CommitAndSave
   function CommitAndSave(out ErrorText:WideString): OleVariant;safecall;
    // Count : Count
   property Count: Integer read Get_Count;
    // _NewEnum : Enumerates the alternates
   property _NewEnum: IUnknown read Get__NewEnum;
    // Dynamic : Dynamic
   property Dynamic: WordBool read Get_Dynamic;
  end;


// ISpeechGrammarRules : ISpeechGrammarRules Interface

 ISpeechGrammarRulesDisp = dispinterface
   ['{6FFA3B44-FC2D-40D1-8AFC-32911C7F1AD1}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // FindRule : FindRule
   function FindRule(RuleNameOrId:OleVariant): ISpeechGrammarRule;dispid 6;
    // Item : Item
   function Item(Index:Integer): ISpeechGrammarRule;dispid 0;
    // Add : Add
   function Add(RuleName:WideString;Attributes:SpeechRuleAttributes;RuleId:Integer): ISpeechGrammarRule;dispid 3;
    // Commit : Commit
   procedure Commit;dispid 4;
    // CommitAndSave : CommitAndSave
   function CommitAndSave(out ErrorText:WideString): OleVariant;dispid 5;
    // Count : Count
   property Count: Integer  readonly dispid 1;
    // _NewEnum : Enumerates the alternates
   property _NewEnum: IUnknown  readonly dispid -4;
    // Dynamic : Dynamic
   property Dynamic: WordBool  readonly dispid 2;
  end;


// ISpeechGrammarRule : ISpeechGrammarRule Interface

 ISpeechGrammarRule = interface(IDispatch)
   ['{AFE719CF-5DD1-44F2-999C-7A399F1CFCCC}']
   function Get_Attributes : SpeechRuleAttributes; safecall;
   function Get_InitialState : ISpeechGrammarRuleState; safecall;
   function Get_Name : WideString; safecall;
   function Get_Id : Integer; safecall;
    // Clear : Clear
   procedure Clear;safecall;
    // AddResource : AddResource
   procedure AddResource(ResourceName:WideString;ResourceValue:WideString);safecall;
    // AddState : AddState
   function AddState: ISpeechGrammarRuleState;safecall;
    // Attributes : RuleAttributes
   property Attributes: SpeechRuleAttributes read Get_Attributes;
    // InitialState : InitialState
   property InitialState: ISpeechGrammarRuleState read Get_InitialState;
    // Name : Name
   property Name: WideString read Get_Name;
    // Id : Id
   property Id: Integer read Get_Id;
  end;


// ISpeechGrammarRule : ISpeechGrammarRule Interface

 ISpeechGrammarRuleDisp = dispinterface
   ['{AFE719CF-5DD1-44F2-999C-7A399F1CFCCC}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Clear : Clear
   procedure Clear;dispid 5;
    // AddResource : AddResource
   procedure AddResource(ResourceName:WideString;ResourceValue:WideString);dispid 6;
    // AddState : AddState
   function AddState: ISpeechGrammarRuleState;dispid 7;
    // Attributes : RuleAttributes
   property Attributes: SpeechRuleAttributes  readonly dispid 1;
    // InitialState : InitialState
   property InitialState: ISpeechGrammarRuleState  readonly dispid 2;
    // Name : Name
   property Name: WideString  readonly dispid 3;
    // Id : Id
   property Id: Integer  readonly dispid 4;
  end;


// ISpeechGrammarRuleState : ISpeechGrammarRuleState Interface

 ISpeechGrammarRuleState = interface(IDispatch)
   ['{D4286F2C-EE67-45AE-B928-28D695362EDA}']
   function Get_Rule : ISpeechGrammarRule; safecall;
   function Get_Transitions : ISpeechGrammarRuleStateTransitions; safecall;
    // AddWordTransition : AddWordTransition
   procedure AddWordTransition(DestState:ISpeechGrammarRuleState;Words:WideString;Separators:WideString;Type_:SpeechGrammarWordType;PropertyName:WideString;PropertyId:Integer;var PropertyValue:OleVariant;Weight:Single);safecall;
    // AddRuleTransition : AddRuleTransition
   procedure AddRuleTransition(DestinationState:ISpeechGrammarRuleState;Rule:ISpeechGrammarRule;PropertyName:WideString;PropertyId:Integer;var PropertyValue:OleVariant;Weight:Single);safecall;
    // AddSpecialTransition : AddSpecialTransition
   procedure AddSpecialTransition(DestinationState:ISpeechGrammarRuleState;Type_:SpeechSpecialTransitionType;PropertyName:WideString;PropertyId:Integer;var PropertyValue:OleVariant;Weight:Single);safecall;
    // Rule : Rule
   property Rule: ISpeechGrammarRule read Get_Rule;
    // Transitions : Transitions
   property Transitions: ISpeechGrammarRuleStateTransitions read Get_Transitions;
  end;


// ISpeechGrammarRuleState : ISpeechGrammarRuleState Interface

 ISpeechGrammarRuleStateDisp = dispinterface
   ['{D4286F2C-EE67-45AE-B928-28D695362EDA}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // AddWordTransition : AddWordTransition
   procedure AddWordTransition(DestState:ISpeechGrammarRuleState;Words:WideString;Separators:WideString;Type_:SpeechGrammarWordType;PropertyName:WideString;PropertyId:Integer;var PropertyValue:OleVariant;Weight:Single);dispid 3;
    // AddRuleTransition : AddRuleTransition
   procedure AddRuleTransition(DestinationState:ISpeechGrammarRuleState;Rule:ISpeechGrammarRule;PropertyName:WideString;PropertyId:Integer;var PropertyValue:OleVariant;Weight:Single);dispid 4;
    // AddSpecialTransition : AddSpecialTransition
   procedure AddSpecialTransition(DestinationState:ISpeechGrammarRuleState;Type_:SpeechSpecialTransitionType;PropertyName:WideString;PropertyId:Integer;var PropertyValue:OleVariant;Weight:Single);dispid 5;
    // Rule : Rule
   property Rule: ISpeechGrammarRule  readonly dispid 1;
    // Transitions : Transitions
   property Transitions: ISpeechGrammarRuleStateTransitions  readonly dispid 2;
  end;


// ISpeechGrammarRuleStateTransitions : ISpeechGrammarRuleStateTransitions Interface

 ISpeechGrammarRuleStateTransitions = interface(IDispatch)
   ['{EABCE657-75BC-44A2-AA7F-C56476742963}']
   function Get_Count : Integer; safecall;
    // Item : Item
   function Item(Index:Integer): ISpeechGrammarRuleStateTransition;safecall;
   function Get__NewEnum : IUnknown; safecall;
    // Count : Count
   property Count: Integer read Get_Count;
    // _NewEnum : Enumerates the transitions
   property _NewEnum: IUnknown read Get__NewEnum;
  end;


// ISpeechGrammarRuleStateTransitions : ISpeechGrammarRuleStateTransitions Interface

 ISpeechGrammarRuleStateTransitionsDisp = dispinterface
   ['{EABCE657-75BC-44A2-AA7F-C56476742963}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Item : Item
   function Item(Index:Integer): ISpeechGrammarRuleStateTransition;dispid 0;
    // Count : Count
   property Count: Integer  readonly dispid 1;
    // _NewEnum : Enumerates the transitions
   property _NewEnum: IUnknown  readonly dispid -4;
  end;


// ISpeechGrammarRuleStateTransition : ISpeechGrammarRuleStateTransition Interface

 ISpeechGrammarRuleStateTransition = interface(IDispatch)
   ['{CAFD1DB1-41D1-4A06-9863-E2E81DA17A9A}']
   function Get_Type_ : SpeechGrammarRuleStateTransitionType; safecall;
   function Get_Text_ : WideString; safecall;
   function Get_Rule : ISpeechGrammarRule; safecall;
   function Get_Weight : OleVariant; safecall;
   function Get_PropertyName : WideString; safecall;
   function Get_PropertyId : Integer; safecall;
   function Get_PropertyValue : OleVariant; safecall;
   function Get_NextState : ISpeechGrammarRuleState; safecall;
    // Type : Type
   property Type_: SpeechGrammarRuleStateTransitionType read Get_Type_;
    // Text : Text
   property Text_: WideString read Get_Text_;
    // Rule : Rule
   property Rule: ISpeechGrammarRule read Get_Rule;
    // Weight : Weight
   property Weight: OleVariant read Get_Weight;
    // PropertyName : PropertyName
   property PropertyName: WideString read Get_PropertyName;
    // PropertyId : PropertyId
   property PropertyId: Integer read Get_PropertyId;
    // PropertyValue : PropertyValue
   property PropertyValue: OleVariant read Get_PropertyValue;
    // NextState : NextState
   property NextState: ISpeechGrammarRuleState read Get_NextState;
  end;


// ISpeechGrammarRuleStateTransition : ISpeechGrammarRuleStateTransition Interface

 ISpeechGrammarRuleStateTransitionDisp = dispinterface
   ['{CAFD1DB1-41D1-4A06-9863-E2E81DA17A9A}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Type : Type
   property Type_: SpeechGrammarRuleStateTransitionType  readonly dispid 1;
    // Text : Text
   property Text_: WideString  readonly dispid 2;
    // Rule : Rule
   property Rule: ISpeechGrammarRule  readonly dispid 3;
    // Weight : Weight
   property Weight: OleVariant  readonly dispid 4;
    // PropertyName : PropertyName
   property PropertyName: WideString  readonly dispid 5;
    // PropertyId : PropertyId
   property PropertyId: Integer  readonly dispid 6;
    // PropertyValue : PropertyValue
   property PropertyValue: OleVariant  readonly dispid 7;
    // NextState : NextState
   property NextState: ISpeechGrammarRuleState  readonly dispid 8;
  end;


// ISpeechTextSelectionInformation : ISpeechTextSelectionInformation Interface

 ISpeechTextSelectionInformation = interface(IDispatch)
   ['{3B9C7E7A-6EEE-4DED-9092-11657279ADBE}']
   procedure Set_ActiveOffset(const ActiveOffset:Integer); safecall;
   function Get_ActiveOffset : Integer; safecall;
   procedure Set_ActiveLength(const ActiveLength:Integer); safecall;
   function Get_ActiveLength : Integer; safecall;
   procedure Set_SelectionOffset(const SelectionOffset:Integer); safecall;
   function Get_SelectionOffset : Integer; safecall;
   procedure Set_SelectionLength(const SelectionLength:Integer); safecall;
   function Get_SelectionLength : Integer; safecall;
    // ActiveOffset : ActiveOffset
   property ActiveOffset: Integer read Get_ActiveOffset write Set_ActiveOffset;
    // ActiveLength : ActiveLength
   property ActiveLength: Integer read Get_ActiveLength write Set_ActiveLength;
    // SelectionOffset : SelectionOffset
   property SelectionOffset: Integer read Get_SelectionOffset write Set_SelectionOffset;
    // SelectionLength : SelectionLength
   property SelectionLength: Integer read Get_SelectionLength write Set_SelectionLength;
  end;


// ISpeechTextSelectionInformation : ISpeechTextSelectionInformation Interface

 ISpeechTextSelectionInformationDisp = dispinterface
   ['{3B9C7E7A-6EEE-4DED-9092-11657279ADBE}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // ActiveOffset : ActiveOffset
   property ActiveOffset: Integer dispid 1;
    // ActiveLength : ActiveLength
   property ActiveLength: Integer dispid 2;
    // SelectionOffset : SelectionOffset
   property SelectionOffset: Integer dispid 3;
    // SelectionLength : SelectionLength
   property SelectionLength: Integer dispid 4;
  end;


// ISpeechRecoResult : ISpeechRecoResult Interface

 ISpeechRecoResult = interface(IDispatch)
   ['{ED2879CF-CED9-4EE6-A534-DE0191D5468D}']
   function Get_RecoContext : ISpeechRecoContext; safecall;
   function Get_Times : ISpeechRecoResultTimes; safecall;
   procedure Set_AudioFormat(const Format:ISpeechAudioFormat); safecall;
   function Get_AudioFormat : ISpeechAudioFormat; safecall;
   function Get_PhraseInfo : ISpeechPhraseInfo; safecall;
    // Alternates : Alternates
   function Alternates(RequestCount:Integer;StartElement:Integer;Elements:Integer): ISpeechPhraseAlternates;safecall;
    // Audio : Audio
   function Audio(StartElement:Integer;Elements:Integer): ISpeechMemoryStream;safecall;
    // SpeakAudio : SpeakAudio
   function SpeakAudio(StartElement:Integer;Elements:Integer;Flags:SpeechVoiceSpeakFlags): Integer;safecall;
    // SaveToMemory : SaveToMemory
   function SaveToMemory: OleVariant;safecall;
    // DiscardResultInfo : DiscardResultInfo
   procedure DiscardResultInfo(ValueTypes:SpeechDiscardType);safecall;
    // RecoContext : RecoContext
   property RecoContext: ISpeechRecoContext read Get_RecoContext;
    // Times : Times
   property Times: ISpeechRecoResultTimes read Get_Times;
    // AudioFormat : AudioFormat
   property AudioFormat: ISpeechAudioFormat read Get_AudioFormat write Set_AudioFormat;
    // PhraseInfo : PhraseInfo
   property PhraseInfo: ISpeechPhraseInfo read Get_PhraseInfo;
  end;


// ISpeechRecoResult : ISpeechRecoResult Interface

 ISpeechRecoResultDisp = dispinterface
   ['{ED2879CF-CED9-4EE6-A534-DE0191D5468D}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Alternates : Alternates
   function Alternates(RequestCount:Integer;StartElement:Integer;Elements:Integer): ISpeechPhraseAlternates;dispid 5;
    // Audio : Audio
   function Audio(StartElement:Integer;Elements:Integer): ISpeechMemoryStream;dispid 6;
    // SpeakAudio : SpeakAudio
   function SpeakAudio(StartElement:Integer;Elements:Integer;Flags:SpeechVoiceSpeakFlags): Integer;dispid 7;
    // SaveToMemory : SaveToMemory
   function SaveToMemory: OleVariant;dispid 8;
    // DiscardResultInfo : DiscardResultInfo
   procedure DiscardResultInfo(ValueTypes:SpeechDiscardType);dispid 9;
    // RecoContext : RecoContext
   property RecoContext: ISpeechRecoContext  readonly dispid 1;
    // Times : Times
   property Times: ISpeechRecoResultTimes  readonly dispid 2;
    // AudioFormat : AudioFormat
   property AudioFormat: ISpeechAudioFormat dispid 3;
    // PhraseInfo : PhraseInfo
   property PhraseInfo: ISpeechPhraseInfo  readonly dispid 4;
  end;


// ISpeechRecoResultTimes : ISpeechRecoResultTimes Interface

 ISpeechRecoResultTimes = interface(IDispatch)
   ['{62B3B8FB-F6E7-41BE-BDCB-056B1C29EFC0}']
   function Get_StreamTime : OleVariant; safecall;
   function Get_Length : OleVariant; safecall;
   function Get_TickCount : Integer; safecall;
   function Get_OffsetFromStart : OleVariant; safecall;
    // StreamTime : StreamTime
   property StreamTime: OleVariant read Get_StreamTime;
    // Length : Length
   property Length: OleVariant read Get_Length;
    // TickCount : TickCount
   property TickCount: Integer read Get_TickCount;
    // OffsetFromStart : Start
   property OffsetFromStart: OleVariant read Get_OffsetFromStart;
  end;


// ISpeechRecoResultTimes : ISpeechRecoResultTimes Interface

 ISpeechRecoResultTimesDisp = dispinterface
   ['{62B3B8FB-F6E7-41BE-BDCB-056B1C29EFC0}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // StreamTime : StreamTime
   property StreamTime: OleVariant  readonly dispid 1;
    // Length : Length
   property Length: OleVariant  readonly dispid 2;
    // TickCount : TickCount
   property TickCount: Integer  readonly dispid 3;
    // OffsetFromStart : Start
   property OffsetFromStart: OleVariant  readonly dispid 4;
  end;


// ISpeechPhraseInfo : ISpeechPhraseInfo Interface

 ISpeechPhraseInfo = interface(IDispatch)
   ['{961559CF-4E67-4662-8BF0-D93F1FCD61B3}']
   function Get_LanguageId : Integer; safecall;
   function Get_GrammarId : OleVariant; safecall;
   function Get_StartTime : OleVariant; safecall;
   function Get_AudioStreamPosition : OleVariant; safecall;
   function Get_AudioSizeBytes : Integer; safecall;
   function Get_RetainedSizeBytes : Integer; safecall;
   function Get_AudioSizeTime : Integer; safecall;
   function Get_Rule : ISpeechPhraseRule; safecall;
   function Get_Properties : ISpeechPhraseProperties; safecall;
   function Get_Elements : ISpeechPhraseElements; safecall;
   function Get_Replacements : ISpeechPhraseReplacements; safecall;
   function Get_EngineId : WideString; safecall;
   function Get_EnginePrivateData : OleVariant; safecall;
    // SaveToMemory : SaveToMemory
   function SaveToMemory: OleVariant;safecall;
    // GetText : GetText
   function GetText(StartElement:Integer;Elements:Integer;UseReplacements:WordBool): WideString;safecall;
    // GetDisplayAttributes : DisplayAttributes
   function GetDisplayAttributes(StartElement:Integer;Elements:Integer;UseReplacements:WordBool): SpeechDisplayAttributes;safecall;
    // LanguageId : LanguageId
   property LanguageId: Integer read Get_LanguageId;
    // GrammarId : GrammarId
   property GrammarId: OleVariant read Get_GrammarId;
    // StartTime : StartTime
   property StartTime: OleVariant read Get_StartTime;
    // AudioStreamPosition : AudioStreamPosition
   property AudioStreamPosition: OleVariant read Get_AudioStreamPosition;
    // AudioSizeBytes : AudioSizeBytes
   property AudioSizeBytes: Integer read Get_AudioSizeBytes;
    // RetainedSizeBytes : RetainedSizeBytes
   property RetainedSizeBytes: Integer read Get_RetainedSizeBytes;
    // AudioSizeTime : AudioSizeTime
   property AudioSizeTime: Integer read Get_AudioSizeTime;
    // Rule : Rule
   property Rule: ISpeechPhraseRule read Get_Rule;
    // Properties : Properties
   property Properties: ISpeechPhraseProperties read Get_Properties;
    // Elements : Elements
   property Elements: ISpeechPhraseElements read Get_Elements;
    // Replacements : Replacements
   property Replacements: ISpeechPhraseReplacements read Get_Replacements;
    // EngineId : EngineId
   property EngineId: WideString read Get_EngineId;
    // EnginePrivateData : EnginePrivateData
   property EnginePrivateData: OleVariant read Get_EnginePrivateData;
  end;


// ISpeechPhraseInfo : ISpeechPhraseInfo Interface

 ISpeechPhraseInfoDisp = dispinterface
   ['{961559CF-4E67-4662-8BF0-D93F1FCD61B3}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // SaveToMemory : SaveToMemory
   function SaveToMemory: OleVariant;dispid 14;
    // GetText : GetText
   function GetText(StartElement:Integer;Elements:Integer;UseReplacements:WordBool): WideString;dispid 15;
    // GetDisplayAttributes : DisplayAttributes
   function GetDisplayAttributes(StartElement:Integer;Elements:Integer;UseReplacements:WordBool): SpeechDisplayAttributes;dispid 16;
    // LanguageId : LanguageId
   property LanguageId: Integer  readonly dispid 1;
    // GrammarId : GrammarId
   property GrammarId: OleVariant  readonly dispid 2;
    // StartTime : StartTime
   property StartTime: OleVariant  readonly dispid 3;
    // AudioStreamPosition : AudioStreamPosition
   property AudioStreamPosition: OleVariant  readonly dispid 4;
    // AudioSizeBytes : AudioSizeBytes
   property AudioSizeBytes: Integer  readonly dispid 5;
    // RetainedSizeBytes : RetainedSizeBytes
   property RetainedSizeBytes: Integer  readonly dispid 6;
    // AudioSizeTime : AudioSizeTime
   property AudioSizeTime: Integer  readonly dispid 7;
    // Rule : Rule
   property Rule: ISpeechPhraseRule  readonly dispid 8;
    // Properties : Properties
   property Properties: ISpeechPhraseProperties  readonly dispid 9;
    // Elements : Elements
   property Elements: ISpeechPhraseElements  readonly dispid 10;
    // Replacements : Replacements
   property Replacements: ISpeechPhraseReplacements  readonly dispid 11;
    // EngineId : EngineId
   property EngineId: WideString  readonly dispid 12;
    // EnginePrivateData : EnginePrivateData
   property EnginePrivateData: OleVariant  readonly dispid 13;
  end;


// ISpeechPhraseRule : ISpeechPhraseRule Interface

 ISpeechPhraseRule = interface(IDispatch)
   ['{A7BFE112-A4A0-48D9-B602-C313843F6964}']
   function Get_Name : WideString; safecall;
   function Get_Id : Integer; safecall;
   function Get_FirstElement : Integer; safecall;
   function Get_NumberOfElements : Integer; safecall;
   function Get_Parent : ISpeechPhraseRule; safecall;
   function Get_Children : ISpeechPhraseRules; safecall;
   function Get_Confidence : SpeechEngineConfidence; safecall;
   function Get_EngineConfidence : Single; safecall;
    // Name : Name
   property Name: WideString read Get_Name;
    // Id : Id
   property Id: Integer read Get_Id;
    // FirstElement : FirstElement
   property FirstElement: Integer read Get_FirstElement;
    // NumberOfElements : NumElements
   property NumberOfElements: Integer read Get_NumberOfElements;
    // Parent : Parent
   property Parent: ISpeechPhraseRule read Get_Parent;
    // Children : Children
   property Children: ISpeechPhraseRules read Get_Children;
    // Confidence : Confidence
   property Confidence: SpeechEngineConfidence read Get_Confidence;
    // EngineConfidence : EngineConfidence
   property EngineConfidence: Single read Get_EngineConfidence;
  end;


// ISpeechPhraseRule : ISpeechPhraseRule Interface

 ISpeechPhraseRuleDisp = dispinterface
   ['{A7BFE112-A4A0-48D9-B602-C313843F6964}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Name : Name
   property Name: WideString  readonly dispid 1;
    // Id : Id
   property Id: Integer  readonly dispid 2;
    // FirstElement : FirstElement
   property FirstElement: Integer  readonly dispid 3;
    // NumberOfElements : NumElements
   property NumberOfElements: Integer  readonly dispid 4;
    // Parent : Parent
   property Parent: ISpeechPhraseRule  readonly dispid 5;
    // Children : Children
   property Children: ISpeechPhraseRules  readonly dispid 6;
    // Confidence : Confidence
   property Confidence: SpeechEngineConfidence  readonly dispid 7;
    // EngineConfidence : EngineConfidence
   property EngineConfidence: Single  readonly dispid 8;
  end;


// ISpeechPhraseRules : ISpeechPhraseRules Interface

 ISpeechPhraseRules = interface(IDispatch)
   ['{9047D593-01DD-4B72-81A3-E4A0CA69F407}']
   function Get_Count : Integer; safecall;
    // Item : Item
   function Item(Index:Integer): ISpeechPhraseRule;safecall;
   function Get__NewEnum : IUnknown; safecall;
    // Count : Count
   property Count: Integer read Get_Count;
    // _NewEnum : Enumerates the Rules
   property _NewEnum: IUnknown read Get__NewEnum;
  end;


// ISpeechPhraseRules : ISpeechPhraseRules Interface

 ISpeechPhraseRulesDisp = dispinterface
   ['{9047D593-01DD-4B72-81A3-E4A0CA69F407}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Item : Item
   function Item(Index:Integer): ISpeechPhraseRule;dispid 0;
    // Count : Count
   property Count: Integer  readonly dispid 1;
    // _NewEnum : Enumerates the Rules
   property _NewEnum: IUnknown  readonly dispid -4;
  end;


// ISpeechPhraseProperties : ISpeechPhraseProperties Interface

 ISpeechPhraseProperties = interface(IDispatch)
   ['{08166B47-102E-4B23-A599-BDB98DBFD1F4}']
   function Get_Count : Integer; safecall;
    // Item : Item
   function Item(Index:Integer): ISpeechPhraseProperty;safecall;
   function Get__NewEnum : IUnknown; safecall;
    // Count : Count
   property Count: Integer read Get_Count;
    // _NewEnum : Enumerates the alternates
   property _NewEnum: IUnknown read Get__NewEnum;
  end;


// ISpeechPhraseProperties : ISpeechPhraseProperties Interface

 ISpeechPhrasePropertiesDisp = dispinterface
   ['{08166B47-102E-4B23-A599-BDB98DBFD1F4}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Item : Item
   function Item(Index:Integer): ISpeechPhraseProperty;dispid 0;
    // Count : Count
   property Count: Integer  readonly dispid 1;
    // _NewEnum : Enumerates the alternates
   property _NewEnum: IUnknown  readonly dispid -4;
  end;


// ISpeechPhraseProperty : ISpeechPhraseProperty Interface

 ISpeechPhraseProperty = interface(IDispatch)
   ['{CE563D48-961E-4732-A2E1-378A42B430BE}']
   function Get_Name : WideString; safecall;
   function Get_Id : Integer; safecall;
   function Get_Value : OleVariant; safecall;
   function Get_FirstElement : Integer; safecall;
   function Get_NumberOfElements : Integer; safecall;
   function Get_EngineConfidence : Single; safecall;
   function Get_Confidence : SpeechEngineConfidence; safecall;
   function Get_Parent : ISpeechPhraseProperty; safecall;
   function Get_Children : ISpeechPhraseProperties; safecall;
    // Name : Name
   property Name: WideString read Get_Name;
    // Id : Id
   property Id: Integer read Get_Id;
    // Value : Value
   property Value: OleVariant read Get_Value;
    // FirstElement : FirstElement
   property FirstElement: Integer read Get_FirstElement;
    // NumberOfElements : NumberOfElements
   property NumberOfElements: Integer read Get_NumberOfElements;
    // EngineConfidence : EngineConfidence
   property EngineConfidence: Single read Get_EngineConfidence;
    // Confidence : Confidence
   property Confidence: SpeechEngineConfidence read Get_Confidence;
    // Parent : Parent
   property Parent: ISpeechPhraseProperty read Get_Parent;
    // Children : Children
   property Children: ISpeechPhraseProperties read Get_Children;
  end;


// ISpeechPhraseProperty : ISpeechPhraseProperty Interface

 ISpeechPhrasePropertyDisp = dispinterface
   ['{CE563D48-961E-4732-A2E1-378A42B430BE}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Name : Name
   property Name: WideString  readonly dispid 1;
    // Id : Id
   property Id: Integer  readonly dispid 2;
    // Value : Value
   property Value: OleVariant  readonly dispid 3;
    // FirstElement : FirstElement
   property FirstElement: Integer  readonly dispid 4;
    // NumberOfElements : NumberOfElements
   property NumberOfElements: Integer  readonly dispid 5;
    // EngineConfidence : EngineConfidence
   property EngineConfidence: Single  readonly dispid 6;
    // Confidence : Confidence
   property Confidence: SpeechEngineConfidence  readonly dispid 7;
    // Parent : Parent
   property Parent: ISpeechPhraseProperty  readonly dispid 8;
    // Children : Children
   property Children: ISpeechPhraseProperties  readonly dispid 9;
  end;


// ISpeechPhraseElements : ISpeechPhraseElements Interface

 ISpeechPhraseElements = interface(IDispatch)
   ['{0626B328-3478-467D-A0B3-D0853B93DDA3}']
   function Get_Count : Integer; safecall;
    // Item : Item
   function Item(Index:Integer): ISpeechPhraseElement;safecall;
   function Get__NewEnum : IUnknown; safecall;
    // Count : Count
   property Count: Integer read Get_Count;
    // _NewEnum : Enumerates the tokens
   property _NewEnum: IUnknown read Get__NewEnum;
  end;


// ISpeechPhraseElements : ISpeechPhraseElements Interface

 ISpeechPhraseElementsDisp = dispinterface
   ['{0626B328-3478-467D-A0B3-D0853B93DDA3}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Item : Item
   function Item(Index:Integer): ISpeechPhraseElement;dispid 0;
    // Count : Count
   property Count: Integer  readonly dispid 1;
    // _NewEnum : Enumerates the tokens
   property _NewEnum: IUnknown  readonly dispid -4;
  end;


// ISpeechPhraseElement : ISpeechPhraseElement Interface

 ISpeechPhraseElement = interface(IDispatch)
   ['{E6176F96-E373-4801-B223-3B62C068C0B4}']
   function Get_AudioTimeOffset : Integer; safecall;
   function Get_AudioSizeTime : Integer; safecall;
   function Get_AudioStreamOffset : Integer; safecall;
   function Get_AudioSizeBytes : Integer; safecall;
   function Get_RetainedStreamOffset : Integer; safecall;
   function Get_RetainedSizeBytes : Integer; safecall;
   function Get_DisplayText : WideString; safecall;
   function Get_LexicalForm : WideString; safecall;
   function Get_Pronunciation : OleVariant; safecall;
   function Get_DisplayAttributes : SpeechDisplayAttributes; safecall;
   function Get_RequiredConfidence : SpeechEngineConfidence; safecall;
   function Get_ActualConfidence : SpeechEngineConfidence; safecall;
   function Get_EngineConfidence : Single; safecall;
    // AudioTimeOffset : AudioTimeOffset
   property AudioTimeOffset: Integer read Get_AudioTimeOffset;
    // AudioSizeTime : AudioSizeTime
   property AudioSizeTime: Integer read Get_AudioSizeTime;
    // AudioStreamOffset : AudioStreamOffset
   property AudioStreamOffset: Integer read Get_AudioStreamOffset;
    // AudioSizeBytes : AudioSizeBytes
   property AudioSizeBytes: Integer read Get_AudioSizeBytes;
    // RetainedStreamOffset : RetainedStreamOffset
   property RetainedStreamOffset: Integer read Get_RetainedStreamOffset;
    // RetainedSizeBytes : RetainedSizeBytes
   property RetainedSizeBytes: Integer read Get_RetainedSizeBytes;
    // DisplayText : DisplayText
   property DisplayText: WideString read Get_DisplayText;
    // LexicalForm : LexicalForm
   property LexicalForm: WideString read Get_LexicalForm;
    // Pronunciation : Pronunciation
   property Pronunciation: OleVariant read Get_Pronunciation;
    // DisplayAttributes : DisplayAttributes
   property DisplayAttributes: SpeechDisplayAttributes read Get_DisplayAttributes;
    // RequiredConfidence : RequiredConfidence
   property RequiredConfidence: SpeechEngineConfidence read Get_RequiredConfidence;
    // ActualConfidence : ActualConfidence
   property ActualConfidence: SpeechEngineConfidence read Get_ActualConfidence;
    // EngineConfidence : EngineConfidence
   property EngineConfidence: Single read Get_EngineConfidence;
  end;


// ISpeechPhraseElement : ISpeechPhraseElement Interface

 ISpeechPhraseElementDisp = dispinterface
   ['{E6176F96-E373-4801-B223-3B62C068C0B4}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // AudioTimeOffset : AudioTimeOffset
   property AudioTimeOffset: Integer  readonly dispid 1;
    // AudioSizeTime : AudioSizeTime
   property AudioSizeTime: Integer  readonly dispid 2;
    // AudioStreamOffset : AudioStreamOffset
   property AudioStreamOffset: Integer  readonly dispid 3;
    // AudioSizeBytes : AudioSizeBytes
   property AudioSizeBytes: Integer  readonly dispid 4;
    // RetainedStreamOffset : RetainedStreamOffset
   property RetainedStreamOffset: Integer  readonly dispid 5;
    // RetainedSizeBytes : RetainedSizeBytes
   property RetainedSizeBytes: Integer  readonly dispid 6;
    // DisplayText : DisplayText
   property DisplayText: WideString  readonly dispid 7;
    // LexicalForm : LexicalForm
   property LexicalForm: WideString  readonly dispid 8;
    // Pronunciation : Pronunciation
   property Pronunciation: OleVariant  readonly dispid 9;
    // DisplayAttributes : DisplayAttributes
   property DisplayAttributes: SpeechDisplayAttributes  readonly dispid 10;
    // RequiredConfidence : RequiredConfidence
   property RequiredConfidence: SpeechEngineConfidence  readonly dispid 11;
    // ActualConfidence : ActualConfidence
   property ActualConfidence: SpeechEngineConfidence  readonly dispid 12;
    // EngineConfidence : EngineConfidence
   property EngineConfidence: Single  readonly dispid 13;
  end;


// ISpeechPhraseReplacements : ISpeechPhraseReplacements Interface

 ISpeechPhraseReplacements = interface(IDispatch)
   ['{38BC662F-2257-4525-959E-2069D2596C05}']
   function Get_Count : Integer; safecall;
    // Item : Item
   function Item(Index:Integer): ISpeechPhraseReplacement;safecall;
   function Get__NewEnum : IUnknown; safecall;
    // Count : Count
   property Count: Integer read Get_Count;
    // _NewEnum : Enumerates the tokens
   property _NewEnum: IUnknown read Get__NewEnum;
  end;


// ISpeechPhraseReplacements : ISpeechPhraseReplacements Interface

 ISpeechPhraseReplacementsDisp = dispinterface
   ['{38BC662F-2257-4525-959E-2069D2596C05}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Item : Item
   function Item(Index:Integer): ISpeechPhraseReplacement;dispid 0;
    // Count : Count
   property Count: Integer  readonly dispid 1;
    // _NewEnum : Enumerates the tokens
   property _NewEnum: IUnknown  readonly dispid -4;
  end;


// ISpeechPhraseReplacement : ISpeechPhraseReplacement Interface

 ISpeechPhraseReplacement = interface(IDispatch)
   ['{2890A410-53A7-4FB5-94EC-06D4998E3D02}']
   function Get_DisplayAttributes : SpeechDisplayAttributes; safecall;
   function Get_Text_ : WideString; safecall;
   function Get_FirstElement : Integer; safecall;
   function Get_NumberOfElements : Integer; safecall;
    // DisplayAttributes : DisplayAttributes
   property DisplayAttributes: SpeechDisplayAttributes read Get_DisplayAttributes;
    // Text : Text
   property Text_: WideString read Get_Text_;
    // FirstElement : FirstElement
   property FirstElement: Integer read Get_FirstElement;
    // NumberOfElements : NumElements
   property NumberOfElements: Integer read Get_NumberOfElements;
  end;


// ISpeechPhraseReplacement : ISpeechPhraseReplacement Interface

 ISpeechPhraseReplacementDisp = dispinterface
   ['{2890A410-53A7-4FB5-94EC-06D4998E3D02}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // DisplayAttributes : DisplayAttributes
   property DisplayAttributes: SpeechDisplayAttributes  readonly dispid 1;
    // Text : Text
   property Text_: WideString  readonly dispid 2;
    // FirstElement : FirstElement
   property FirstElement: Integer  readonly dispid 3;
    // NumberOfElements : NumElements
   property NumberOfElements: Integer  readonly dispid 4;
  end;


// ISpeechPhraseAlternates : ISpeechPhraseAlternates Interface

 ISpeechPhraseAlternates = interface(IDispatch)
   ['{B238B6D5-F276-4C3D-A6C1-2974801C3CC2}']
   function Get_Count : Integer; safecall;
    // Item : Item
   function Item(Index:Integer): ISpeechPhraseAlternate;safecall;
   function Get__NewEnum : IUnknown; safecall;
    // Count : Count
   property Count: Integer read Get_Count;
    // _NewEnum : Enumerates the alternates
   property _NewEnum: IUnknown read Get__NewEnum;
  end;


// ISpeechPhraseAlternates : ISpeechPhraseAlternates Interface

 ISpeechPhraseAlternatesDisp = dispinterface
   ['{B238B6D5-F276-4C3D-A6C1-2974801C3CC2}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Item : Item
   function Item(Index:Integer): ISpeechPhraseAlternate;dispid 0;
    // Count : Count
   property Count: Integer  readonly dispid 1;
    // _NewEnum : Enumerates the alternates
   property _NewEnum: IUnknown  readonly dispid -4;
  end;


// ISpeechPhraseAlternate : ISpeechPhraseAlternate Interface

 ISpeechPhraseAlternate = interface(IDispatch)
   ['{27864A2A-2B9F-4CB8-92D3-0D2722FD1E73}']
   function Get_RecoResult : ISpeechRecoResult; safecall;
   function Get_StartElementInResult : Integer; safecall;
   function Get_NumberOfElementsInResult : Integer; safecall;
   function Get_PhraseInfo : ISpeechPhraseInfo; safecall;
    // Commit : Commit
   procedure Commit;safecall;
    // RecoResult : RecoResult
   property RecoResult: ISpeechRecoResult read Get_RecoResult;
    // StartElementInResult : StartElementInResult
   property StartElementInResult: Integer read Get_StartElementInResult;
    // NumberOfElementsInResult : NumberOfElementsInResult
   property NumberOfElementsInResult: Integer read Get_NumberOfElementsInResult;
    // PhraseInfo : Phrase
   property PhraseInfo: ISpeechPhraseInfo read Get_PhraseInfo;
  end;


// ISpeechPhraseAlternate : ISpeechPhraseAlternate Interface

 ISpeechPhraseAlternateDisp = dispinterface
   ['{27864A2A-2B9F-4CB8-92D3-0D2722FD1E73}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Commit : Commit
   procedure Commit;dispid 5;
    // RecoResult : RecoResult
   property RecoResult: ISpeechRecoResult  readonly dispid 1;
    // StartElementInResult : StartElementInResult
   property StartElementInResult: Integer  readonly dispid 2;
    // NumberOfElementsInResult : NumberOfElementsInResult
   property NumberOfElementsInResult: Integer  readonly dispid 3;
    // PhraseInfo : Phrase
   property PhraseInfo: ISpeechPhraseInfo  readonly dispid 4;
  end;


// _ISpeechRecoContextEvents :

 _ISpeechRecoContextEvents = dispinterface
   ['{7B8FCB42-0E9D-4F00-A048-7B04D6179D3D}']
    // StartStream : StartStream
   procedure StartStream(StreamNumber:Integer;StreamPosition:OleVariant);dispid 1;
    // EndStream : EndStream
   procedure EndStream(StreamNumber:Integer;StreamPosition:OleVariant;StreamReleased:WordBool);dispid 2;
    // Bookmark : Bookmark
   procedure Bookmark(StreamNumber:Integer;StreamPosition:OleVariant;BookmarkId:OleVariant;Options:SpeechBookmarkOptions);dispid 3;
    // SoundStart : SoundStart
   procedure SoundStart(StreamNumber:Integer;StreamPosition:OleVariant);dispid 4;
    // SoundEnd : SoundEnd
   procedure SoundEnd(StreamNumber:Integer;StreamPosition:OleVariant);dispid 5;
    // PhraseStart : PhraseStart
   procedure PhraseStart(StreamNumber:Integer;StreamPosition:OleVariant);dispid 6;
    // Recognition : Recognition
   procedure Recognition(StreamNumber:Integer;StreamPosition:OleVariant;RecognitionType:SpeechRecognitionType;Result:ISpeechRecoResult);dispid 7;
    // Hypothesis : Hypothesis
   procedure Hypothesis(StreamNumber:Integer;StreamPosition:OleVariant;Result:ISpeechRecoResult);dispid 8;
    // PropertyNumberChange : PropertyNumberChange
   procedure PropertyNumberChange(StreamNumber:Integer;StreamPosition:OleVariant;PropertyName:WideString;NewNumberValue:Integer);dispid 9;
    // PropertyStringChange : PropertyStringChange
   procedure PropertyStringChange(StreamNumber:Integer;StreamPosition:OleVariant;PropertyName:WideString;NewStringValue:WideString);dispid 10;
    // FalseRecognition : FalseRecognition
   procedure FalseRecognition(StreamNumber:Integer;StreamPosition:OleVariant;Result:ISpeechRecoResult);dispid 11;
    // Interference : Interference
   procedure Interference(StreamNumber:Integer;StreamPosition:OleVariant;Interference:SpeechInterference);dispid 12;
    // RequestUI : RequestUI
   procedure RequestUI(StreamNumber:Integer;StreamPosition:OleVariant;UIType:WideString);dispid 13;
    // RecognizerStateChange : RecognizerStateChange
   procedure RecognizerStateChange(StreamNumber:Integer;StreamPosition:OleVariant;NewState:SpeechRecognizerState);dispid 14;
    // Adaptation : Adaptation
   procedure Adaptation(StreamNumber:Integer;StreamPosition:OleVariant);dispid 15;
    // RecognitionForOtherContext : RecognitionForOtherContext
   procedure RecognitionForOtherContext(StreamNumber:Integer;StreamPosition:OleVariant);dispid 16;
    // AudioLevel : AudioLevel
   procedure AudioLevel(StreamNumber:Integer;StreamPosition:OleVariant;AudioLevel:Integer);dispid 17;
    // EnginePrivate : EnginePrivate
   procedure EnginePrivate(StreamNumber:Integer;StreamPosition:OleVariant;EngineData:OleVariant);dispid 18;
  end;


// ISpeechRecoResult2 : ISpeechRecoResult2 Interface

 ISpeechRecoResult2 = interface(ISpeechRecoResult)
   ['{8E0A246D-D3C8-45DE-8657-04290C458C3C}']
    // SetTextFeedback : DiscardResultInfo
   procedure SetTextFeedback(Feedback:WideString;WasSuccessful:WordBool);safecall;
  end;


// ISpeechRecoResult2 : ISpeechRecoResult2 Interface

 ISpeechRecoResult2Disp = dispinterface
   ['{8E0A246D-D3C8-45DE-8657-04290C458C3C}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Alternates : Alternates
   function Alternates(RequestCount:Integer;StartElement:Integer;Elements:Integer): ISpeechPhraseAlternates;dispid 5;
    // Audio : Audio
   function Audio(StartElement:Integer;Elements:Integer): ISpeechMemoryStream;dispid 6;
    // SpeakAudio : SpeakAudio
   function SpeakAudio(StartElement:Integer;Elements:Integer;Flags:SpeechVoiceSpeakFlags): Integer;dispid 7;
    // SaveToMemory : SaveToMemory
   function SaveToMemory: OleVariant;dispid 8;
    // DiscardResultInfo : DiscardResultInfo
   procedure DiscardResultInfo(ValueTypes:SpeechDiscardType);dispid 9;
    // SetTextFeedback : DiscardResultInfo
   procedure SetTextFeedback(Feedback:WideString;WasSuccessful:WordBool);dispid 12;
    // RecoContext : RecoContext
   property RecoContext: ISpeechRecoContext  readonly dispid 1;
    // Times : Times
   property Times: ISpeechRecoResultTimes  readonly dispid 2;
    // AudioFormat : AudioFormat
   property AudioFormat: ISpeechAudioFormat dispid 3;
    // PhraseInfo : PhraseInfo
   property PhraseInfo: ISpeechPhraseInfo  readonly dispid 4;
  end;


// ISpeechLexicon : ISpeechLexicon Interface

 ISpeechLexicon = interface(IDispatch)
   ['{3DA7627A-C7AE-4B23-8708-638C50362C25}']
   function Get_GenerationId : Integer; safecall;
    // GetWords : GetWords
   function GetWords(Flags:SpeechLexiconType;out GenerationId:Integer): ISpeechLexiconWords;safecall;
    // AddPronunciation : AddPronunciation
   procedure AddPronunciation(bstrWord:WideString;LangId:Integer;PartOfSpeech:SpeechPartOfSpeech;bstrPronunciation:WideString);safecall;
    // AddPronunciationByPhoneIds : AddPronunciationByPhoneIds
   procedure AddPronunciationByPhoneIds(bstrWord:WideString;LangId:Integer;PartOfSpeech:SpeechPartOfSpeech;var PhoneIds:OleVariant);safecall;
    // RemovePronunciation : RemovePronunciation
   procedure RemovePronunciation(bstrWord:WideString;LangId:Integer;PartOfSpeech:SpeechPartOfSpeech;bstrPronunciation:WideString);safecall;
    // RemovePronunciationByPhoneIds : RemovePronunciationByPhoneIds
   procedure RemovePronunciationByPhoneIds(bstrWord:WideString;LangId:Integer;PartOfSpeech:SpeechPartOfSpeech;var PhoneIds:OleVariant);safecall;
    // GetPronunciations : GetPronunciations
   function GetPronunciations(bstrWord:WideString;LangId:Integer;TypeFlags:SpeechLexiconType): ISpeechLexiconPronunciations;safecall;
    // GetGenerationChange : GetGenerationChange
   function GetGenerationChange(var GenerationId:Integer): ISpeechLexiconWords;safecall;
    // GenerationId : GenerationId
   property GenerationId: Integer read Get_GenerationId;
  end;


// ISpeechLexicon : ISpeechLexicon Interface

 ISpeechLexiconDisp = dispinterface
   ['{3DA7627A-C7AE-4B23-8708-638C50362C25}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // GetWords : GetWords
   function GetWords(Flags:SpeechLexiconType;out GenerationId:Integer): ISpeechLexiconWords;dispid 2;
    // AddPronunciation : AddPronunciation
   procedure AddPronunciation(bstrWord:WideString;LangId:Integer;PartOfSpeech:SpeechPartOfSpeech;bstrPronunciation:WideString);dispid 3;
    // AddPronunciationByPhoneIds : AddPronunciationByPhoneIds
   procedure AddPronunciationByPhoneIds(bstrWord:WideString;LangId:Integer;PartOfSpeech:SpeechPartOfSpeech;var PhoneIds:OleVariant);dispid 4;
    // RemovePronunciation : RemovePronunciation
   procedure RemovePronunciation(bstrWord:WideString;LangId:Integer;PartOfSpeech:SpeechPartOfSpeech;bstrPronunciation:WideString);dispid 5;
    // RemovePronunciationByPhoneIds : RemovePronunciationByPhoneIds
   procedure RemovePronunciationByPhoneIds(bstrWord:WideString;LangId:Integer;PartOfSpeech:SpeechPartOfSpeech;var PhoneIds:OleVariant);dispid 6;
    // GetPronunciations : GetPronunciations
   function GetPronunciations(bstrWord:WideString;LangId:Integer;TypeFlags:SpeechLexiconType): ISpeechLexiconPronunciations;dispid 7;
    // GetGenerationChange : GetGenerationChange
   function GetGenerationChange(var GenerationId:Integer): ISpeechLexiconWords;dispid 8;
    // GenerationId : GenerationId
   property GenerationId: Integer  readonly dispid 1;
  end;


// ISpeechLexiconWords : ISpeechLexiconWords Interface

 ISpeechLexiconWords = interface(IDispatch)
   ['{8D199862-415E-47D5-AC4F-FAA608B424E6}']
   function Get_Count : Integer; safecall;
    // Item : Item
   function Item(Index:Integer): ISpeechLexiconWord;safecall;
   function Get__NewEnum : IUnknown; safecall;
    // Count : Count
   property Count: Integer read Get_Count;
    // _NewEnum : Enumerates the tokens
   property _NewEnum: IUnknown read Get__NewEnum;
  end;


// ISpeechLexiconWords : ISpeechLexiconWords Interface

 ISpeechLexiconWordsDisp = dispinterface
   ['{8D199862-415E-47D5-AC4F-FAA608B424E6}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Item : Item
   function Item(Index:Integer): ISpeechLexiconWord;dispid 0;
    // Count : Count
   property Count: Integer  readonly dispid 1;
    // _NewEnum : Enumerates the tokens
   property _NewEnum: IUnknown  readonly dispid -4;
  end;


// ISpeechLexiconWord : ISpeechLexiconWord Interface

 ISpeechLexiconWord = interface(IDispatch)
   ['{4E5B933C-C9BE-48ED-8842-1EE51BB1D4FF}']
   function Get_LangId : Integer; safecall;
   function Get_Type_ : SpeechWordType; safecall;
   function Get_Word : WideString; safecall;
   function Get_Pronunciations : ISpeechLexiconPronunciations; safecall;
    // LangId :
   property LangId: Integer read Get_LangId;
    // Type :
   property Type_: SpeechWordType read Get_Type_;
    // Word :
   property Word: WideString read Get_Word;
    // Pronunciations :
   property Pronunciations: ISpeechLexiconPronunciations read Get_Pronunciations;
  end;


// ISpeechLexiconWord : ISpeechLexiconWord Interface

 ISpeechLexiconWordDisp = dispinterface
   ['{4E5B933C-C9BE-48ED-8842-1EE51BB1D4FF}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // LangId :
   property LangId: Integer  readonly dispid 1;
    // Type :
   property Type_: SpeechWordType  readonly dispid 2;
    // Word :
   property Word: WideString  readonly dispid 3;
    // Pronunciations :
   property Pronunciations: ISpeechLexiconPronunciations  readonly dispid 4;
  end;


// ISpeechLexiconPronunciations : ISpeechLexiconPronunciations Interface

 ISpeechLexiconPronunciations = interface(IDispatch)
   ['{72829128-5682-4704-A0D4-3E2BB6F2EAD3}']
   function Get_Count : Integer; safecall;
    // Item : Item
   function Item(Index:Integer): ISpeechLexiconPronunciation;safecall;
   function Get__NewEnum : IUnknown; safecall;
    // Count : Count
   property Count: Integer read Get_Count;
    // _NewEnum : Enumerates the tokens
   property _NewEnum: IUnknown read Get__NewEnum;
  end;


// ISpeechLexiconPronunciations : ISpeechLexiconPronunciations Interface

 ISpeechLexiconPronunciationsDisp = dispinterface
   ['{72829128-5682-4704-A0D4-3E2BB6F2EAD3}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Item : Item
   function Item(Index:Integer): ISpeechLexiconPronunciation;dispid 0;
    // Count : Count
   property Count: Integer  readonly dispid 1;
    // _NewEnum : Enumerates the tokens
   property _NewEnum: IUnknown  readonly dispid -4;
  end;


// ISpeechLexiconPronunciation : ISpeechLexiconPronunciation Interface

 ISpeechLexiconPronunciation = interface(IDispatch)
   ['{95252C5D-9E43-4F4A-9899-48EE73352F9F}']
   function Get_Type_ : SpeechLexiconType; safecall;
   function Get_LangId : Integer; safecall;
   function Get_PartOfSpeech : SpeechPartOfSpeech; safecall;
   function Get_PhoneIds : OleVariant; safecall;
   function Get_Symbolic : WideString; safecall;
    // Type : Type
   property Type_: SpeechLexiconType read Get_Type_;
    // LangId : LangId
   property LangId: Integer read Get_LangId;
    // PartOfSpeech : PartOfSpeech
   property PartOfSpeech: SpeechPartOfSpeech read Get_PartOfSpeech;
    // PhoneIds : PhoneIds
   property PhoneIds: OleVariant read Get_PhoneIds;
    // Symbolic : Symbolic
   property Symbolic: WideString read Get_Symbolic;
  end;


// ISpeechLexiconPronunciation : ISpeechLexiconPronunciation Interface

 ISpeechLexiconPronunciationDisp = dispinterface
   ['{95252C5D-9E43-4F4A-9899-48EE73352F9F}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Type : Type
   property Type_: SpeechLexiconType  readonly dispid 1;
    // LangId : LangId
   property LangId: Integer  readonly dispid 2;
    // PartOfSpeech : PartOfSpeech
   property PartOfSpeech: SpeechPartOfSpeech  readonly dispid 3;
    // PhoneIds : PhoneIds
   property PhoneIds: OleVariant  readonly dispid 4;
    // Symbolic : Symbolic
   property Symbolic: WideString  readonly dispid 5;
  end;


// ISpeechXMLRecoResult : ISpeechXMLRecoResult Interface

 ISpeechXMLRecoResult = interface(ISpeechRecoResult)
   ['{AAEC54AF-8F85-4924-944D-B79D39D72E19}']
    // GetXMLResult : GetXMLResult
   function GetXMLResult(Options:SPXMLRESULTOPTIONS): WideString;safecall;
    // GetXMLErrorInfo : GetXMLErrorInfo
   function GetXMLErrorInfo(out LineNumber:Integer;out ScriptLine:WideString;out Source:WideString;out Description:WideString;out ResultCode:Integer): WordBool;safecall;
  end;


// ISpeechXMLRecoResult : ISpeechXMLRecoResult Interface

 ISpeechXMLRecoResultDisp = dispinterface
   ['{AAEC54AF-8F85-4924-944D-B79D39D72E19}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Alternates : Alternates
   function Alternates(RequestCount:Integer;StartElement:Integer;Elements:Integer): ISpeechPhraseAlternates;dispid 5;
    // Audio : Audio
   function Audio(StartElement:Integer;Elements:Integer): ISpeechMemoryStream;dispid 6;
    // SpeakAudio : SpeakAudio
   function SpeakAudio(StartElement:Integer;Elements:Integer;Flags:SpeechVoiceSpeakFlags): Integer;dispid 7;
    // SaveToMemory : SaveToMemory
   function SaveToMemory: OleVariant;dispid 8;
    // DiscardResultInfo : DiscardResultInfo
   procedure DiscardResultInfo(ValueTypes:SpeechDiscardType);dispid 9;
    // GetXMLResult : GetXMLResult
   function GetXMLResult(Options:SPXMLRESULTOPTIONS): WideString;dispid 10;
    // GetXMLErrorInfo : GetXMLErrorInfo
   function GetXMLErrorInfo(out LineNumber:Integer;out ScriptLine:WideString;out Source:WideString;out Description:WideString;out ResultCode:Integer): WordBool;dispid 11;
    // RecoContext : RecoContext
   property RecoContext: ISpeechRecoContext  readonly dispid 1;
    // Times : Times
   property Times: ISpeechRecoResultTimes  readonly dispid 2;
    // AudioFormat : AudioFormat
   property AudioFormat: ISpeechAudioFormat dispid 3;
    // PhraseInfo : PhraseInfo
   property PhraseInfo: ISpeechPhraseInfo  readonly dispid 4;
  end;


// ISpeechRecoResultDispatch : ISpeechRecoResultDispatch Interface

 ISpeechRecoResultDispatch = interface(IDispatch)
   ['{6D60EB64-ACED-40A6-BBF3-4E557F71DEE2}']
   function Get_RecoContext : ISpeechRecoContext; safecall;
   function Get_Times : ISpeechRecoResultTimes; safecall;
   procedure Set_AudioFormat(const Format:ISpeechAudioFormat); safecall;
   function Get_AudioFormat : ISpeechAudioFormat; safecall;
   function Get_PhraseInfo : ISpeechPhraseInfo; safecall;
    // Alternates : Alternates
   function Alternates(RequestCount:Integer;StartElement:Integer;Elements:Integer): ISpeechPhraseAlternates;safecall;
    // Audio : Audio
   function Audio(StartElement:Integer;Elements:Integer): ISpeechMemoryStream;safecall;
    // SpeakAudio : SpeakAudio
   function SpeakAudio(StartElement:Integer;Elements:Integer;Flags:SpeechVoiceSpeakFlags): Integer;safecall;
    // SaveToMemory : SaveToMemory
   function SaveToMemory: OleVariant;safecall;
    // DiscardResultInfo : DiscardResultInfo
   procedure DiscardResultInfo(ValueTypes:SpeechDiscardType);safecall;
    // GetXMLResult : GetXMLResult
   function GetXMLResult(Options:SPXMLRESULTOPTIONS): WideString;safecall;
    // GetXMLErrorInfo : GetXMLErrorInfo
   function GetXMLErrorInfo(out LineNumber:Integer;out ScriptLine:WideString;out Source:WideString;out Description:WideString;out ResultCode:HResult): WordBool;safecall;
    // SetTextFeedback : SetTextFeedback
   procedure SetTextFeedback(Feedback:WideString;WasSuccessful:WordBool);safecall;
    // RecoContext : RecoContext
   property RecoContext: ISpeechRecoContext read Get_RecoContext;
    // Times : Times
   property Times: ISpeechRecoResultTimes read Get_Times;
    // AudioFormat : AudioFormat
   property AudioFormat: ISpeechAudioFormat read Get_AudioFormat write Set_AudioFormat;
    // PhraseInfo : PhraseInfo
   property PhraseInfo: ISpeechPhraseInfo read Get_PhraseInfo;
  end;


// ISpeechRecoResultDispatch : ISpeechRecoResultDispatch Interface

 ISpeechRecoResultDispatchDisp = dispinterface
   ['{6D60EB64-ACED-40A6-BBF3-4E557F71DEE2}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Alternates : Alternates
   function Alternates(RequestCount:Integer;StartElement:Integer;Elements:Integer): ISpeechPhraseAlternates;dispid 5;
    // Audio : Audio
   function Audio(StartElement:Integer;Elements:Integer): ISpeechMemoryStream;dispid 6;
    // SpeakAudio : SpeakAudio
   function SpeakAudio(StartElement:Integer;Elements:Integer;Flags:SpeechVoiceSpeakFlags): Integer;dispid 7;
    // SaveToMemory : SaveToMemory
   function SaveToMemory: OleVariant;dispid 8;
    // DiscardResultInfo : DiscardResultInfo
   procedure DiscardResultInfo(ValueTypes:SpeechDiscardType);dispid 9;
    // GetXMLResult : GetXMLResult
   function GetXMLResult(Options:SPXMLRESULTOPTIONS): WideString;dispid 10;
    // GetXMLErrorInfo : GetXMLErrorInfo
   function GetXMLErrorInfo(out LineNumber:Integer;out ScriptLine:WideString;out Source:WideString;out Description:WideString;out ResultCode:HResult): WordBool;dispid 11;
    // SetTextFeedback : SetTextFeedback
   procedure SetTextFeedback(Feedback:WideString;WasSuccessful:WordBool);dispid 12;
    // RecoContext : RecoContext
   property RecoContext: ISpeechRecoContext  readonly dispid 1;
    // Times : Times
   property Times: ISpeechRecoResultTimes  readonly dispid 2;
    // AudioFormat : AudioFormat
   property AudioFormat: ISpeechAudioFormat dispid 3;
    // PhraseInfo : PhraseInfo
   property PhraseInfo: ISpeechPhraseInfo  readonly dispid 4;
  end;


// ISpeechPhraseInfoBuilder : ISpeechPhraseInfoBuilder Interface

 ISpeechPhraseInfoBuilder = interface(IDispatch)
   ['{3B151836-DF3A-4E0A-846C-D2ADC9334333}']
    // RestorePhraseFromMemory : RestorePhraseFromMemory
   function RestorePhraseFromMemory(var PhraseInMemory:OleVariant): ISpeechPhraseInfo;safecall;
  end;


// ISpeechPhraseInfoBuilder : ISpeechPhraseInfoBuilder Interface

 ISpeechPhraseInfoBuilderDisp = dispinterface
   ['{3B151836-DF3A-4E0A-846C-D2ADC9334333}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // RestorePhraseFromMemory : RestorePhraseFromMemory
   function RestorePhraseFromMemory(var PhraseInMemory:OleVariant): ISpeechPhraseInfo;dispid 1;
  end;


// ISpeechPhoneConverter : ISpeechPhoneConverter Interface

 ISpeechPhoneConverter = interface(IDispatch)
   ['{C3E4F353-433F-43D6-89A1-6A62A7054C3D}']
   function Get_LanguageId : Integer; safecall;
   procedure Set_LanguageId(const LanguageId:Integer); safecall;
    // PhoneToId : PhoneToId
   function PhoneToId(Phonemes:WideString): OleVariant;safecall;
    // IdToPhone : IdToPhone
   function IdToPhone(IdArray:OleVariant): WideString;safecall;
    // LanguageId : LanguageId
   property LanguageId: Integer read Get_LanguageId write Set_LanguageId;
  end;


// ISpeechPhoneConverter : ISpeechPhoneConverter Interface

 ISpeechPhoneConverterDisp = dispinterface
   ['{C3E4F353-433F-43D6-89A1-6A62A7054C3D}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // PhoneToId : PhoneToId
   function PhoneToId(Phonemes:WideString): OleVariant;dispid 2;
    // IdToPhone : IdToPhone
   function IdToPhone(IdArray:OleVariant): WideString;dispid 3;
    // LanguageId : LanguageId
   property LanguageId: Integer dispid 1;
  end;


// ISpNotifySink : ISpNotifySink Interface

 ISpNotifySink = interface(IUnknown)
   ['{259684DC-37C3-11D2-9603-00C04F8EE628}']
    // Notify :
   function Notify: HRESULT;stdcall;
  end;


// ISpNotifyTranslator : ISpNotifyTranslator Interface

 ISpNotifyTranslator = interface(ISpNotifySink)
   ['{ACA16614-5D3D-11D2-960E-00C04F8EE628}']
    // InitWindowMessage :
   function InitWindowMessage(hWnd:wireHWND;Msg:UInt;wParam:UINT_PTR;lParam:LONG_PTR): HRESULT;stdcall;
    // InitCallback :
   function InitCallback(var pfnCallback:Ppointer;wParam:UINT_PTR;lParam:LONG_PTR): HRESULT;stdcall;
    // InitSpNotifyCallback :
   function InitSpNotifyCallback(var pSpCallback:Ppointer;wParam:UINT_PTR;lParam:LONG_PTR): HRESULT;stdcall;
    // InitWin32Event :
   function InitWin32Event(var hEvent:pointer;fCloseHandleOnRelease:Integer): HRESULT;stdcall;
    // Wait :
   function Wait(dwMilliseconds:LongWord): HRESULT;stdcall;
    // GetEventHandle :
   function GetEventHandle: HRESULT;stdcall;
  end;


// ISpDataKey : ISpDataKey Interface

 ISpDataKey = interface(IUnknown)
   ['{14056581-E16C-11D2-BB90-00C04F8EE6C0}']
    // SetData :
   function SetData(pszValueName:PWideChar;cbData:LongWord;var pData:Byte): HRESULT;stdcall;
    // GetData :
   function GetData(pszValueName:PWideChar;var pcbData:LongWord;out pData:Byte): HRESULT;stdcall;
    // SetStringValue :
   function SetStringValue(pszValueName:PWideChar;pszValue:PWideChar): HRESULT;stdcall;
    // GetStringValue :
   function GetStringValue(pszValueName:PWideChar;out ppszValue:PWideChar): HRESULT;stdcall;
    // SetDWORD :
   function SetDWORD(pszValueName:PWideChar;dwValue:LongWord): HRESULT;stdcall;
    // GetDWORD :
   function GetDWORD(pszValueName:PWideChar;out pdwValue:LongWord): HRESULT;stdcall;
    // OpenKey :
   function OpenKey(pszSubKeyName:PWideChar;out ppSubKey:ISpDataKey): HRESULT;stdcall;
    // CreateKey :
   function CreateKey(pszSubKey:PWideChar;out ppSubKey:ISpDataKey): HRESULT;stdcall;
    // DeleteKey :
   function DeleteKey(pszSubKey:PWideChar): HRESULT;stdcall;
    // DeleteValue :
   function DeleteValue(pszValueName:PWideChar): HRESULT;stdcall;
    // EnumKeys :
   function EnumKeys(Index:LongWord;out ppszSubKeyName:PWideChar): HRESULT;stdcall;
    // EnumValues :
   function EnumValues(Index:LongWord;out ppszValueName:PWideChar): HRESULT;stdcall;
  end;


// ISpObjectTokenCategory : ISpObjectTokenCategory

 ISpObjectTokenCategory = interface(ISpDataKey)
   ['{2D3D3845-39AF-4850-BBF9-40B49780011D}']
    // SetId :
   function SetId(pszCategoryId:PWideChar;fCreateIfNotExist:Integer): HRESULT;stdcall;
    // GetId :
   function GetId(out ppszCoMemCategoryId:PWideChar): HRESULT;stdcall;
    // GetDataKey :
   function GetDataKey(spdkl:SPDATAKEYLOCATION;out ppDataKey:ISpDataKey): HRESULT;stdcall;
    // EnumTokens :
   function EnumTokens(pzsReqAttribs:PWideChar;pszOptAttribs:PWideChar;out ppEnum:IEnumSpObjectTokens): HRESULT;stdcall;
    // SetDefaultTokenId :
   function SetDefaultTokenId(pszTokenId:PWideChar): HRESULT;stdcall;
    // GetDefaultTokenId :
   function GetDefaultTokenId(out ppszCoMemTokenId:PWideChar): HRESULT;stdcall;
  end;


// IEnumSpObjectTokens : IEnumSpObjectTokens Interface

 IEnumSpObjectTokens = interface(IUnknown)
   ['{06B64F9E-7FDA-11D2-B4F2-00C04F797396}']
    // Next :
   function Next(celt:LongWord;out pelt:ISpObjectToken;out pceltFetched:LongWord): HRESULT;stdcall;
    // Skip :
   function Skip(celt:LongWord): HRESULT;stdcall;
    // Reset_ :
   function Reset_: HRESULT;stdcall;
    // Clone :
   function Clone(out ppEnum:IEnumSpObjectTokens): HRESULT;stdcall;
    // Item :
   function Item(Index:LongWord;out ppToken:ISpObjectToken): HRESULT;stdcall;
    // GetCount :
   function GetCount(out pCount:LongWord): HRESULT;stdcall;
  end;


// ISpObjectToken : ISpObjectToken Interface

 ISpObjectToken = interface(ISpDataKey)
   ['{14056589-E16C-11D2-BB90-00C04F8EE6C0}']
    // SetId :
   function SetId(pszCategoryId:PWideChar;pszTokenId:PWideChar;fCreateIfNotExist:Integer): HRESULT;stdcall;
    // GetId :
   function GetId(out ppszCoMemTokenId:PWideChar): HRESULT;stdcall;
    // GetCategory :
   function GetCategory(out ppTokenCategory:ISpObjectTokenCategory): HRESULT;stdcall;
    // CreateInstance :
   function CreateInstance(pUnkOuter:IUnknown;dwClsContext:LongWord;var riid:GUID;out ppvObject:Ppointer): HRESULT;stdcall;
    // GetStorageFileName :
   function GetStorageFileName(var clsidCaller:GUID;pszValueName:PWideChar;pszFileNameSpecifier:PWideChar;nFolder:LongWord;out ppszFilePath:PWideChar): HRESULT;stdcall;
    // RemoveStorageFileName :
   function RemoveStorageFileName(var clsidCaller:GUID;pszKeyName:PWideChar;fDeleteFile:Integer): HRESULT;stdcall;
    // Remove :
   function Remove(var pclsidCaller:GUID): HRESULT;stdcall;
    // IsUISupported :
   function IsUISupported(pszTypeOfUI:PWideChar;var pvExtraData:pointer;cbExtraData:LongWord;punkObject:IUnknown;out pfSupported:Integer): HRESULT;stdcall;
    // DisplayUI :
   function DisplayUI(hWndParent:wireHWND;pszTitle:PWideChar;pszTypeOfUI:PWideChar;var pvExtraData:pointer;cbExtraData:LongWord;punkObject:IUnknown): HRESULT;stdcall;
    // MatchesAttributes :
   function MatchesAttributes(pszAttributes:PWideChar;out pfMatches:Integer): HRESULT;stdcall;
  end;


// IServiceProvider :

 IServiceProvider = interface(IUnknown)
   ['{6D5140C1-7436-11CE-8034-00AA006009FA}']
    // RemoteQueryService :
   function RemoteQueryService(var guidService:GUID;var riid:GUID;out ppvObject:IUnknown): HRESULT;stdcall;
  end;


// ISpResourceManager : ISpResourceManager Interface

 ISpResourceManager = interface(IServiceProvider)
   ['{93384E18-5014-43D5-ADBB-A78E055926BD}']
    // SetObject :
   function SetObject(var guidServiceId:GUID;punkObject:IUnknown): HRESULT;stdcall;
    // GetObject :
   function GetObject(var guidServiceId:GUID;var ObjectCLSID:GUID;var ObjectIID:GUID;fReleaseWhenLastExternalRefReleased:Integer;out ppObject:Ppointer): HRESULT;stdcall;
  end;


// ISequentialStream :

 ISequentialStream = interface(IUnknown)
   ['{0C733A30-2A1C-11CE-ADE5-00AA0044773D}']
    // RemoteRead :
   function RemoteRead(out pv:Byte;cb:LongWord;out pcbRead:LongWord): HRESULT;stdcall;
    // RemoteWrite :
   function RemoteWrite(var pv:Byte;cb:LongWord;out pcbWritten:LongWord): HRESULT;stdcall;
  end;


// IStream :

 IStream = interface(ISequentialStream)
   ['{0000000C-0000-0000-C000-000000000046}']
    // RemoteSeek :
   function RemoteSeek(dlibMove:_LARGE_INTEGER;dwOrigin:LongWord;out plibNewPosition:_ULARGE_INTEGER): HRESULT;stdcall;
    // SetSize :
   function SetSize(libNewSize:_ULARGE_INTEGER): HRESULT;stdcall;
    // RemoteCopyTo :
   function RemoteCopyTo(pstm:IStream;cb:_ULARGE_INTEGER;out pcbRead:_ULARGE_INTEGER;out pcbWritten:_ULARGE_INTEGER): HRESULT;stdcall;
    // Commit :
   function Commit(grfCommitFlags:LongWord): HRESULT;stdcall;
    // Revert :
   function Revert: HRESULT;stdcall;
    // LockRegion :
   function LockRegion(libOffset:_ULARGE_INTEGER;cb:_ULARGE_INTEGER;dwLockType:LongWord): HRESULT;stdcall;
    // UnlockRegion :
   function UnlockRegion(libOffset:_ULARGE_INTEGER;cb:_ULARGE_INTEGER;dwLockType:LongWord): HRESULT;stdcall;
    // Stat :
   function Stat(out pstatstg:tagSTATSTG;grfStatFlag:LongWord): HRESULT;stdcall;
    // Clone :
   function Clone(out ppstm:IStream): HRESULT;stdcall;
  end;


// ISpStreamFormat : ISpStreamFormat Interface

 ISpStreamFormat = interface(IStream)
   ['{BED530BE-2606-4F4D-A1C0-54C5CDA5566F}']
    // GetFormat :
   function GetFormat(var pguidFormatId:GUID;out ppCoMemWaveFormatEx:PWAVEFORMATEX): HRESULT;stdcall;
  end;


// ISpStreamFormatConverter : ISpStreamFormatConverter Interface

 ISpStreamFormatConverter = interface(ISpStreamFormat)
   ['{678A932C-EA71-4446-9B41-78FDA6280A29}']
    // SetBaseStream :
   function SetBaseStream(pStream:ISpStreamFormat;fSetFormatToBaseStreamFormat:Integer;fWriteToBaseStream:Integer): HRESULT;stdcall;
    // GetBaseStream :
   function GetBaseStream(out ppStream:ISpStreamFormat): HRESULT;stdcall;
    // SetFormat :
   function SetFormat(var rguidFormatIdOfConvertedStream:GUID;var pWaveFormatExOfConvertedStream:WAVEFORMATEX): HRESULT;stdcall;
    // ResetSeekPosition :
   function ResetSeekPosition: HRESULT;stdcall;
    // ScaleConvertedToBaseOffset :
   function ScaleConvertedToBaseOffset(ullOffsetConvertedStream:QWord;out pullOffsetBaseStream:QWord): HRESULT;stdcall;
    // ScaleBaseToConvertedOffset :
   function ScaleBaseToConvertedOffset(ullOffsetBaseStream:QWord;out pullOffsetConvertedStream:QWord): HRESULT;stdcall;
  end;


// ISpNotifySource : ISpNotifySource Interface

 ISpNotifySource = interface(IUnknown)
   ['{5EFF4AEF-8487-11D2-961C-00C04F8EE628}']
    // SetNotifySink :
   function SetNotifySink(pNotifySink:ISpNotifySink): HRESULT;stdcall;
    // SetNotifyWindowMessage :
   function SetNotifyWindowMessage(hWnd:wireHWND;Msg:UInt;wParam:UINT_PTR;lParam:LONG_PTR): HRESULT;stdcall;
    // SetNotifyCallbackFunction :
   function SetNotifyCallbackFunction(var pfnCallback:Ppointer;wParam:UINT_PTR;lParam:LONG_PTR): HRESULT;stdcall;
    // SetNotifyCallbackInterface :
   function SetNotifyCallbackInterface(var pSpCallback:Ppointer;wParam:UINT_PTR;lParam:LONG_PTR): HRESULT;stdcall;
    // SetNotifyWin32Event :
   function SetNotifyWin32Event: HRESULT;stdcall;
    // WaitForNotifyEvent :
   function WaitForNotifyEvent(dwMilliseconds:LongWord): HRESULT;stdcall;
    // GetNotifyEventHandle :
   function GetNotifyEventHandle: HRESULT;stdcall;
  end;


// ISpEventSource : ISpEventSource Interface

 ISpEventSource = interface(ISpNotifySource)
   ['{BE7A9CCE-5F9E-11D2-960F-00C04F8EE628}']
    // SetInterest :
   function SetInterest(ullEventInterest:QWord;ullQueuedInterest:QWord): HRESULT;stdcall;
    // GetEvents :
   function GetEvents(ulCount:LongWord;out pEventArray:SPEVENT;out pulFetched:LongWord): HRESULT;stdcall;
    // GetInfo :
   function GetInfo(out pInfo:SPEVENTSOURCEINFO): HRESULT;stdcall;
  end;


// ISpEventSink : ISpEventSink Interface

 ISpEventSink = interface(IUnknown)
   ['{BE7A9CC9-5F9E-11D2-960F-00C04F8EE628}']
    // AddEvents :
   function AddEvents(var pEventArray:SPEVENT;ulCount:LongWord): HRESULT;stdcall;
    // GetEventInterest :
   function GetEventInterest(out pullEventInterest:QWord): HRESULT;stdcall;
  end;


// ISpObjectWithToken : ISpObjectWithToken Interface

 ISpObjectWithToken = interface(IUnknown)
   ['{5B559F40-E952-11D2-BB91-00C04F8EE6C0}']
    // SetObjectToken :
   function SetObjectToken(pToken:ISpObjectToken): HRESULT;stdcall;
    // GetObjectToken :
   function GetObjectToken(out ppToken:ISpObjectToken): HRESULT;stdcall;
  end;


// ISpAudio : ISpAudio Interface

 ISpAudio = interface(ISpStreamFormat)
   ['{C05C768F-FAE8-4EC2-8E07-338321C12452}']
    // SetState :
   function SetState(NewState:SPAUDIOSTATE;ullReserved:QWord): HRESULT;stdcall;
    // SetFormat :
   function SetFormat(var rguidFmtId:GUID;var pWaveFormatEx:WAVEFORMATEX): HRESULT;stdcall;
    // GetStatus :
   function GetStatus(out pStatus:SPAUDIOSTATUS): HRESULT;stdcall;
    // SetBufferInfo :
   function SetBufferInfo(var pBuffInfo:SPAUDIOBUFFERINFO): HRESULT;stdcall;
    // GetBufferInfo :
   function GetBufferInfo(out pBuffInfo:SPAUDIOBUFFERINFO): HRESULT;stdcall;
    // GetDefaultFormat :
   function GetDefaultFormat(out pFormatId:GUID;out ppCoMemWaveFormatEx:PWAVEFORMATEX): HRESULT;stdcall;
    // EventHandle :
   function EventHandle: HRESULT;stdcall;
    // GetVolumeLevel :
   function GetVolumeLevel(out pLevel:LongWord): HRESULT;stdcall;
    // SetVolumeLevel :
   function SetVolumeLevel(Level:LongWord): HRESULT;stdcall;
    // GetBufferNotifySize :
   function GetBufferNotifySize(out pcbSize:LongWord): HRESULT;stdcall;
    // SetBufferNotifySize :
   function SetBufferNotifySize(cbSize:LongWord): HRESULT;stdcall;
  end;


// ISpMMSysAudio : ISpMMSysAudio Interface

 ISpMMSysAudio = interface(ISpAudio)
   ['{15806F6E-1D70-4B48-98E6-3B1A007509AB}']
    // GetDeviceId :
   function GetDeviceId(out puDeviceId:UInt): HRESULT;stdcall;
    // SetDeviceId :
   function SetDeviceId(uDeviceId:UInt): HRESULT;stdcall;
    // GetMMHandle :
   function GetMMHandle(out pHandle:Ppointer): HRESULT;stdcall;
    // GetLineId :
   function GetLineId(out puLineId:UInt): HRESULT;stdcall;
    // SetLineId :
   function SetLineId(uLineId:UInt): HRESULT;stdcall;
  end;


// ISpStream : ISpStream Interface

 ISpStream = interface(ISpStreamFormat)
   ['{12E3CCA9-7518-44C5-A5E7-BA5A79CB929E}']
    // SetBaseStream :
   function SetBaseStream(pStream:IStream;var rguidFormat:GUID;var pWaveFormatEx:WAVEFORMATEX): HRESULT;stdcall;
    // GetBaseStream :
   function GetBaseStream(out ppStream:IStream): HRESULT;stdcall;
    // BindToFile :
   function BindToFile(pszFileName:PWideChar;eMode:SPFILEMODE;var pFormatId:GUID;var pWaveFormatEx:WAVEFORMATEX;ullEventInterest:QWord): HRESULT;stdcall;
    // Close :
   function Close: HRESULT;stdcall;
  end;


// ISpVoice : ISpVoice Interface

 ISpVoice = interface(ISpEventSource)
   ['{6C44DF74-72B9-4992-A1EC-EF996E0422D4}']
    // SetOutput :
   function SetOutput(pUnkOutput:IUnknown;fAllowFormatChanges:Integer): HRESULT;stdcall;
    // GetOutputObjectToken :
   function GetOutputObjectToken(out ppObjectToken:ISpObjectToken): HRESULT;stdcall;
    // GetOutputStream :
   function GetOutputStream(out ppStream:ISpStreamFormat): HRESULT;stdcall;
    // Pause :
   function Pause: HRESULT;stdcall;
    // Resume :
   function Resume: HRESULT;stdcall;
    // SetVoice :
   function SetVoice(pToken:ISpObjectToken): HRESULT;stdcall;
    // GetVoice :
   function GetVoice(out ppToken:ISpObjectToken): HRESULT;stdcall;
    // Speak :
   function Speak(pwcs:PWideChar;dwFlags:LongWord;out pulStreamNumber:LongWord): HRESULT;stdcall;
    // SpeakStream :
   function SpeakStream(pStream:IStream;dwFlags:LongWord;out pulStreamNumber:LongWord): HRESULT;stdcall;
    // GetStatus :
   function GetStatus(out pStatus:SPVOICESTATUS;out ppszLastBookmark:PWideChar): HRESULT;stdcall;
    // Skip :
   function Skip(pItemType:PWideChar;lNumItems:Integer;out pulNumSkipped:LongWord): HRESULT;stdcall;
    // SetPriority :
   function SetPriority(ePriority:SPVPRIORITY): HRESULT;stdcall;
    // GetPriority :
   function GetPriority(out pePriority:SPVPRIORITY): HRESULT;stdcall;
    // SetAlertBoundary :
   function SetAlertBoundary(eBoundary:SPEVENTENUM): HRESULT;stdcall;
    // GetAlertBoundary :
   function GetAlertBoundary(out peBoundary:SPEVENTENUM): HRESULT;stdcall;
    // SetRate :
   function SetRate(RateAdjust:Integer): HRESULT;stdcall;
    // GetRate :
   function GetRate(out pRateAdjust:Integer): HRESULT;stdcall;
    // SetVolume :
   function SetVolume(usVolume:Word): HRESULT;stdcall;
    // GetVolume :
   function GetVolume(out pusVolume:Word): HRESULT;stdcall;
    // WaitUntilDone :
   function WaitUntilDone(msTimeout:LongWord): HRESULT;stdcall;
    // SetSyncSpeakTimeout :
   function SetSyncSpeakTimeout(msTimeout:LongWord): HRESULT;stdcall;
    // GetSyncSpeakTimeout :
   function GetSyncSpeakTimeout(out pmsTimeout:LongWord): HRESULT;stdcall;
    // SpeakCompleteEvent :
   function SpeakCompleteEvent: HRESULT;stdcall;
    // IsUISupported :
   function IsUISupported(pszTypeOfUI:PWideChar;var pvExtraData:pointer;cbExtraData:LongWord;out pfSupported:Integer): HRESULT;stdcall;
    // DisplayUI :
   function DisplayUI(hWndParent:wireHWND;pszTitle:PWideChar;pszTypeOfUI:PWideChar;var pvExtraData:pointer;cbExtraData:LongWord): HRESULT;stdcall;
  end;


// ISpPhoneticAlphabetSelection : ISpPhoneticAlphabetSelection Interface

 ISpPhoneticAlphabetSelection = interface(IUnknown)
   ['{B2745EFD-42CE-48CA-81F1-A96E02538A90}']
    // IsAlphabetUPS :
   function IsAlphabetUPS(out pfIsUPS:Integer): HRESULT;stdcall;
    // SetAlphabetToUPS :
   function SetAlphabetToUPS(fForceUPS:Integer): HRESULT;stdcall;
  end;


// ISpRecoContext : ISpRecoContext Interface

 ISpRecoContext = interface(ISpEventSource)
   ['{F740A62F-7C15-489E-8234-940A33D9272D}']
    // GetRecognizer :
   function GetRecognizer(out ppRecognizer:ISpRecognizer): HRESULT;stdcall;
    // CreateGrammar :
   function CreateGrammar(ullGrammarID:QWord;out ppGrammar:ISpRecoGrammar): HRESULT;stdcall;
    // GetStatus :
   function GetStatus(out pStatus:SPRECOCONTEXTSTATUS): HRESULT;stdcall;
    // GetMaxAlternates :
   function GetMaxAlternates(var pcAlternates:LongWord): HRESULT;stdcall;
    // SetMaxAlternates :
   function SetMaxAlternates(cAlternates:LongWord): HRESULT;stdcall;
    // SetAudioOptions :
   function SetAudioOptions(Options:SPAUDIOOPTIONS;var pAudioFormatId:GUID;var pWaveFormatEx:WAVEFORMATEX): HRESULT;stdcall;
    // GetAudioOptions :
   function GetAudioOptions(var pOptions:SPAUDIOOPTIONS;out pAudioFormatId:GUID;out ppCoMemWFEX:PWAVEFORMATEX): HRESULT;stdcall;
    // DeserializeResult :
   function DeserializeResult(var pSerializedResult:SPSERIALIZEDRESULT;out ppResult:ISpRecoResult): HRESULT;stdcall;
    // Bookmark :
   function Bookmark(Options:SPBOOKMARKOPTIONS;ullStreamPosition:QWord;lparamEvent:LONG_PTR): HRESULT;stdcall;
    // SetAdaptationData :
   function SetAdaptationData(pAdaptationData:PWideChar;cch:LongWord): HRESULT;stdcall;
    // Pause :
   function Pause(dwReserved:LongWord): HRESULT;stdcall;
    // Resume :
   function Resume(dwReserved:LongWord): HRESULT;stdcall;
    // SetVoice :
   function SetVoice(pVoice:ISpVoice;fAllowFormatChanges:Integer): HRESULT;stdcall;
    // GetVoice :
   function GetVoice(out ppVoice:ISpVoice): HRESULT;stdcall;
    // SetVoicePurgeEvent :
   function SetVoicePurgeEvent(ullEventInterest:QWord): HRESULT;stdcall;
    // GetVoicePurgeEvent :
   function GetVoicePurgeEvent(out pullEventInterest:QWord): HRESULT;stdcall;
    // SetContextState :
   function SetContextState(eContextState:SPCONTEXTSTATE): HRESULT;stdcall;
    // GetContextState :
   function GetContextState(out peContextState:SPCONTEXTSTATE): HRESULT;stdcall;
  end;


// ISpProperties : ISpProperties Interface

 ISpProperties = interface(IUnknown)
   ['{5B4FB971-B115-4DE1-AD97-E482E3BF6EE4}']
    // SetPropertyNum :
   function SetPropertyNum(pName:PWideChar;lValue:Integer): HRESULT;stdcall;
    // GetPropertyNum :
   function GetPropertyNum(pName:PWideChar;out plValue:Integer): HRESULT;stdcall;
    // SetPropertyString :
   function SetPropertyString(pName:PWideChar;pValue:PWideChar): HRESULT;stdcall;
    // GetPropertyString :
   function GetPropertyString(pName:PWideChar;out ppCoMemValue:PWideChar): HRESULT;stdcall;
  end;


// ISpRecognizer : ISpRecognizer Interface

 ISpRecognizer = interface(ISpProperties)
   ['{C2B5F241-DAA0-4507-9E16-5A1EAA2B7A5C}']
    // SetRecognizer :
   function SetRecognizer(pRecognizer:ISpObjectToken): HRESULT;stdcall;
    // GetRecognizer :
   function GetRecognizer(out ppRecognizer:ISpObjectToken): HRESULT;stdcall;
    // SetInput :
   function SetInput(pUnkInput:IUnknown;fAllowFormatChanges:Integer): HRESULT;stdcall;
    // GetInputObjectToken :
   function GetInputObjectToken(out ppToken:ISpObjectToken): HRESULT;stdcall;
    // GetInputStream :
   function GetInputStream(out ppStream:ISpStreamFormat): HRESULT;stdcall;
    // CreateRecoContext :
   function CreateRecoContext(out ppNewCtxt:ISpRecoContext): HRESULT;stdcall;
    // GetRecoProfile :
   function GetRecoProfile(out ppToken:ISpObjectToken): HRESULT;stdcall;
    // SetRecoProfile :
   function SetRecoProfile(pToken:ISpObjectToken): HRESULT;stdcall;
    // IsSharedInstance :
   function IsSharedInstance: HRESULT;stdcall;
    // GetRecoState :
   function GetRecoState(out pState:SPRECOSTATE): HRESULT;stdcall;
    // SetRecoState :
   function SetRecoState(NewState:SPRECOSTATE): HRESULT;stdcall;
    // GetStatus :
   function GetStatus(out pStatus:SPRECOGNIZERSTATUS): HRESULT;stdcall;
    // GetFormat :
   function GetFormat(WaveFormatType:SPSTREAMFORMATTYPE;out pFormatId:GUID;out ppCoMemWFEX:PWAVEFORMATEX): HRESULT;stdcall;
    // IsUISupported :
   function IsUISupported(pszTypeOfUI:PWideChar;var pvExtraData:pointer;cbExtraData:LongWord;out pfSupported:Integer): HRESULT;stdcall;
    // DisplayUI :
   function DisplayUI(hWndParent:wireHWND;pszTitle:PWideChar;pszTypeOfUI:PWideChar;var pvExtraData:pointer;cbExtraData:LongWord): HRESULT;stdcall;
    // EmulateRecognition :
   function EmulateRecognition(pPhrase:ISpPhrase): HRESULT;stdcall;
  end;


// ISpPhrase : ISpPhrase Interface

 ISpPhrase = interface(IUnknown)
   ['{1A5C0354-B621-4B5A-8791-D306ED379E53}']
    // GetPhrase :
   function GetPhrase(out ppCoMemPhrase:PSPPHRASE): HRESULT;stdcall;
    // GetSerializedPhrase :
   function GetSerializedPhrase(out ppCoMemPhrase:PSPSERIALIZEDPHRASE): HRESULT;stdcall;
    // GetText :
   function GetText(ulStart:LongWord;ulCount:LongWord;fUseTextReplacements:Integer;out ppszCoMemText:PWideChar;out pbDisplayAttributes:Byte): HRESULT;stdcall;
    // Discard :
   function Discard(dwValueTypes:LongWord): HRESULT;stdcall;
  end;


// ISpGrammarBuilder : ISpGrammarBuilder Interface

 ISpGrammarBuilder = interface(IUnknown)
   ['{8137828F-591A-4A42-BE58-49EA7EBAAC68}']
    // ResetGrammar :
   function ResetGrammar(NewLanguage:Word): HRESULT;stdcall;
    // GetRule :
   function GetRule(pszRuleName:PWideChar;dwRuleId:LongWord;dwAttributes:LongWord;fCreateIfNotExist:Integer;out phInitialState:Ppointer): HRESULT;stdcall;
    // ClearRule :
   function ClearRule(var hState:pointer): HRESULT;stdcall;
    // CreateNewState :
   function CreateNewState(var hState:pointer;out phState:Ppointer): HRESULT;stdcall;
    // AddWordTransition :
   function AddWordTransition(var hFromState:pointer;var hToState:pointer;psz:PWideChar;pszSeparators:PWideChar;eWordType:SPGRAMMARWORDTYPE;Weight:Single;var pPropInfo:SPPROPERTYINFO): HRESULT;stdcall;
    // AddRuleTransition :
   function AddRuleTransition(var hFromState:pointer;var hToState:pointer;var hRule:pointer;Weight:Single;var pPropInfo:SPPROPERTYINFO): HRESULT;stdcall;
    // AddResource :
   function AddResource(var hRuleState:pointer;pszResourceName:PWideChar;pszResourceValue:PWideChar): HRESULT;stdcall;
    // Commit :
   function Commit(dwReserved:LongWord): HRESULT;stdcall;
  end;


// ISpRecoGrammar : ISpRecoGrammar Interface

 ISpRecoGrammar = interface(ISpGrammarBuilder)
   ['{2177DB29-7F45-47D0-8554-067E91C80502}']
    // GetGrammarId :
   function GetGrammarId(out pullGrammarId:QWord): HRESULT;stdcall;
    // GetRecoContext :
   function GetRecoContext(out ppRecoCtxt:ISpRecoContext): HRESULT;stdcall;
    // LoadCmdFromFile :
   function LoadCmdFromFile(pszFileName:PWideChar;Options:SPLOADOPTIONS): HRESULT;stdcall;
    // LoadCmdFromObject :
   function LoadCmdFromObject(var rcid:GUID;pszGrammarName:PWideChar;Options:SPLOADOPTIONS): HRESULT;stdcall;
    // LoadCmdFromResource :
   function LoadCmdFromResource(var hModule:pointer;pszResourceName:PWideChar;pszResourceType:PWideChar;wLanguage:Word;Options:SPLOADOPTIONS): HRESULT;stdcall;
    // LoadCmdFromMemory :
   function LoadCmdFromMemory(var pGrammar:SPBINARYGRAMMAR;Options:SPLOADOPTIONS): HRESULT;stdcall;
    // LoadCmdFromProprietaryGrammar :
   function LoadCmdFromProprietaryGrammar(var rguidParam:GUID;pszStringParam:PWideChar;var pvDataPrarm:pointer;cbDataSize:LongWord;Options:SPLOADOPTIONS): HRESULT;stdcall;
    // SetRuleState :
   function SetRuleState(pszName:PWideChar;var pReserved:pointer;NewState:SPRULESTATE): HRESULT;stdcall;
    // SetRuleIdState :
   function SetRuleIdState(ulRuleId:LongWord;NewState:SPRULESTATE): HRESULT;stdcall;
    // LoadDictation :
   function LoadDictation(pszTopicName:PWideChar;Options:SPLOADOPTIONS): HRESULT;stdcall;
    // UnloadDictation :
   function UnloadDictation: HRESULT;stdcall;
    // SetDictationState :
   function SetDictationState(NewState:SPRULESTATE): HRESULT;stdcall;
    // SetWordSequenceData :
   function SetWordSequenceData(var pText:Word;cchText:LongWord;var pInfo:SPTEXTSELECTIONINFO): HRESULT;stdcall;
    // SetTextSelection :
   function SetTextSelection(var pInfo:SPTEXTSELECTIONINFO): HRESULT;stdcall;
    // IsPronounceable :
   function IsPronounceable(pszWord:PWideChar;out pWordPronounceable:SPWORDPRONOUNCEABLE): HRESULT;stdcall;
    // SetGrammarState :
   function SetGrammarState(eGrammarState:SPGRAMMARSTATE): HRESULT;stdcall;
    // SaveCmd :
   function SaveCmd(pStream:IStream;out ppszCoMemErrorText:PWideChar): HRESULT;stdcall;
    // GetGrammarState :
   function GetGrammarState(out peGrammarState:SPGRAMMARSTATE): HRESULT;stdcall;
  end;


// ISpRecoResult : ISpRecoResult Interface

 ISpRecoResult = interface(ISpPhrase)
   ['{20B053BE-E235-43CD-9A2A-8D17A48B7842}']
    // GetResultTimes :
   function GetResultTimes(out pTimes:SPRECORESULTTIMES): HRESULT;stdcall;
    // GetAlternates :
   function GetAlternates(ulStartElement:LongWord;cElements:LongWord;ulRequestCount:LongWord;out ppPhrases:ISpPhraseAlt;out pcPhrasesReturned:LongWord): HRESULT;stdcall;
    // GetAudio :
   function GetAudio(ulStartElement:LongWord;cElements:LongWord;out ppStream:ISpStreamFormat): HRESULT;stdcall;
    // SpeakAudio :
   function SpeakAudio(ulStartElement:LongWord;cElements:LongWord;dwFlags:LongWord;out pulStreamNumber:LongWord): HRESULT;stdcall;
    // Serialize :
   function Serialize(out ppCoMemSerializedResult:PSPSERIALIZEDRESULT): HRESULT;stdcall;
    // ScaleAudio :
   function ScaleAudio(var pAudioFormatId:GUID;var pWaveFormatEx:WAVEFORMATEX): HRESULT;stdcall;
    // GetRecoContext :
   function GetRecoContext(out ppRecoContext:ISpRecoContext): HRESULT;stdcall;
  end;


// ISpPhraseAlt : ISpPhraseAlt Interface

 ISpPhraseAlt = interface(ISpPhrase)
   ['{8FCEBC98-4E49-4067-9C6C-D86A0E092E3D}']
    // GetAltInfo :
   function GetAltInfo(out ppParent:ISpPhrase;out pulStartElementInParent:LongWord;out pcElementsInParent:LongWord;out pcElementsInAlt:LongWord): HRESULT;stdcall;
    // Commit :
   function Commit: HRESULT;stdcall;
  end;


// ISpRecoContext2 : ISpRecoContext2 Interface

 ISpRecoContext2 = interface(IUnknown)
   ['{BEAD311C-52FF-437F-9464-6B21054CA73D}']
    // SetGrammarOptions :
   function SetGrammarOptions(eGrammarOptions:LongWord): HRESULT;stdcall;
    // GetGrammarOptions :
   function GetGrammarOptions(out peGrammarOptions:LongWord): HRESULT;stdcall;
    // SetAdaptationData2 :
   function SetAdaptationData2(pAdaptationData:PWideChar;cch:LongWord;pTopicName:PWideChar;eAdaptationSettings:LongWord;eRelevance:SPADAPTATIONRELEVANCE): HRESULT;stdcall;
  end;


// ISpRecognizer2 : ISpRecognizer2 Interface

 ISpRecognizer2 = interface(IUnknown)
   ['{8FC6D974-C81E-4098-93C5-0147F61ED4D3}']
    // EmulateRecognitionEx :
   function EmulateRecognitionEx(pPhrase:ISpPhrase;dwCompareFlags:LongWord): HRESULT;stdcall;
    // SetTrainingState :
   function SetTrainingState(fDoingTraining:Integer;fAdaptFromTrainingData:Integer): HRESULT;stdcall;
    // ResetAcousticModelAdaptation :
   function ResetAcousticModelAdaptation: HRESULT;stdcall;
  end;


// ISpRecognizer3 : ISpRecognizer3 Interface

 ISpRecognizer3 = interface(IUnknown)
   ['{DF1B943C-5838-4AA2-8706-D7CD5B333499}']
    // GetCategory :
   function GetCategory(categoryType:SPCATEGORYTYPE;out ppCategory:ISpRecoCategory): HRESULT;stdcall;
    // SetActiveCategory :
   function SetActiveCategory(pCategory:ISpRecoCategory): HRESULT;stdcall;
    // GetActiveCategory :
   function GetActiveCategory(out ppCategory:ISpRecoCategory): HRESULT;stdcall;
  end;


// ISpRecoCategory : ISpRecoCategory Interface

 ISpRecoCategory = interface(IUnknown)
   ['{DA0CD0F9-14A2-4F09-8C2A-85CC48979345}']
    // GetType :
   function GetType(out peCategoryType:SPCATEGORYTYPE): HRESULT;stdcall;
  end;


// ISpSerializeState : ISpSerializeState Interface

 ISpSerializeState = interface(IUnknown)
   ['{21B501A0-0EC7-46C9-92C3-A2BC784C54B9}']
    // GetSerializedState :
   function GetSerializedState(out ppbData:PByte;out pulSize:LongWord;dwReserved:LongWord): HRESULT;stdcall;
    // SetSerializedState :
   function SetSerializedState(var pbData:Byte;ulSize:LongWord;dwReserved:LongWord): HRESULT;stdcall;
  end;


// ISpLexicon : ISpLexicon Interface

 ISpLexicon = interface(IUnknown)
   ['{DA41A7C2-5383-4DB2-916B-6C1719E3DB58}']
    // GetPronunciations :
   function GetPronunciations(pszWord:PWideChar;LangId:Word;dwFlags:LongWord;var pWordPronunciationList:SPWORDPRONUNCIATIONLIST): HRESULT;stdcall;
    // AddPronunciation :
   function AddPronunciation(pszWord:PWideChar;LangId:Word;ePartOfSpeech:SPPARTOFSPEECH;pszPronunciation:PWideChar): HRESULT;stdcall;
    // RemovePronunciation :
   function RemovePronunciation(pszWord:PWideChar;LangId:Word;ePartOfSpeech:SPPARTOFSPEECH;pszPronunciation:PWideChar): HRESULT;stdcall;
    // GetGeneration :
   function GetGeneration(out pdwGeneration:LongWord): HRESULT;stdcall;
    // GetGenerationChange :
   function GetGenerationChange(dwFlags:LongWord;var pdwGeneration:LongWord;var pWordList:SPWORDLIST): HRESULT;stdcall;
    // GetWords :
   function GetWords(dwFlags:LongWord;var pdwGeneration:LongWord;var pdwCookie:LongWord;var pWordList:SPWORDLIST): HRESULT;stdcall;
  end;


// ISpShortcut : ISpShortcut Interface

 ISpShortcut = interface(IUnknown)
   ['{3DF681E2-EA56-11D9-8BDE-F66BAD1E3F3A}']
    // AddShortcut :
   function AddShortcut(pszDisplay:PWideChar;LangId:Word;pszSpoken:PWideChar;shType:SPSHORTCUTTYPE): HRESULT;stdcall;
    // RemoveShortcut :
   function RemoveShortcut(pszDisplay:PWideChar;LangId:Word;pszSpoken:PWideChar;shType:SPSHORTCUTTYPE): HRESULT;stdcall;
    // GetShortcuts :
   function GetShortcuts(LangId:Word;var pShortcutpairList:SPSHORTCUTPAIRLIST): HRESULT;stdcall;
    // GetGeneration :
   function GetGeneration(out pdwGeneration:LongWord): HRESULT;stdcall;
    // GetWordsFromGenerationChange :
   function GetWordsFromGenerationChange(var pdwGeneration:LongWord;var pWordList:SPWORDLIST): HRESULT;stdcall;
    // GetWords :
   function GetWords(var pdwGeneration:LongWord;var pdwCookie:LongWord;var pWordList:SPWORDLIST): HRESULT;stdcall;
    // GetShortcutsForGeneration :
   function GetShortcutsForGeneration(var pdwGeneration:LongWord;var pdwCookie:LongWord;var pShortcutpairList:SPSHORTCUTPAIRLIST): HRESULT;stdcall;
    // GetGenerationChange :
   function GetGenerationChange(var pdwGeneration:LongWord;var pShortcutpairList:SPSHORTCUTPAIRLIST): HRESULT;stdcall;
  end;


// ISpPhoneConverter : ISpPhoneConverter Interface

 ISpPhoneConverter = interface(ISpObjectWithToken)
   ['{8445C581-0CAC-4A38-ABFE-9B2CE2826455}']
    // PhoneToId :
   function PhoneToId(pszPhone:PWideChar;out pId:Word): HRESULT;stdcall;
    // IdToPhone :
   function IdToPhone(pId:PWideChar;out pszPhone:Word): HRESULT;stdcall;
  end;


// ISpPhoneticAlphabetConverter : ISpPhoneticAlphabetConverter Interface

 ISpPhoneticAlphabetConverter = interface(IUnknown)
   ['{133ADCD4-19B4-4020-9FDC-842E78253B17}']
    // GetLangId :
   function GetLangId(out pLangID:Word): HRESULT;stdcall;
    // SetLangId :
   function SetLangId(LangId:Word): HRESULT;stdcall;
    // SAPI2UPS :
   function SAPI2UPS(var pszSAPIId:Word;out pszUPSId:Word;cMaxLength:LongWord): HRESULT;stdcall;
    // UPS2SAPI :
   function UPS2SAPI(var pszUPSId:Word;out pszSAPIId:Word;cMaxLength:LongWord): HRESULT;stdcall;
    // GetMaxConvertLength :
   function GetMaxConvertLength(cSrcLength:LongWord;bSAPI2UPS:Integer;out pcMaxDestLength:LongWord): HRESULT;stdcall;
  end;


// ISpXMLRecoResult : ISpXMLRecoResult Interface

 ISpXMLRecoResult = interface(ISpRecoResult)
   ['{AE39362B-45A8-4074-9B9E-CCF49AA2D0B6}']
    // GetXMLResult :
   function GetXMLResult(out ppszCoMemXMLResult:PWideChar;Options:SPXMLRESULTOPTIONS): HRESULT;stdcall;
    // GetXMLErrorInfo :
   function GetXMLErrorInfo(out pSemanticErrorInfo:SPSEMANTICERRORINFO): HRESULT;stdcall;
  end;


// ISpRecoGrammar2 : ISpRecoGrammar2 Interface

 ISpRecoGrammar2 = interface(IUnknown)
   ['{4B37BC9E-9ED6-44A3-93D3-18F022B79EC3}']
    // GetRules :
   function GetRules(out ppCoMemRules:PSPRULE;out puNumRules:UInt): HRESULT;stdcall;
    // LoadCmdFromFile2 :
   function LoadCmdFromFile2(pszFileName:PWideChar;Options:SPLOADOPTIONS;pszSharingUri:PWideChar;pszBaseUri:PWideChar): HRESULT;stdcall;
    // LoadCmdFromMemory2 :
   function LoadCmdFromMemory2(var pGrammar:SPBINARYGRAMMAR;Options:SPLOADOPTIONS;pszSharingUri:PWideChar;pszBaseUri:PWideChar): HRESULT;stdcall;
    // SetRulePriority :
   function SetRulePriority(pszRuleName:PWideChar;ulRuleId:LongWord;nRulePriority:SYSINT): HRESULT;stdcall;
    // SetRuleWeight :
   function SetRuleWeight(pszRuleName:PWideChar;ulRuleId:LongWord;flWeight:Single): HRESULT;stdcall;
    // SetDictationWeight :
   function SetDictationWeight(flWeight:Single): HRESULT;stdcall;
    // SetGrammarLoader :
   function SetGrammarLoader(pLoader:ISpeechResourceLoader): HRESULT;stdcall;
    // SetSMLSecurityManager :
   function SetSMLSecurityManager(pSMLSecurityManager:IInternetSecurityManager): HRESULT;stdcall;
  end;


// ISpeechResourceLoader : ISpeechResourceLoader Interface

 ISpeechResourceLoader = interface(IDispatch)
   ['{B9AC5783-FCD0-4B21-B119-B4F8DA8FD2C3}']
    // LoadResource :
   procedure LoadResource(bstrResourceUri:WideString;fAlwaysReload:WordBool;out pStream:IUnknown;out pbstrMIMEType:WideString;out pfModified:WordBool;out pbstrRedirectUrl:WideString);safecall;
    // GetLocalCopy :
   procedure GetLocalCopy(bstrResourceUri:WideString;out pbstrLocalPath:WideString;out pbstrMIMEType:WideString;out pbstrRedirectUrl:WideString);safecall;
    // ReleaseLocalCopy :
   procedure ReleaseLocalCopy(pbstrLocalPath:WideString);safecall;
  end;


// ISpeechResourceLoader : ISpeechResourceLoader Interface

 ISpeechResourceLoaderDisp = dispinterface
   ['{B9AC5783-FCD0-4B21-B119-B4F8DA8FD2C3}']
    // QueryInterface :
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :
   function AddRef: LongWord;dispid 1610612737;
    // Release :
   function Release: LongWord;dispid 1610612738;
    // GetTypeInfoCount :
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // LoadResource :
   procedure LoadResource(bstrResourceUri:WideString;fAlwaysReload:WordBool;out pStream:IUnknown;out pbstrMIMEType:WideString;out pfModified:WordBool;out pbstrRedirectUrl:WideString);dispid 1;
    // GetLocalCopy :
   procedure GetLocalCopy(bstrResourceUri:WideString;out pbstrLocalPath:WideString;out pbstrMIMEType:WideString;out pbstrRedirectUrl:WideString);dispid 2;
    // ReleaseLocalCopy :
   procedure ReleaseLocalCopy(pbstrLocalPath:WideString);dispid 3;
  end;


// IInternetSecurityManager : IInternetSecurityManager Interface

 IInternetSecurityManager = interface(IUnknown)
   ['{79EAC9EE-BAF9-11CE-8C82-00AA004BA90B}']
    // SetSecuritySite :
   function SetSecuritySite(pSite:IInternetSecurityMgrSite): HRESULT;stdcall;
    // GetSecuritySite :
   function GetSecuritySite(out ppSite:IInternetSecurityMgrSite): HRESULT;stdcall;
    // MapUrlToZone :
   function MapUrlToZone(pwszUrl:PWideChar;out pdwZone:LongWord;dwFlags:LongWord): HRESULT;stdcall;
    // GetSecurityId :
   function GetSecurityId(pwszUrl:PWideChar;out pbSecurityId:Byte;var pcbSecurityId:LongWord;dwReserved:ULONG_PTR): HRESULT;stdcall;
    // ProcessUrlAction :
   function ProcessUrlAction(pwszUrl:PWideChar;dwAction:LongWord;out pPolicy:Byte;cbPolicy:LongWord;var pContext:Byte;cbContext:LongWord;dwFlags:LongWord;dwReserved:LongWord): HRESULT;stdcall;
    // QueryCustomPolicy :
   function QueryCustomPolicy(pwszUrl:PWideChar;var guidKey:GUID;out ppPolicy:PByte;out pcbPolicy:LongWord;var pContext:Byte;cbContext:LongWord;dwReserved:LongWord): HRESULT;stdcall;
    // SetZoneMapping :
   function SetZoneMapping(dwZone:LongWord;lpszPattern:PWideChar;dwFlags:LongWord): HRESULT;stdcall;
    // GetZoneMappings :
   function GetZoneMappings(dwZone:LongWord;out ppenumString:IEnumString;dwFlags:LongWord): HRESULT;stdcall;
  end;


// IInternetSecurityMgrSite : IInternetSecurityMgrSite Interface

 IInternetSecurityMgrSite = interface(IUnknown)
   ['{79EAC9ED-BAF9-11CE-8C82-00AA004BA90B}']
    // GetWindow :
   function GetWindow(out phwnd:wireHWND): HRESULT;stdcall;
    // EnableModeless :
   function EnableModeless(fEnable:Integer): HRESULT;stdcall;
  end;


// IEnumString :

 IEnumString = interface(IUnknown)
   ['{00000101-0000-0000-C000-000000000046}']
    // RemoteNext :
   function RemoteNext(celt:LongWord;out rgelt:PWideChar;out pceltFetched:LongWord): HRESULT;stdcall;
    // Skip :
   function Skip(celt:LongWord): HRESULT;stdcall;
    // Reset_ :
   function Reset_: HRESULT;stdcall;
    // Clone :
   function Clone(out ppEnum:IEnumString): HRESULT;stdcall;
  end;

//CoClasses
  CoSpNotifyTranslator = Class
  Public
    Class Function Create: ISpNotifyTranslator;
    Class Function CreateRemote(const MachineName: string): ISpNotifyTranslator;
  end;

  CoSpObjectTokenCategory = Class
  Public
    Class Function Create: ISpeechObjectTokenCategory;
    Class Function CreateRemote(const MachineName: string): ISpeechObjectTokenCategory;
  end;

  TAxcSpObjectTokenCategory = Class(TActiveXContainer)
  Private
    FServer:ISpeechObjectTokenCategory;
  Public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property OleServer:ISpeechObjectTokenCategory read FServer;
  Published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnStatusText;
    property OnUnDock;
    property Active;
  end;

  CoSpObjectToken = Class
  Public
    Class Function Create: ISpeechObjectToken;
    Class Function CreateRemote(const MachineName: string): ISpeechObjectToken;
  end;

  TAxcSpObjectToken = Class(TActiveXContainer)
  Private
    FServer:ISpeechObjectToken;
  Public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property OleServer:ISpeechObjectToken read FServer;
  Published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnStatusText;
    property OnUnDock;
    property Active;
  end;

  CoSpResourceManager = Class
  Public
    Class Function Create: ISpResourceManager;
    Class Function CreateRemote(const MachineName: string): ISpResourceManager;
  end;

  CoSpStreamFormatConverter = Class
  Public
    Class Function Create: ISpStreamFormatConverter;
    Class Function CreateRemote(const MachineName: string): ISpStreamFormatConverter;
  end;

  CoSpMMAudioEnum = Class
  Public
    Class Function Create: IEnumSpObjectTokens;
    Class Function CreateRemote(const MachineName: string): IEnumSpObjectTokens;
  end;

  CoSpMMAudioIn = Class
  Public
    Class Function Create: ISpeechMMSysAudio;
    Class Function CreateRemote(const MachineName: string): ISpeechMMSysAudio;
  end;

  TAxcSpMMAudioIn = Class(TActiveXContainer)
  Private
    FServer:ISpeechMMSysAudio;
  Public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property OleServer:ISpeechMMSysAudio read FServer;
  Published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnStatusText;
    property OnUnDock;
    property Active;
  end;

  CoSpMMAudioOut = Class
  Public
    Class Function Create: ISpeechMMSysAudio;
    Class Function CreateRemote(const MachineName: string): ISpeechMMSysAudio;
  end;

  TAxcSpMMAudioOut = Class(TActiveXContainer)
  Private
    FServer:ISpeechMMSysAudio;
  Public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property OleServer:ISpeechMMSysAudio read FServer;
  Published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnStatusText;
    property OnUnDock;
    property Active;
  end;

  CoSpStream = Class
  Public
    Class Function Create: ISpStream;
    Class Function CreateRemote(const MachineName: string): ISpStream;
  end;

  T_ISpeechVoiceEventsStartStream = procedure(Sender: TObject;StreamNumber:Integer;StreamPosition:OleVariant) of object;
  T_ISpeechVoiceEventsEndStream = procedure(Sender: TObject;StreamNumber:Integer;StreamPosition:OleVariant) of object;
  T_ISpeechVoiceEventsVoiceChange = procedure(Sender: TObject;StreamNumber:Integer;StreamPosition:OleVariant;VoiceObjectToken:ISpeechObjectToken) of object;
  T_ISpeechVoiceEventsBookmark = procedure(Sender: TObject;StreamNumber:Integer;StreamPosition:OleVariant;Bookmark:WideString;BookmarkId:Integer) of object;
  T_ISpeechVoiceEventsWord = procedure(Sender: TObject;StreamNumber:Integer;StreamPosition:OleVariant;CharacterPosition:Integer;Length:Integer) of object;
  T_ISpeechVoiceEventsSentence = procedure(Sender: TObject;StreamNumber:Integer;StreamPosition:OleVariant;CharacterPosition:Integer;Length:Integer) of object;
  T_ISpeechVoiceEventsPhoneme = procedure(Sender: TObject;StreamNumber:Integer;StreamPosition:OleVariant;Duration:Integer;NextPhoneId:Smallint;Feature:SpeechVisemeFeature;CurrentPhoneId:Smallint) of object;
  T_ISpeechVoiceEventsViseme = procedure(Sender: TObject;StreamNumber:Integer;StreamPosition:OleVariant;Duration:Integer;NextVisemeId:SpeechVisemeType;Feature:SpeechVisemeFeature;CurrentVisemeId:SpeechVisemeType) of object;
  T_ISpeechVoiceEventsAudioLevel = procedure(Sender: TObject;StreamNumber:Integer;StreamPosition:OleVariant;AudioLevel:Integer) of object;
  T_ISpeechVoiceEventsEnginePrivate = procedure(Sender: TObject;StreamNumber:Integer;StreamPosition:Integer;EngineData:OleVariant) of object;


  CoSpVoice = Class
  Public
    Class Function Create: ISpeechVoice;
    Class Function CreateRemote(const MachineName: string): ISpeechVoice;
  end;

  TAxcSpVoice = Class(TActiveXContainer)
  Private
    FServer:ISpeechVoice;
    FOnStartStream:T_ISpeechVoiceEventsStartStream;
    FOnEndStream:T_ISpeechVoiceEventsEndStream;
    FOnVoiceChange:T_ISpeechVoiceEventsVoiceChange;
    FOnBookmark:T_ISpeechVoiceEventsBookmark;
    FOnWord:T_ISpeechVoiceEventsWord;
    FOnSentence:T_ISpeechVoiceEventsSentence;
    FOnPhoneme:T_ISpeechVoiceEventsPhoneme;
    FOnViseme:T_ISpeechVoiceEventsViseme;
    FOnAudioLevel:T_ISpeechVoiceEventsAudioLevel;
    FOnEnginePrivate:T_ISpeechVoiceEventsEnginePrivate;

    FEventSink:TEventSink;
    procedure EventSinkInvoke(Sender: TObject; DispID: Integer;
          const IID: TGUID; LocaleID: Integer; Flags: Word;
          Params: tagDISPPARAMS; VarResult, ExcepInfo, ArgErr: Pointer);
  Public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property OleServer:ISpeechVoice read FServer;
  Published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnStatusText;
    property OnUnDock;
    property OnStartStream : T_ISpeechVoiceEventsStartStream read FOnStartStream write FOnStartStream;
    property OnEndStream : T_ISpeechVoiceEventsEndStream read FOnEndStream write FOnEndStream;
    property OnVoiceChange : T_ISpeechVoiceEventsVoiceChange read FOnVoiceChange write FOnVoiceChange;
    property OnBookmark : T_ISpeechVoiceEventsBookmark read FOnBookmark write FOnBookmark;
    property OnWord : T_ISpeechVoiceEventsWord read FOnWord write FOnWord;
    property OnSentence : T_ISpeechVoiceEventsSentence read FOnSentence write FOnSentence;
    property OnPhoneme : T_ISpeechVoiceEventsPhoneme read FOnPhoneme write FOnPhoneme;
    property OnViseme : T_ISpeechVoiceEventsViseme read FOnViseme write FOnViseme;
    property OnAudioLevel : T_ISpeechVoiceEventsAudioLevel read FOnAudioLevel write FOnAudioLevel;
    property OnEnginePrivate : T_ISpeechVoiceEventsEnginePrivate read FOnEnginePrivate write FOnEnginePrivate;

    property Active;
  end;

  T_ISpeechRecoContextEventsStartStream = procedure(Sender: TObject;StreamNumber:Integer;StreamPosition:OleVariant) of object;
  T_ISpeechRecoContextEventsEndStream = procedure(Sender: TObject;StreamNumber:Integer;StreamPosition:OleVariant;StreamReleased:WordBool) of object;
  T_ISpeechRecoContextEventsBookmark = procedure(Sender: TObject;StreamNumber:Integer;StreamPosition:OleVariant;BookmarkId:OleVariant;Options:SpeechBookmarkOptions) of object;
  T_ISpeechRecoContextEventsSoundStart = procedure(Sender: TObject;StreamNumber:Integer;StreamPosition:OleVariant) of object;
  T_ISpeechRecoContextEventsSoundEnd = procedure(Sender: TObject;StreamNumber:Integer;StreamPosition:OleVariant) of object;
  T_ISpeechRecoContextEventsPhraseStart = procedure(Sender: TObject;StreamNumber:Integer;StreamPosition:OleVariant) of object;
  T_ISpeechRecoContextEventsRecognition = procedure(Sender: TObject;StreamNumber:Integer;StreamPosition:OleVariant;RecognitionType:SpeechRecognitionType;Result:ISpeechRecoResult) of object;
  T_ISpeechRecoContextEventsHypothesis = procedure(Sender: TObject;StreamNumber:Integer;StreamPosition:OleVariant;Result:ISpeechRecoResult) of object;
  T_ISpeechRecoContextEventsPropertyNumberChange = procedure(Sender: TObject;StreamNumber:Integer;StreamPosition:OleVariant;PropertyName:WideString;NewNumberValue:Integer) of object;
  T_ISpeechRecoContextEventsPropertyStringChange = procedure(Sender: TObject;StreamNumber:Integer;StreamPosition:OleVariant;PropertyName:WideString;NewStringValue:WideString) of object;
  T_ISpeechRecoContextEventsFalseRecognition = procedure(Sender: TObject;StreamNumber:Integer;StreamPosition:OleVariant;Result:ISpeechRecoResult) of object;
  T_ISpeechRecoContextEventsInterference = procedure(Sender: TObject;StreamNumber:Integer;StreamPosition:OleVariant;Interference:SpeechInterference) of object;
  T_ISpeechRecoContextEventsRequestUI = procedure(Sender: TObject;StreamNumber:Integer;StreamPosition:OleVariant;UIType:WideString) of object;
  T_ISpeechRecoContextEventsRecognizerStateChange = procedure(Sender: TObject;StreamNumber:Integer;StreamPosition:OleVariant;NewState:SpeechRecognizerState) of object;
  T_ISpeechRecoContextEventsAdaptation = procedure(Sender: TObject;StreamNumber:Integer;StreamPosition:OleVariant) of object;
  T_ISpeechRecoContextEventsRecognitionForOtherContext = procedure(Sender: TObject;StreamNumber:Integer;StreamPosition:OleVariant) of object;
  T_ISpeechRecoContextEventsAudioLevel = procedure(Sender: TObject;StreamNumber:Integer;StreamPosition:OleVariant;AudioLevel:Integer) of object;
  T_ISpeechRecoContextEventsEnginePrivate = procedure(Sender: TObject;StreamNumber:Integer;StreamPosition:OleVariant;EngineData:OleVariant) of object;


  CoSpSharedRecoContext = Class
  Public
    Class Function Create: ISpeechRecoContext;
    Class Function CreateRemote(const MachineName: string): ISpeechRecoContext;
  end;

  TAxcSpSharedRecoContext = Class(TActiveXContainer)
  Private
    FServer:ISpeechRecoContext;
    FOnStartStream:T_ISpeechRecoContextEventsStartStream;
    FOnEndStream:T_ISpeechRecoContextEventsEndStream;
    FOnBookmark:T_ISpeechRecoContextEventsBookmark;
    FOnSoundStart:T_ISpeechRecoContextEventsSoundStart;
    FOnSoundEnd:T_ISpeechRecoContextEventsSoundEnd;
    FOnPhraseStart:T_ISpeechRecoContextEventsPhraseStart;
    FOnRecognition:T_ISpeechRecoContextEventsRecognition;
    FOnHypothesis:T_ISpeechRecoContextEventsHypothesis;
    FOnPropertyNumberChange:T_ISpeechRecoContextEventsPropertyNumberChange;
    FOnPropertyStringChange:T_ISpeechRecoContextEventsPropertyStringChange;
    FOnFalseRecognition:T_ISpeechRecoContextEventsFalseRecognition;
    FOnInterference:T_ISpeechRecoContextEventsInterference;
    FOnRequestUI:T_ISpeechRecoContextEventsRequestUI;
    FOnRecognizerStateChange:T_ISpeechRecoContextEventsRecognizerStateChange;
    FOnAdaptation:T_ISpeechRecoContextEventsAdaptation;
    FOnRecognitionForOtherContext:T_ISpeechRecoContextEventsRecognitionForOtherContext;
    FOnAudioLevel:T_ISpeechRecoContextEventsAudioLevel;
    FOnEnginePrivate:T_ISpeechRecoContextEventsEnginePrivate;

    FEventSink:TEventSink;
    procedure EventSinkInvoke(Sender: TObject; DispID: Integer;
          const IID: TGUID; LocaleID: Integer; Flags: Word;
          Params: tagDISPPARAMS; VarResult, ExcepInfo, ArgErr: Pointer);
  Public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property OleServer:ISpeechRecoContext read FServer;
  Published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnStatusText;
    property OnUnDock;
    property OnStartStream : T_ISpeechRecoContextEventsStartStream read FOnStartStream write FOnStartStream;
    property OnEndStream : T_ISpeechRecoContextEventsEndStream read FOnEndStream write FOnEndStream;
    property OnBookmark : T_ISpeechRecoContextEventsBookmark read FOnBookmark write FOnBookmark;
    property OnSoundStart : T_ISpeechRecoContextEventsSoundStart read FOnSoundStart write FOnSoundStart;
    property OnSoundEnd : T_ISpeechRecoContextEventsSoundEnd read FOnSoundEnd write FOnSoundEnd;
    property OnPhraseStart : T_ISpeechRecoContextEventsPhraseStart read FOnPhraseStart write FOnPhraseStart;
    property OnRecognition : T_ISpeechRecoContextEventsRecognition read FOnRecognition write FOnRecognition;
    property OnHypothesis : T_ISpeechRecoContextEventsHypothesis read FOnHypothesis write FOnHypothesis;
    property OnPropertyNumberChange : T_ISpeechRecoContextEventsPropertyNumberChange read FOnPropertyNumberChange write FOnPropertyNumberChange;
    property OnPropertyStringChange : T_ISpeechRecoContextEventsPropertyStringChange read FOnPropertyStringChange write FOnPropertyStringChange;
    property OnFalseRecognition : T_ISpeechRecoContextEventsFalseRecognition read FOnFalseRecognition write FOnFalseRecognition;
    property OnInterference : T_ISpeechRecoContextEventsInterference read FOnInterference write FOnInterference;
    property OnRequestUI : T_ISpeechRecoContextEventsRequestUI read FOnRequestUI write FOnRequestUI;
    property OnRecognizerStateChange : T_ISpeechRecoContextEventsRecognizerStateChange read FOnRecognizerStateChange write FOnRecognizerStateChange;
    property OnAdaptation : T_ISpeechRecoContextEventsAdaptation read FOnAdaptation write FOnAdaptation;
    property OnRecognitionForOtherContext : T_ISpeechRecoContextEventsRecognitionForOtherContext read FOnRecognitionForOtherContext write FOnRecognitionForOtherContext;
    property OnAudioLevel : T_ISpeechRecoContextEventsAudioLevel read FOnAudioLevel write FOnAudioLevel;
    property OnEnginePrivate : T_ISpeechRecoContextEventsEnginePrivate read FOnEnginePrivate write FOnEnginePrivate;

    property Active;
  end;

  CoSpInprocRecognizer = Class
  Public
    Class Function Create: ISpeechRecognizer;
    Class Function CreateRemote(const MachineName: string): ISpeechRecognizer;
  end;

  TAxcSpInprocRecognizer = Class(TActiveXContainer)
  Private
    FServer:ISpeechRecognizer;
  Public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property OleServer:ISpeechRecognizer read FServer;
  Published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnStatusText;
    property OnUnDock;
    property Active;
  end;

  CoSpSharedRecognizer = Class
  Public
    Class Function Create: ISpeechRecognizer;
    Class Function CreateRemote(const MachineName: string): ISpeechRecognizer;
  end;

  TAxcSpSharedRecognizer = Class(TActiveXContainer)
  Private
    FServer:ISpeechRecognizer;
  Public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property OleServer:ISpeechRecognizer read FServer;
  Published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnStatusText;
    property OnUnDock;
    property Active;
  end;

  CoSpLexicon = Class
  Public
    Class Function Create: ISpeechLexicon;
    Class Function CreateRemote(const MachineName: string): ISpeechLexicon;
  end;

  TAxcSpLexicon = Class(TActiveXContainer)
  Private
    FServer:ISpeechLexicon;
  Public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property OleServer:ISpeechLexicon read FServer;
  Published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnStatusText;
    property OnUnDock;
    property Active;
  end;

  CoSpUnCompressedLexicon = Class
  Public
    Class Function Create: ISpeechLexicon;
    Class Function CreateRemote(const MachineName: string): ISpeechLexicon;
  end;

  TAxcSpUnCompressedLexicon = Class(TActiveXContainer)
  Private
    FServer:ISpeechLexicon;
  Public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property OleServer:ISpeechLexicon read FServer;
  Published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnStatusText;
    property OnUnDock;
    property Active;
  end;

  CoSpCompressedLexicon = Class
  Public
    Class Function Create: ISpLexicon;
    Class Function CreateRemote(const MachineName: string): ISpLexicon;
  end;

  CoSpShortcut = Class
  Public
    Class Function Create: ISpShortcut;
    Class Function CreateRemote(const MachineName: string): ISpShortcut;
  end;

  CoSpPhoneConverter = Class
  Public
    Class Function Create: ISpeechPhoneConverter;
    Class Function CreateRemote(const MachineName: string): ISpeechPhoneConverter;
  end;

  TAxcSpPhoneConverter = Class(TActiveXContainer)
  Private
    FServer:ISpeechPhoneConverter;
  Public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property OleServer:ISpeechPhoneConverter read FServer;
  Published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnStatusText;
    property OnUnDock;
    property Active;
  end;

  CoSpPhoneticAlphabetConverter = Class
  Public
    Class Function Create: ISpPhoneticAlphabetConverter;
    Class Function CreateRemote(const MachineName: string): ISpPhoneticAlphabetConverter;
  end;

  CoSpNullPhoneConverter = Class
  Public
    Class Function Create: ISpPhoneConverter;
    Class Function CreateRemote(const MachineName: string): ISpPhoneConverter;
  end;

  CoSpTextSelectionInformation = Class
  Public
    Class Function Create: ISpeechTextSelectionInformation;
    Class Function CreateRemote(const MachineName: string): ISpeechTextSelectionInformation;
  end;

  TAxcSpTextSelectionInformation = Class(TActiveXContainer)
  Private
    FServer:ISpeechTextSelectionInformation;
  Public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property OleServer:ISpeechTextSelectionInformation read FServer;
  Published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnStatusText;
    property OnUnDock;
    property Active;
  end;

  CoSpPhraseInfoBuilder = Class
  Public
    Class Function Create: ISpeechPhraseInfoBuilder;
    Class Function CreateRemote(const MachineName: string): ISpeechPhraseInfoBuilder;
  end;

  TAxcSpPhraseInfoBuilder = Class(TActiveXContainer)
  Private
    FServer:ISpeechPhraseInfoBuilder;
  Public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property OleServer:ISpeechPhraseInfoBuilder read FServer;
  Published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnStatusText;
    property OnUnDock;
    property Active;
  end;

  CoSpAudioFormat = Class
  Public
    Class Function Create: ISpeechAudioFormat;
    Class Function CreateRemote(const MachineName: string): ISpeechAudioFormat;
  end;

  TAxcSpAudioFormat = Class(TActiveXContainer)
  Private
    FServer:ISpeechAudioFormat;
  Public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property OleServer:ISpeechAudioFormat read FServer;
  Published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnStatusText;
    property OnUnDock;
    property Active;
  end;

  CoSpWaveFormatEx = Class
  Public
    Class Function Create: ISpeechWaveFormatEx;
    Class Function CreateRemote(const MachineName: string): ISpeechWaveFormatEx;
  end;

  TAxcSpWaveFormatEx = Class(TActiveXContainer)
  Private
    FServer:ISpeechWaveFormatEx;
  Public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property OleServer:ISpeechWaveFormatEx read FServer;
  Published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnStatusText;
    property OnUnDock;
    property Active;
  end;

  CoSpInProcRecoContext = Class
  Public
    Class Function Create: ISpeechRecoContext;
    Class Function CreateRemote(const MachineName: string): ISpeechRecoContext;
  end;

  TAxcSpInProcRecoContext = Class(TActiveXContainer)
  Private
    FServer:ISpeechRecoContext;
    FOnStartStream:T_ISpeechRecoContextEventsStartStream;
    FOnEndStream:T_ISpeechRecoContextEventsEndStream;
    FOnBookmark:T_ISpeechRecoContextEventsBookmark;
    FOnSoundStart:T_ISpeechRecoContextEventsSoundStart;
    FOnSoundEnd:T_ISpeechRecoContextEventsSoundEnd;
    FOnPhraseStart:T_ISpeechRecoContextEventsPhraseStart;
    FOnRecognition:T_ISpeechRecoContextEventsRecognition;
    FOnHypothesis:T_ISpeechRecoContextEventsHypothesis;
    FOnPropertyNumberChange:T_ISpeechRecoContextEventsPropertyNumberChange;
    FOnPropertyStringChange:T_ISpeechRecoContextEventsPropertyStringChange;
    FOnFalseRecognition:T_ISpeechRecoContextEventsFalseRecognition;
    FOnInterference:T_ISpeechRecoContextEventsInterference;
    FOnRequestUI:T_ISpeechRecoContextEventsRequestUI;
    FOnRecognizerStateChange:T_ISpeechRecoContextEventsRecognizerStateChange;
    FOnAdaptation:T_ISpeechRecoContextEventsAdaptation;
    FOnRecognitionForOtherContext:T_ISpeechRecoContextEventsRecognitionForOtherContext;
    FOnAudioLevel:T_ISpeechRecoContextEventsAudioLevel;
    FOnEnginePrivate:T_ISpeechRecoContextEventsEnginePrivate;

    FEventSink:TEventSink;
    procedure EventSinkInvoke(Sender: TObject; DispID: Integer;
          const IID: TGUID; LocaleID: Integer; Flags: Word;
          Params: tagDISPPARAMS; VarResult, ExcepInfo, ArgErr: Pointer);
  Public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property OleServer:ISpeechRecoContext read FServer;
  Published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnStatusText;
    property OnUnDock;
    property OnStartStream : T_ISpeechRecoContextEventsStartStream read FOnStartStream write FOnStartStream;
    property OnEndStream : T_ISpeechRecoContextEventsEndStream read FOnEndStream write FOnEndStream;
    property OnBookmark : T_ISpeechRecoContextEventsBookmark read FOnBookmark write FOnBookmark;
    property OnSoundStart : T_ISpeechRecoContextEventsSoundStart read FOnSoundStart write FOnSoundStart;
    property OnSoundEnd : T_ISpeechRecoContextEventsSoundEnd read FOnSoundEnd write FOnSoundEnd;
    property OnPhraseStart : T_ISpeechRecoContextEventsPhraseStart read FOnPhraseStart write FOnPhraseStart;
    property OnRecognition : T_ISpeechRecoContextEventsRecognition read FOnRecognition write FOnRecognition;
    property OnHypothesis : T_ISpeechRecoContextEventsHypothesis read FOnHypothesis write FOnHypothesis;
    property OnPropertyNumberChange : T_ISpeechRecoContextEventsPropertyNumberChange read FOnPropertyNumberChange write FOnPropertyNumberChange;
    property OnPropertyStringChange : T_ISpeechRecoContextEventsPropertyStringChange read FOnPropertyStringChange write FOnPropertyStringChange;
    property OnFalseRecognition : T_ISpeechRecoContextEventsFalseRecognition read FOnFalseRecognition write FOnFalseRecognition;
    property OnInterference : T_ISpeechRecoContextEventsInterference read FOnInterference write FOnInterference;
    property OnRequestUI : T_ISpeechRecoContextEventsRequestUI read FOnRequestUI write FOnRequestUI;
    property OnRecognizerStateChange : T_ISpeechRecoContextEventsRecognizerStateChange read FOnRecognizerStateChange write FOnRecognizerStateChange;
    property OnAdaptation : T_ISpeechRecoContextEventsAdaptation read FOnAdaptation write FOnAdaptation;
    property OnRecognitionForOtherContext : T_ISpeechRecoContextEventsRecognitionForOtherContext read FOnRecognitionForOtherContext write FOnRecognitionForOtherContext;
    property OnAudioLevel : T_ISpeechRecoContextEventsAudioLevel read FOnAudioLevel write FOnAudioLevel;
    property OnEnginePrivate : T_ISpeechRecoContextEventsEnginePrivate read FOnEnginePrivate write FOnEnginePrivate;

    property Active;
  end;

  CoSpCustomStream = Class
  Public
    Class Function Create: ISpeechCustomStream;
    Class Function CreateRemote(const MachineName: string): ISpeechCustomStream;
  end;

  TAxcSpCustomStream = Class(TActiveXContainer)
  Private
    FServer:ISpeechCustomStream;
  Public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property OleServer:ISpeechCustomStream read FServer;
  Published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnStatusText;
    property OnUnDock;
    property Active;
  end;

  CoSpFileStream = Class
  Public
    Class Function Create: ISpeechFileStream;
    Class Function CreateRemote(const MachineName: string): ISpeechFileStream;
  end;

  TAxcSpFileStream = Class(TActiveXContainer)
  Private
    FServer:ISpeechFileStream;
  Public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property OleServer:ISpeechFileStream read FServer;
  Published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnStatusText;
    property OnUnDock;
    property Active;
  end;

  CoSpMemoryStream = Class
  Public
    Class Function Create: ISpeechMemoryStream;
    Class Function CreateRemote(const MachineName: string): ISpeechMemoryStream;
  end;

  TAxcSpMemoryStream = Class(TActiveXContainer)
  Private
    FServer:ISpeechMemoryStream;
  Public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property OleServer:ISpeechMemoryStream read FServer;
  Published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnStatusText;
    property OnUnDock;
    property Active;
  end;

implementation

uses comobj;

Class Function CoSpNotifyTranslator.Create: ISpNotifyTranslator;
begin
  Result := CreateComObject(CLASS_SpNotifyTranslator) as ISpNotifyTranslator;
end;

Class Function CoSpNotifyTranslator.CreateRemote(const MachineName: string): ISpNotifyTranslator;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SpNotifyTranslator) as ISpNotifyTranslator;
end;

Class Function CoSpObjectTokenCategory.Create: ISpeechObjectTokenCategory;
begin
  Result := CreateComObject(CLASS_SpObjectTokenCategory) as ISpeechObjectTokenCategory;
end;

Class Function CoSpObjectTokenCategory.CreateRemote(const MachineName: string): ISpeechObjectTokenCategory;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SpObjectTokenCategory) as ISpeechObjectTokenCategory;
end;

constructor TAxcSpObjectTokenCategory.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FServer:=CoSpObjectTokenCategory.Create;
  ComServer:=FServer;
end;

destructor TAxcSpObjectTokenCategory.Destroy;
begin
  inherited destroy;
end;

Class Function CoSpObjectToken.Create: ISpeechObjectToken;
begin
  Result := CreateComObject(CLASS_SpObjectToken) as ISpeechObjectToken;
end;

Class Function CoSpObjectToken.CreateRemote(const MachineName: string): ISpeechObjectToken;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SpObjectToken) as ISpeechObjectToken;
end;

constructor TAxcSpObjectToken.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FServer:=CoSpObjectToken.Create;
  ComServer:=FServer;
end;

destructor TAxcSpObjectToken.Destroy;
begin
  inherited destroy;
end;

Class Function CoSpResourceManager.Create: ISpResourceManager;
begin
  Result := CreateComObject(CLASS_SpResourceManager) as ISpResourceManager;
end;

Class Function CoSpResourceManager.CreateRemote(const MachineName: string): ISpResourceManager;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SpResourceManager) as ISpResourceManager;
end;

Class Function CoSpStreamFormatConverter.Create: ISpStreamFormatConverter;
begin
  Result := CreateComObject(CLASS_SpStreamFormatConverter) as ISpStreamFormatConverter;
end;

Class Function CoSpStreamFormatConverter.CreateRemote(const MachineName: string): ISpStreamFormatConverter;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SpStreamFormatConverter) as ISpStreamFormatConverter;
end;

Class Function CoSpMMAudioEnum.Create: IEnumSpObjectTokens;
begin
  Result := CreateComObject(CLASS_SpMMAudioEnum) as IEnumSpObjectTokens;
end;

Class Function CoSpMMAudioEnum.CreateRemote(const MachineName: string): IEnumSpObjectTokens;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SpMMAudioEnum) as IEnumSpObjectTokens;
end;

Class Function CoSpMMAudioIn.Create: ISpeechMMSysAudio;
begin
  Result := CreateComObject(CLASS_SpMMAudioIn) as ISpeechMMSysAudio;
end;

Class Function CoSpMMAudioIn.CreateRemote(const MachineName: string): ISpeechMMSysAudio;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SpMMAudioIn) as ISpeechMMSysAudio;
end;

constructor TAxcSpMMAudioIn.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FServer:=CoSpMMAudioIn.Create;
  ComServer:=FServer;
end;

destructor TAxcSpMMAudioIn.Destroy;
begin
  inherited destroy;
end;

Class Function CoSpMMAudioOut.Create: ISpeechMMSysAudio;
begin
  Result := CreateComObject(CLASS_SpMMAudioOut) as ISpeechMMSysAudio;
end;

Class Function CoSpMMAudioOut.CreateRemote(const MachineName: string): ISpeechMMSysAudio;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SpMMAudioOut) as ISpeechMMSysAudio;
end;

constructor TAxcSpMMAudioOut.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FServer:=CoSpMMAudioOut.Create;
  ComServer:=FServer;
end;

destructor TAxcSpMMAudioOut.Destroy;
begin
  inherited destroy;
end;

Class Function CoSpStream.Create: ISpStream;
begin
  Result := CreateComObject(CLASS_SpStream) as ISpStream;
end;

Class Function CoSpStream.CreateRemote(const MachineName: string): ISpStream;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SpStream) as ISpStream;
end;

Class Function CoSpVoice.Create: ISpeechVoice;
begin
  Result := CreateComObject(CLASS_SpVoice) as ISpeechVoice;
end;

Class Function CoSpVoice.CreateRemote(const MachineName: string): ISpeechVoice;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SpVoice) as ISpeechVoice;
end;

constructor TAxcSpVoice.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FServer:=CoSpVoice.Create;
  ComServer:=FServer;
  FEventSink:=TEventSink.Create(Self);
  FEventSink.OnInvoke:=EventSinkInvoke;
  FEventSink.Connect(FServer,_ISpeechVoiceEvents);
end;

destructor TAxcSpVoice.Destroy;
begin
  FEventSink.Destroy;
  inherited destroy;
end;

procedure TAxcSpVoice.EventSinkInvoke(Sender: TObject; DispID: Integer;
  const IID: TGUID; LocaleID: Integer; Flags: Word; Params: tagDISPPARAMS;
  VarResult, ExcepInfo, ArgErr: Pointer);
begin
  case DispID of
    1: if assigned(OnStartStream) then
          OnStartStream(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    2: if assigned(OnEndStream) then
          OnEndStream(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    3: if assigned(OnVoiceChange) then
          OnVoiceChange(Self, OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    4: if assigned(OnBookmark) then
          OnBookmark(Self, OleVariant(Params.rgvarg[3]), OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    5: if assigned(OnWord) then
          OnWord(Self, OleVariant(Params.rgvarg[3]), OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    7: if assigned(OnSentence) then
          OnSentence(Self, OleVariant(Params.rgvarg[3]), OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    6: if assigned(OnPhoneme) then
          OnPhoneme(Self, OleVariant(Params.rgvarg[5]), OleVariant(Params.rgvarg[4]), OleVariant(Params.rgvarg[3]), OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    8: if assigned(OnViseme) then
          OnViseme(Self, OleVariant(Params.rgvarg[5]), OleVariant(Params.rgvarg[4]), OleVariant(Params.rgvarg[3]), OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    9: if assigned(OnAudioLevel) then
          OnAudioLevel(Self, OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    10: if assigned(OnEnginePrivate) then
          OnEnginePrivate(Self, OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));

  end;
end;

Class Function CoSpSharedRecoContext.Create: ISpeechRecoContext;
begin
  Result := CreateComObject(CLASS_SpSharedRecoContext) as ISpeechRecoContext;
end;

Class Function CoSpSharedRecoContext.CreateRemote(const MachineName: string): ISpeechRecoContext;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SpSharedRecoContext) as ISpeechRecoContext;
end;

constructor TAxcSpSharedRecoContext.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FServer:=CoSpSharedRecoContext.Create;
  ComServer:=FServer;
  FEventSink:=TEventSink.Create(Self);
  FEventSink.OnInvoke:=EventSinkInvoke;
  FEventSink.Connect(FServer,_ISpeechRecoContextEvents);
end;

destructor TAxcSpSharedRecoContext.Destroy;
begin
  FEventSink.Destroy;
  inherited destroy;
end;

procedure TAxcSpSharedRecoContext.EventSinkInvoke(Sender: TObject; DispID: Integer;
  const IID: TGUID; LocaleID: Integer; Flags: Word; Params: tagDISPPARAMS;
  VarResult, ExcepInfo, ArgErr: Pointer);
begin
  case DispID of
    1: if assigned(OnStartStream) then
          OnStartStream(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    2: if assigned(OnEndStream) then
          OnEndStream(Self, OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    3: if assigned(OnBookmark) then
          OnBookmark(Self, OleVariant(Params.rgvarg[3]), OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    4: if assigned(OnSoundStart) then
          OnSoundStart(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    5: if assigned(OnSoundEnd) then
          OnSoundEnd(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    6: if assigned(OnPhraseStart) then
          OnPhraseStart(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    7: if assigned(OnRecognition) then
          OnRecognition(Self, OleVariant(Params.rgvarg[3]), OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    8: if assigned(OnHypothesis) then
          OnHypothesis(Self, OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    9: if assigned(OnPropertyNumberChange) then
          OnPropertyNumberChange(Self, OleVariant(Params.rgvarg[3]), OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    10: if assigned(OnPropertyStringChange) then
          OnPropertyStringChange(Self, OleVariant(Params.rgvarg[3]), OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    11: if assigned(OnFalseRecognition) then
          OnFalseRecognition(Self, OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    12: if assigned(OnInterference) then
          OnInterference(Self, OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    13: if assigned(OnRequestUI) then
          OnRequestUI(Self, OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    14: if assigned(OnRecognizerStateChange) then
          OnRecognizerStateChange(Self, OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    15: if assigned(OnAdaptation) then
          OnAdaptation(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    16: if assigned(OnRecognitionForOtherContext) then
          OnRecognitionForOtherContext(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    17: if assigned(OnAudioLevel) then
          OnAudioLevel(Self, OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    18: if assigned(OnEnginePrivate) then
          OnEnginePrivate(Self, OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));

  end;
end;

Class Function CoSpInprocRecognizer.Create: ISpeechRecognizer;
begin
  Result := CreateComObject(CLASS_SpInprocRecognizer) as ISpeechRecognizer;
end;

Class Function CoSpInprocRecognizer.CreateRemote(const MachineName: string): ISpeechRecognizer;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SpInprocRecognizer) as ISpeechRecognizer;
end;

constructor TAxcSpInprocRecognizer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FServer:=CoSpInprocRecognizer.Create;
  ComServer:=FServer;
end;

destructor TAxcSpInprocRecognizer.Destroy;
begin
  inherited destroy;
end;

Class Function CoSpSharedRecognizer.Create: ISpeechRecognizer;
begin
  Result := CreateComObject(CLASS_SpSharedRecognizer) as ISpeechRecognizer;
end;

Class Function CoSpSharedRecognizer.CreateRemote(const MachineName: string): ISpeechRecognizer;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SpSharedRecognizer) as ISpeechRecognizer;
end;

constructor TAxcSpSharedRecognizer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FServer:=CoSpSharedRecognizer.Create;
  ComServer:=FServer;
end;

destructor TAxcSpSharedRecognizer.Destroy;
begin
  inherited destroy;
end;

Class Function CoSpLexicon.Create: ISpeechLexicon;
begin
  Result := CreateComObject(CLASS_SpLexicon) as ISpeechLexicon;
end;

Class Function CoSpLexicon.CreateRemote(const MachineName: string): ISpeechLexicon;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SpLexicon) as ISpeechLexicon;
end;

constructor TAxcSpLexicon.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FServer:=CoSpLexicon.Create;
  ComServer:=FServer;
end;

destructor TAxcSpLexicon.Destroy;
begin
  inherited destroy;
end;

Class Function CoSpUnCompressedLexicon.Create: ISpeechLexicon;
begin
  Result := CreateComObject(CLASS_SpUnCompressedLexicon) as ISpeechLexicon;
end;

Class Function CoSpUnCompressedLexicon.CreateRemote(const MachineName: string): ISpeechLexicon;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SpUnCompressedLexicon) as ISpeechLexicon;
end;

constructor TAxcSpUnCompressedLexicon.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FServer:=CoSpUnCompressedLexicon.Create;
  ComServer:=FServer;
end;

destructor TAxcSpUnCompressedLexicon.Destroy;
begin
  inherited destroy;
end;

Class Function CoSpCompressedLexicon.Create: ISpLexicon;
begin
  Result := CreateComObject(CLASS_SpCompressedLexicon) as ISpLexicon;
end;

Class Function CoSpCompressedLexicon.CreateRemote(const MachineName: string): ISpLexicon;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SpCompressedLexicon) as ISpLexicon;
end;

Class Function CoSpShortcut.Create: ISpShortcut;
begin
  Result := CreateComObject(CLASS_SpShortcut) as ISpShortcut;
end;

Class Function CoSpShortcut.CreateRemote(const MachineName: string): ISpShortcut;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SpShortcut) as ISpShortcut;
end;

Class Function CoSpPhoneConverter.Create: ISpeechPhoneConverter;
begin
  Result := CreateComObject(CLASS_SpPhoneConverter) as ISpeechPhoneConverter;
end;

Class Function CoSpPhoneConverter.CreateRemote(const MachineName: string): ISpeechPhoneConverter;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SpPhoneConverter) as ISpeechPhoneConverter;
end;

constructor TAxcSpPhoneConverter.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FServer:=CoSpPhoneConverter.Create;
  ComServer:=FServer;
end;

destructor TAxcSpPhoneConverter.Destroy;
begin
  inherited destroy;
end;

Class Function CoSpPhoneticAlphabetConverter.Create: ISpPhoneticAlphabetConverter;
begin
  Result := CreateComObject(CLASS_SpPhoneticAlphabetConverter) as ISpPhoneticAlphabetConverter;
end;

Class Function CoSpPhoneticAlphabetConverter.CreateRemote(const MachineName: string): ISpPhoneticAlphabetConverter;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SpPhoneticAlphabetConverter) as ISpPhoneticAlphabetConverter;
end;

Class Function CoSpNullPhoneConverter.Create: ISpPhoneConverter;
begin
  Result := CreateComObject(CLASS_SpNullPhoneConverter) as ISpPhoneConverter;
end;

Class Function CoSpNullPhoneConverter.CreateRemote(const MachineName: string): ISpPhoneConverter;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SpNullPhoneConverter) as ISpPhoneConverter;
end;

Class Function CoSpTextSelectionInformation.Create: ISpeechTextSelectionInformation;
begin
  Result := CreateComObject(CLASS_SpTextSelectionInformation) as ISpeechTextSelectionInformation;
end;

Class Function CoSpTextSelectionInformation.CreateRemote(const MachineName: string): ISpeechTextSelectionInformation;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SpTextSelectionInformation) as ISpeechTextSelectionInformation;
end;

constructor TAxcSpTextSelectionInformation.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FServer:=CoSpTextSelectionInformation.Create;
  ComServer:=FServer;
end;

destructor TAxcSpTextSelectionInformation.Destroy;
begin
  inherited destroy;
end;

Class Function CoSpPhraseInfoBuilder.Create: ISpeechPhraseInfoBuilder;
begin
  Result := CreateComObject(CLASS_SpPhraseInfoBuilder) as ISpeechPhraseInfoBuilder;
end;

Class Function CoSpPhraseInfoBuilder.CreateRemote(const MachineName: string): ISpeechPhraseInfoBuilder;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SpPhraseInfoBuilder) as ISpeechPhraseInfoBuilder;
end;

constructor TAxcSpPhraseInfoBuilder.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FServer:=CoSpPhraseInfoBuilder.Create;
  ComServer:=FServer;
end;

destructor TAxcSpPhraseInfoBuilder.Destroy;
begin
  inherited destroy;
end;

Class Function CoSpAudioFormat.Create: ISpeechAudioFormat;
begin
  Result := CreateComObject(CLASS_SpAudioFormat) as ISpeechAudioFormat;
end;

Class Function CoSpAudioFormat.CreateRemote(const MachineName: string): ISpeechAudioFormat;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SpAudioFormat) as ISpeechAudioFormat;
end;

constructor TAxcSpAudioFormat.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FServer:=CoSpAudioFormat.Create;
  ComServer:=FServer;
end;

destructor TAxcSpAudioFormat.Destroy;
begin
  inherited destroy;
end;

Class Function CoSpWaveFormatEx.Create: ISpeechWaveFormatEx;
begin
  Result := CreateComObject(CLASS_SpWaveFormatEx) as ISpeechWaveFormatEx;
end;

Class Function CoSpWaveFormatEx.CreateRemote(const MachineName: string): ISpeechWaveFormatEx;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SpWaveFormatEx) as ISpeechWaveFormatEx;
end;

constructor TAxcSpWaveFormatEx.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FServer:=CoSpWaveFormatEx.Create;
  ComServer:=FServer;
end;

destructor TAxcSpWaveFormatEx.Destroy;
begin
  inherited destroy;
end;

Class Function CoSpInProcRecoContext.Create: ISpeechRecoContext;
begin
  Result := CreateComObject(CLASS_SpInProcRecoContext) as ISpeechRecoContext;
end;

Class Function CoSpInProcRecoContext.CreateRemote(const MachineName: string): ISpeechRecoContext;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SpInProcRecoContext) as ISpeechRecoContext;
end;

constructor TAxcSpInProcRecoContext.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FServer:=CoSpInProcRecoContext.Create;
  ComServer:=FServer;
  FEventSink:=TEventSink.Create(Self);
  FEventSink.OnInvoke:=EventSinkInvoke;
  FEventSink.Connect(FServer,_ISpeechRecoContextEvents);
end;

destructor TAxcSpInProcRecoContext.Destroy;
begin
  FEventSink.Destroy;
  inherited destroy;
end;

procedure TAxcSpInProcRecoContext.EventSinkInvoke(Sender: TObject; DispID: Integer;
  const IID: TGUID; LocaleID: Integer; Flags: Word; Params: tagDISPPARAMS;
  VarResult, ExcepInfo, ArgErr: Pointer);
begin
  case DispID of
    1: if assigned(OnStartStream) then
          OnStartStream(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    2: if assigned(OnEndStream) then
          OnEndStream(Self, OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    3: if assigned(OnBookmark) then
          OnBookmark(Self, OleVariant(Params.rgvarg[3]), OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    4: if assigned(OnSoundStart) then
          OnSoundStart(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    5: if assigned(OnSoundEnd) then
          OnSoundEnd(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    6: if assigned(OnPhraseStart) then
          OnPhraseStart(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    7: if assigned(OnRecognition) then
          OnRecognition(Self, OleVariant(Params.rgvarg[3]), OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    8: if assigned(OnHypothesis) then
          OnHypothesis(Self, OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    9: if assigned(OnPropertyNumberChange) then
          OnPropertyNumberChange(Self, OleVariant(Params.rgvarg[3]), OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    10: if assigned(OnPropertyStringChange) then
          OnPropertyStringChange(Self, OleVariant(Params.rgvarg[3]), OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    11: if assigned(OnFalseRecognition) then
          OnFalseRecognition(Self, OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    12: if assigned(OnInterference) then
          OnInterference(Self, OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    13: if assigned(OnRequestUI) then
          OnRequestUI(Self, OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    14: if assigned(OnRecognizerStateChange) then
          OnRecognizerStateChange(Self, OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    15: if assigned(OnAdaptation) then
          OnAdaptation(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    16: if assigned(OnRecognitionForOtherContext) then
          OnRecognitionForOtherContext(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    17: if assigned(OnAudioLevel) then
          OnAudioLevel(Self, OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    18: if assigned(OnEnginePrivate) then
          OnEnginePrivate(Self, OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));

  end;
end;

Class Function CoSpCustomStream.Create: ISpeechCustomStream;
begin
  Result := CreateComObject(CLASS_SpCustomStream) as ISpeechCustomStream;
end;

Class Function CoSpCustomStream.CreateRemote(const MachineName: string): ISpeechCustomStream;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SpCustomStream) as ISpeechCustomStream;
end;

constructor TAxcSpCustomStream.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FServer:=CoSpCustomStream.Create;
  ComServer:=FServer;
end;

destructor TAxcSpCustomStream.Destroy;
begin
  inherited destroy;
end;

Class Function CoSpFileStream.Create: ISpeechFileStream;
begin
  Result := CreateComObject(CLASS_SpFileStream) as ISpeechFileStream;
end;

Class Function CoSpFileStream.CreateRemote(const MachineName: string): ISpeechFileStream;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SpFileStream) as ISpeechFileStream;
end;

constructor TAxcSpFileStream.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FServer:=CoSpFileStream.Create;
  ComServer:=FServer;
end;

destructor TAxcSpFileStream.Destroy;
begin
  inherited destroy;
end;

Class Function CoSpMemoryStream.Create: ISpeechMemoryStream;
begin
  Result := CreateComObject(CLASS_SpMemoryStream) as ISpeechMemoryStream;
end;

Class Function CoSpMemoryStream.CreateRemote(const MachineName: string): ISpeechMemoryStream;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SpMemoryStream) as ISpeechMemoryStream;
end;

constructor TAxcSpMemoryStream.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FServer:=CoSpMemoryStream.Create;
  ComServer:=FServer;
end;

destructor TAxcSpMemoryStream.Destroy;
begin
  inherited destroy;
end;

end.
