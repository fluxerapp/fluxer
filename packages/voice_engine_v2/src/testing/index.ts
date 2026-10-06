// SPDX-License-Identifier: AGPL-3.0-or-later

export type {
	VoiceEngineV2ConformanceSubject,
	VoiceEngineV2ConformanceSubjectFactory,
} from '@fluxer/voice_engine_v2/src/testing/conformance';
export {runVoiceEngineV2ConformanceSuite, waitForRuntime} from '@fluxer/voice_engine_v2/src/testing/conformance';
export type {
	VoiceEngineV2EventLogFixture,
	VoiceEngineV2EventLogFixtureExpected,
	VoiceEngineV2EventLogFixtureStep,
	VoiceEngineV2EventLogReplayResult,
	VoiceEngineV2EventLogReplayStepResult,
} from '@fluxer/voice_engine_v2/src/testing/eventLogReplay';
export {
	assertVoiceEngineV2EventLogFixture,
	replayVoiceEngineV2EventLogFixture,
	VOICE_ENGINE_V2_EVENT_LOG_FIXTURE_VERSION,
} from '@fluxer/voice_engine_v2/src/testing/eventLogReplay';
export type {
	FakeVoiceEngineV2DriverCall,
	FakeVoiceEngineV2DriverCallType,
	FakeVoiceEngineV2DriverOptions,
	FakeVoiceEngineV2FailureMap,
} from '@fluxer/voice_engine_v2/src/testing/FakeVoiceEngineV2Driver';
export {FakeVoiceEngineV2Driver} from '@fluxer/voice_engine_v2/src/testing/FakeVoiceEngineV2Driver';
export type {VoiceEngineV2TestDriver} from '@fluxer/voice_engine_v2/src/testing/VoiceEngineV2TestImplementation';
export {VoiceEngineV2TestImplementation} from '@fluxer/voice_engine_v2/src/testing/VoiceEngineV2TestImplementation';
