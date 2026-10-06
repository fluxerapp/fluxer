// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type VoiceEngineV2Driver,
	VoiceEngineV2ImplementationBase,
} from '@fluxer/voice_engine_v2/src/implementations/VoiceEngineV2ImplementationBase';

export interface VoiceEngineV2TestDriver extends VoiceEngineV2Driver {}

export class VoiceEngineV2TestImplementation extends VoiceEngineV2ImplementationBase {
	readonly kind = 'js' as const;
}
