// SPDX-License-Identifier: AGPL-3.0-or-later

import {VoiceEngineV2ImplementationBase} from '@fluxer/voice_engine_v2/src/implementations/VoiceEngineV2ImplementationBase';

export class VoiceEngineV2TestImplementation extends VoiceEngineV2ImplementationBase {
	readonly kind = 'js' as const;
}
