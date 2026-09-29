// SPDX-License-Identifier: AGPL-3.0-or-later

import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import type {VoiceNoiseSuppressionBackend} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionBackends';
import {resolveNoiseSuppressionBackend} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionSelection';

export function readNoiseSuppressionBackend(): VoiceNoiseSuppressionBackend {
	return resolveNoiseSuppressionBackend(VoiceSettings.getNoiseSuppressionBackend());
}
