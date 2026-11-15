// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayErrorData} from '@app/features/gateway/transport/GatewaySocket';
import type {GuildReadyData} from '@app/features/gateway/types/GatewayGuildTypes';
import VoiceRuntime from '@app/features/voice/engine/VoiceRuntime';

export function seedVoiceStatesFromReady(guilds: Array<GuildReadyData>): void {
	VoiceRuntime.handleGatewayReady(guilds);
}

export function reportGatewayErrorToVoice(error: GatewayErrorData): void {
	VoiceRuntime.handleGatewayError(error);
}

export function teardownVoiceForFatalGatewayCrash(): void {
	VoiceRuntime.cleanup();
}
