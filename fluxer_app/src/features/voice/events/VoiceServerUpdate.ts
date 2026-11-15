// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {VOICE_MESH_ENDPOINT, VOICE_MESH_TOKEN} from '@app/features/voice/engine/mesh/VoiceMeshSignal';
import VoiceRuntime from '@app/features/voice/engine/VoiceRuntime';

interface VoiceServerUpdateSfuPayload {
	p2p?: undefined;
	token: string;
	endpoint: string;
	connection_id: string;
	guild_id?: string;
	channel_id?: string;
	e2ee_key?: string | null;
}

interface VoiceServerUpdateP2pPayload {
	p2p: true;
	ice_servers: Array<Pick<RTCIceServer, 'urls'>>;
	connection_id: string;
	guild_id?: string;
	channel_id?: string;
}

type VoiceServerUpdatePayload = VoiceServerUpdateSfuPayload | VoiceServerUpdateP2pPayload;

const logger = new Logger('VoiceServerUpdate');

export function handleVoiceServerUpdate(data: VoiceServerUpdatePayload, _context: GatewayHandlerContext): void {
	const engine = VoiceRuntime.engine;
	if (engine === null) {
		logger.warn('Dropping a voice server update that arrived before the voice runtime loaded', {
			connectionId: data.connection_id,
		});
		return;
	}
	if (data.p2p === true) {
		engine.handleVoiceServerUpdate({
			token: VOICE_MESH_TOKEN,
			endpoint: VOICE_MESH_ENDPOINT,
			connection_id: data.connection_id,
			guild_id: data.guild_id,
			channel_id: data.channel_id,
			e2ee_key: null,
			ice_servers: data.ice_servers,
		});
		return;
	}
	engine.handleVoiceServerUpdate({
		token: data.token,
		endpoint: data.endpoint,
		connection_id: data.connection_id,
		guild_id: data.guild_id,
		channel_id: data.channel_id,
		e2ee_key: data.e2ee_key ?? null,
	});
}
