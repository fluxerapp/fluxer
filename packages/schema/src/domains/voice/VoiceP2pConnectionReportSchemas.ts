// SPDX-License-Identifier: AGPL-3.0-or-later

import {VOICE_P2P_MAX_PARTICIPANTS} from '@fluxer/constants/src/LimitConstants';
import {createNamedStringLiteralUnion, Int32Type, SnowflakeType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

const VOICE_P2P_CONNECTION_REPORTS_MAX = 8;

const VoiceP2pCandidateType = createNamedStringLiteralUnion(
	[
		['host', 'host', 'A local interface address'],
		['srflx', 'srflx', 'A public address a STUN server reflected'],
		['prflx', 'prflx', 'A public address learned from a peer during checks'],
		['relay', 'relay', 'A relayed address'],
	],
	'ICE candidate type of one end of the selected candidate pair',
);

const VoiceP2pConnectionReport = z.object({
	channel_id: SnowflakeType.describe('The voice channel or private channel of the call'),
	guild_id: SnowflakeType.nullable().describe('The guild of the voice channel, null for a call'),
	participant_count: z
		.number()
		.int()
		.min(2)
		.max(VOICE_P2P_MAX_PARTICIPANTS)
		.describe('Participants in the call when the peer connection settled'),
	outcome: createNamedStringLiteralUnion(
		[
			['connected', 'connected', 'The peer connection reached connected'],
			['failed', 'failed', 'The peer connection failed'],
		],
		'How the peer connection settled',
	),
	local_candidate_type: VoiceP2pCandidateType.nullable().describe('Local candidate type, null without a selected pair'),
	remote_candidate_type: VoiceP2pCandidateType.nullable().describe(
		'Remote candidate type, null without a selected pair',
	),
	ip_family: createNamedStringLiteralUnion(
		[
			['ipv4', 'ipv4', 'IPv4'],
			['ipv6', 'ipv6', 'IPv6'],
		],
		'Address family of the selected pair',
	)
		.nullable()
		.describe('Address family of the selected pair, null without one'),
	protocol: createNamedStringLiteralUnion(
		[
			['udp', 'udp', 'UDP'],
			['tcp', 'tcp', 'TCP'],
		],
		'Transport protocol of the selected pair',
	)
		.nullable()
		.describe('Transport protocol of the selected pair, null without one'),
	setup_ms: Int32Type.nullable().describe('Milliseconds from peer connection creation to first connected'),
	ice_restarted: z.boolean().describe('Whether the peer connection ran an ICE restart'),
});

export const VoiceP2pConnectionReportsRequest = z.object({
	reports: z
		.array(VoiceP2pConnectionReport)
		.min(1)
		.max(VOICE_P2P_CONNECTION_REPORTS_MAX)
		.describe('One report for each remote peer that settled'),
});
