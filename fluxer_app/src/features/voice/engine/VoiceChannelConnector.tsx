// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import Users from '@app/features/user/state/Users';
import {VoiceChannelFullModal} from '@app/features/voice/components/alerts/VoiceChannelFullModal';
import {VoiceConnectionConfirmModal} from '@app/features/voice/components/alerts/VoiceConnectionConfirmModal';
import {VoiceP2pConsentModal} from '@app/features/voice/components/alerts/VoiceP2pConsentModal';
import {
	getAllVoiceStatesInChannelFromMediaEngine,
	getVoiceConnectionContextFromMediaEngine,
	getVoiceEngineV2SnapshotFromMediaEngine,
	type VoiceMediaEngineVoiceState,
} from '@app/features/voice/engine/VoiceMediaEngineBridge';
import type {VoiceStateSyncPartial} from '@app/features/voice/engine/VoiceStateSyncTypes';
import {selectVoiceEngineV2AppIntentSelfMuteForVoiceStatePayload} from '@app/features/voice/engine/v2/VoiceEngineV2AppSelectors';
import {isChannelP2p} from '@app/features/voice/state/ChannelP2pStatus';
import LocalVoiceState from '@app/features/voice/state/LocalVoiceState';
import VoiceP2pConsent from '@app/features/voice/state/VoiceP2pConsent';
import VoiceP2pRollout from '@app/features/voice/state/VoiceP2pRollout';
import VoicePrompts from '@app/features/voice/state/VoicePrompts';
import {applyVoiceSpeakPermissionToSelfMute} from '@app/features/voice/utils/VoicePermissionUtils';
import {ME} from '@fluxer/constants/src/AppConstants';
import {VOICE_CHANNEL_CONNECTION_LIMIT_DEFAULT} from '@fluxer/constants/src/LimitConstants';

const logger = new Logger('VoiceChannelConnector');

export function showVoiceChannelFullModal(): void {
	ModalCommands.push(
		modal(() => (
			<VoiceChannelFullModal data-flx="voice.engine.voice-channel-connector.show-voice-channel-full-modal.voice-channel-full-modal" />
		)),
	);
}

function countOtherVoiceConnections(guildId: string | null, channelId: string): number {
	const voiceStates = getAllVoiceStatesInChannelFromMediaEngine(guildId ?? ME, channelId);
	const currentConnectionId = getVoiceConnectionContextFromMediaEngine()?.connectionId;
	const currentUserId = Users.getCurrentUser()?.id;
	let count = 0;
	for (const connectionId in voiceStates) {
		const voiceState = voiceStates[connectionId];
		if (!voiceState) continue;
		if (voiceState.connection_id === currentConnectionId) continue;
		if (guildId === null && voiceState.user_id === currentUserId) continue;
		count += 1;
	}
	return count;
}

export function checkChannelLimit(guildId: string | null, channelId: string): boolean {
	const limits: Array<number> = [];
	const userLimit = guildId ? Channels.getChannel(channelId)?.userLimit : undefined;
	if (userLimit && userLimit > 0) limits.push(userLimit);
	if (isChannelP2p(guildId, channelId)) limits.push(VoiceP2pRollout.maxParticipants);
	if (limits.length === 0) return true;
	if (countOtherVoiceConnections(guildId, channelId) < Math.min(...limits)) return true;
	showVoiceChannelFullModal();
	return false;
}

function isEmptyPinnedP2pChannel(guildId: string | null, channelId: string): boolean {
	return (
		VoiceP2pRollout.enabled &&
		guildId !== null &&
		Channels.getChannel(channelId)?.rtcP2p === true &&
		countOtherVoiceConnections(guildId, channelId) === 0
	);
}

export function requestP2pConsent(
	guildId: string | null,
	channelId: string,
	onAgree: () => void,
	onCancel: () => void,
): boolean {
	if (VoiceP2pConsent.isAgreed(channelId)) return true;
	if (VoicePrompts.getSkipP2pJoinConfirm()) {
		VoiceP2pConsent.agree(channelId);
		return true;
	}
	const intent = isEmptyPinnedP2pChannel(guildId, channelId) ? 'start' : 'join';
	ModalCommands.push(
		modal(() => (
			<VoiceP2pConsentModal
				intent={intent}
				allowStandard={false}
				onP2p={() => {
					VoiceP2pConsent.agree(channelId);
					onAgree();
				}}
				onCancel={onCancel}
				data-flx="voice.engine.voice-channel-connector.request-p2p-consent.voice-p2p-consent-modal"
			/>
		)),
	);
	return false;
}

export function checkP2pConsent(
	guildId: string | null,
	channelId: string,
	onAgree: () => void,
	onCancel: () => void,
): boolean {
	if (!isChannelP2p(guildId, channelId) && !isEmptyPinnedP2pChannel(guildId, channelId)) return true;
	return requestP2pConsent(guildId, channelId, onAgree, onCancel);
}

function resolveVoiceConnectionLimit(guildId: string | null, channelId: string): number {
	if (!guildId) return VOICE_CHANNEL_CONNECTION_LIMIT_DEFAULT;
	const channel = Channels.getChannel(channelId);
	return channel?.voiceConnectionLimit ?? VOICE_CHANNEL_CONNECTION_LIMIT_DEFAULT;
}

export function checkMultipleConnections(
	guildId: string | null,
	channelId: string,
	onSwitchDevice: () => Promise<void>,
	onJustJoin: () => void,
	onCancel: () => void,
): boolean {
	const user = Users.getCurrentUser();
	if (!user) return true;
	if (!GatewayConnection.socket) return true;
	const voiceStateGuildId = guildId ?? ME;
	const voiceStates = getAllVoiceStatesInChannelFromMediaEngine(voiceStateGuildId, channelId);
	const currentConnectionId = getVoiceConnectionContextFromMediaEngine()?.connectionId;
	const userStates: Array<VoiceMediaEngineVoiceState> = [];
	for (const connectionId in voiceStates) {
		const voiceState = voiceStates[connectionId];
		if (!voiceState) continue;
		if (voiceState.user_id !== user.id) continue;
		if (voiceState.connection_id === currentConnectionId) continue;
		userStates.push(voiceState);
	}
	if (userStates.length > 0) {
		const connectionLimit = resolveVoiceConnectionLimit(guildId, channelId);
		const allowJustJoin = userStates.length < connectionLimit;
		ModalCommands.push(
			modal(() => (
				<VoiceConnectionConfirmModal
					guildId={guildId}
					channelId={channelId}
					allowJustJoin={allowJustJoin}
					connectionLimit={connectionLimit}
					existingConnectionsCount={userStates.length}
					onSwitchDevice={async () => {
						for (const vs of userStates) {
							if (vs.connection_id) {
								sendVoiceStateDisconnect(guildId, vs.connection_id);
							}
						}
						await onSwitchDevice();
					}}
					onJustJoin={onJustJoin}
					onCancel={onCancel}
					data-flx="voice.engine.voice-channel-connector.check-multiple-connections.voice-connection-confirm-modal"
				/>
			)),
		);
		return false;
	}
	return true;
}

export function sendVoiceStateConnect(
	guildId: string | null,
	channelId: string,
	viewerStreamKeys: ReadonlyArray<string> = [],
): void {
	const socket = GatewayConnection.socket;
	if (!socket) {
		logger.warn('No socket');
		return;
	}
	LocalVoiceState.ensurePermissionMute();
	const snapshot = getVoiceEngineV2SnapshotFromMediaEngine();
	const effectiveSelfMute = snapshot
		? selectVoiceEngineV2AppIntentSelfMuteForVoiceStatePayload(snapshot)
		: LocalVoiceState.getSelfMute();
	const selfMute = applyVoiceSpeakPermissionToSelfMute(guildId, channelId, effectiveSelfMute);
	const connectionId = getVoiceConnectionContextFromMediaEngine()?.connectionId ?? null;
	VoiceP2pConsent.commit(channelId);
	socket.updateVoiceStateExplicit({
		guild_id: guildId,
		channel_id: channelId,
		self_mute: selfMute,
		self_deaf: LocalVoiceState.getSelfDeaf(),
		self_video: false,
		self_stream: false,
		viewer_stream_keys: viewerStreamKeys,
		connection_id: connectionId,
		...(VoiceP2pConsent.isAgreed(channelId) ? {p2p: true} : {}),
	});
}

export function sendVoiceStateDisconnect(guildId: string | null, connectionId: string | null): void {
	const socket = GatewayConnection.socket;
	if (!socket) {
		logger.warn('No socket');
		return;
	}
	socket.updateVoiceStateExplicit({
		guild_id: guildId,
		channel_id: null,
		self_mute: true,
		self_deaf: true,
		self_video: false,
		self_stream: false,
		viewer_stream_keys: [],
		connection_id: connectionId,
	});
}

export function syncVoiceStateToServer(
	guildId: string | null,
	channelId: string,
	connectionId: string,
	partial?: VoiceStateSyncPartial,
	p2p?: false,
): void {
	const socket = GatewayConnection.socket;
	if (!socket) return;
	const snapshot = getVoiceEngineV2SnapshotFromMediaEngine();
	const effectiveSelfMute = snapshot
		? selectVoiceEngineV2AppIntentSelfMuteForVoiceStatePayload(snapshot)
		: LocalVoiceState.getSelfMute();
	const selfMute = applyVoiceSpeakPermissionToSelfMute(guildId, channelId, partial?.self_mute ?? effectiveSelfMute);
	socket.updateVoiceStateExplicit({
		guild_id: guildId,
		channel_id: channelId,
		self_mute: selfMute,
		self_deaf: partial?.self_deaf ?? LocalVoiceState.getSelfDeaf(),
		self_video: partial?.self_video ?? LocalVoiceState.getSelfVideo(),
		self_stream: partial?.self_stream ?? LocalVoiceState.getSelfStream(),
		viewer_stream_keys: partial?.viewer_stream_keys ?? LocalVoiceState.getViewerStreamKeys(),
		connection_id: connectionId,
		...(p2p === undefined ? {} : {p2p}),
	});
}
