// SPDX-License-Identifier: AGPL-3.0-or-later

import {SoundType} from '@app/features/notification/utils/SoundUtils';
import Permission from '@app/features/permissions/state/Permission';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as SoundCommands from '@app/features/ui/commands/SoundCommands';
import {useStoreVersion} from '@app/features/voice/engine/Store';
import {selectVoiceMediaGraphViewerStreamKeys} from '@app/features/voice/engine/VoiceMediaGraph';
import {voiceMediaGraphStore} from '@app/features/voice/engine/VoiceMediaGraphStore';
import VoiceRuntime, {useVoiceRuntimeVersion} from '@app/features/voice/engine/VoiceRuntime';
import {
	addWatchedStreamKey,
	replaceWatchedStreamKeys,
	stopWatchingStreamKey,
} from '@app/features/voice/engine/VoiceStreamWatchState';
import {usePendingVoiceConnection} from '@app/features/voice/hooks/usePendingVoiceConnection';
import {useVoiceJoinEligibility} from '@app/features/voice/hooks/useVoiceJoinEligibility';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {useCallback, useMemo} from 'react';

interface StreamWatchState {
	isWatching: boolean;
	isPendingJoin: boolean;
	canWatch: boolean;
	startWatching: () => void;
	addStream: () => void;
	stopWatching: () => void;
}

const logger = new Logger('useStreamWatchState');

export function useStreamWatchState({
	streamKey,
	guildId,
	channelId,
}: {
	streamKey: string;
	guildId: string | null | undefined;
	channelId: string | null | undefined;
}): StreamWatchState {
	useVoiceRuntimeVersion();
	useStoreVersion(voiceMediaGraphStore);
	const {canJoin} = useVoiceJoinEligibility({
		guildId: guildId ?? null,
		channelId: channelId ?? null,
	});
	const isConnectedToChannel = useMemo(() => {
		if (!channelId) return false;
		return VoiceRuntime.channelId === channelId && VoiceRuntime.guildId === (guildId ?? null);
	}, [channelId, guildId, VoiceRuntime.channelId, VoiceRuntime.guildId]);
	const replaceViewerStreamKeys = useCallback(
		(keys: Array<string>) => {
			const result = replaceWatchedStreamKeys(keys);
			logger.debug('Replacing viewer stream keys', {
				streamKey,
				isConnectedToChannel,
				previousKeys: result.previousKeys,
				keys: result.keys,
			});
			if (!result.membershipChanged) return;
			if (result.hadStreams && !result.hasStreams) {
				SoundCommands.playSound(SoundType.ViewerLeave);
				return;
			}
			if (!result.hadStreams && result.hasStreams) {
				SoundCommands.playSound(SoundType.ViewerJoin);
				return;
			}
			if (result.hadStreams && result.hasStreams) {
				SoundCommands.playSound(SoundType.ViewerJoin);
			}
		},
		[isConnectedToChannel, streamKey],
	);
	const handleStreamConnected = useCallback(() => {
		replaceViewerStreamKeys([streamKey]);
	}, [streamKey, replaceViewerStreamKeys]);
	const {
		isPending: isPendingJoin,
		startConnection,
		cancel: cancelPendingJoin,
	} = usePendingVoiceConnection({
		guildId,
		channelId,
		onConnected: handleStreamConnected,
	});
	const isWatching =
		isConnectedToChannel &&
		selectVoiceMediaGraphViewerStreamKeys(voiceMediaGraphStore.getGraphSnapshot()).includes(streamKey);
	const hasConnectPermission = !guildId || !channelId || Permission.can(Permissions.CONNECT, {guildId, channelId});
	const canWatch = isConnectedToChannel || (canJoin && hasConnectPermission);
	const startWatching = useCallback(() => {
		if (!streamKey) return;
		if (isConnectedToChannel) {
			logger.debug('Starting stream watch in active call', {
				streamKey,
				channelId,
				guildId,
				previousKeys: selectVoiceMediaGraphViewerStreamKeys(voiceMediaGraphStore.getGraphSnapshot()),
			});
			replaceViewerStreamKeys([streamKey]);
			return;
		}
		if (!channelId || !canJoin || !hasConnectPermission) return;
		logger.debug('Starting stream watch with pending voice join', {streamKey, channelId, guildId});
		startConnection({initialViewerStreamKeys: [streamKey], skipConfirm: true});
	}, [
		isConnectedToChannel,
		streamKey,
		channelId,
		canJoin,
		hasConnectPermission,
		startConnection,
		replaceViewerStreamKeys,
		guildId,
	]);
	const addStream = useCallback(() => {
		if (!streamKey) return;
		if (!isConnectedToChannel) {
			logger.debug('Add stream requested while disconnected, falling back to startWatching', {
				streamKey,
				channelId,
				guildId,
			});
			startWatching();
			return;
		}
		const result = addWatchedStreamKey(streamKey);
		logger.debug('Adding stream to watcher set', {
			streamKey,
			channelId,
			guildId,
			previousKeys: result.previousKeys,
			updatedKeys: result.keys,
		});
		if (!result.membershipChanged) return;
		SoundCommands.playSound(SoundType.ViewerJoin);
	}, [streamKey, isConnectedToChannel, startWatching, channelId, guildId]);
	const stopWatching = useCallback(() => {
		cancelPendingJoin();
		if (!streamKey) return;
		logger.debug('Stopping stream watch', {
			streamKey,
			channelId,
			guildId,
			previousKeys: selectVoiceMediaGraphViewerStreamKeys(voiceMediaGraphStore.getGraphSnapshot()),
		});
		if (stopWatchingStreamKey(streamKey, {guildId, channelId})) {
			SoundCommands.playSound(SoundType.ViewerLeave);
		}
	}, [streamKey, cancelPendingJoin, channelId, guildId]);
	return {isWatching, isPendingJoin, canWatch, startWatching, addStream, stopWatching};
}
