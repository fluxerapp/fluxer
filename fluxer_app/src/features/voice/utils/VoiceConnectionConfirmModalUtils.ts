// SPDX-License-Identifier: AGPL-3.0-or-later

import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import Users from '@app/features/user/state/Users';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import {ME} from '@fluxer/constants/src/AppConstants';
import {useCallback, useMemo} from 'react';

interface VoiceConnectionConfirmModalCallbacks {
	onSwitchDevice: () => void | Promise<void>;
	onJustJoin: () => void;
	onCancel: () => void;
}

export interface VoiceConnectionConfirmModalProps extends VoiceConnectionConfirmModalCallbacks {
	guildId: string | null;
	channelId: string;
	allowJustJoin?: boolean;
	connectionLimit?: number;
	existingConnectionsCount?: number;
}

export interface VoiceConnectionConfirmModalLogicState {
	existingConnectionsCount: number;
	handleSwitchDevice: () => Promise<void>;
	handleJustJoin: () => void;
	handleCancel: () => void;
}

export function useVoiceConnectionConfirmModalLogic({
	guildId,
	channelId,
	onSwitchDevice,
	onJustJoin,
	onCancel,
}: VoiceConnectionConfirmModalProps): VoiceConnectionConfirmModalLogicState {
	const currentUser = Users.currentUser;
	const currentConnectionId = MediaEngine.connectionId;
	const existingConnectionsCount = useMemo(() => {
		if (!currentUser) return 0;
		const resolvedGuildId = guildId ?? ME;
		const voiceStates = MediaEngine.getAllVoiceStatesInChannel(resolvedGuildId, channelId);
		let count = 0;
		for (const connectionId in voiceStates) {
			const voiceState = voiceStates[connectionId];
			if (!voiceState) continue;
			if (voiceState.user_id !== currentUser.id) continue;
			if (!voiceState.connection_id) continue;
			if (voiceState.connection_id === currentConnectionId) continue;
			count += 1;
		}
		return count;
	}, [channelId, currentConnectionId, currentUser, guildId]);
	const handleSwitchDevice = useCallback(async () => {
		ModalCommands.pop();
		await onSwitchDevice();
	}, [onSwitchDevice]);
	const handleJustJoin = useCallback(() => {
		ModalCommands.pop();
		onJustJoin();
	}, [onJustJoin]);
	const handleCancel = useCallback(() => {
		ModalCommands.pop();
		onCancel();
	}, [onCancel]);
	return {
		existingConnectionsCount,
		handleSwitchDevice,
		handleJustJoin,
		handleCancel,
	};
}
