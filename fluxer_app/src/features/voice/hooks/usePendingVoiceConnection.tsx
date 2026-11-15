// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import VoiceRuntime, {useVoiceRuntimeVersion} from '@app/features/voice/engine/VoiceRuntime';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {useCallback, useEffect, useRef, useState} from 'react';

const JOIN_VOICE_CHANNEL_DESCRIPTOR = msg({
	message: 'Join voice channel',
	comment: 'Confirm dialog title shown before joining a voice channel from a context where confirmation is required.',
});
const ARE_YOU_SURE_YOU_WANT_TO_JOIN_THIS_DESCRIPTOR = msg({
	message: 'Join this voice channel?',
	comment: 'Confirm dialog body shown before joining a voice channel from a context where confirmation is required.',
});

interface PendingVoiceConnectionResult {
	isPending: boolean;
	startConnection: (options?: PendingVoiceConnectionStartOptions) => void;
	markPending: () => void;
	cancel: () => void;
}

interface PendingVoiceConnectionStartOptions {
	initialViewerStreamKeys?: Array<string>;
	skipConfirm?: boolean;
}

export function usePendingVoiceConnection({
	guildId,
	channelId,
	onConnected,
}: {
	guildId: string | null | undefined;
	channelId: string | null | undefined;
	onConnected?: () => void;
}): PendingVoiceConnectionResult {
	const {i18n} = useLingui();
	useVoiceRuntimeVersion();
	const [isPending, setIsPending] = useState(false);
	const onConnectedRef = useRef(onConnected);
	onConnectedRef.current = onConnected;
	const isConnectedToTarget = channelId
		? VoiceRuntime.channelId === channelId && VoiceRuntime.guildId === (guildId ?? null)
		: false;
	const isConnected = VoiceRuntime.connected;
	const isConnecting = VoiceRuntime.connecting;
	const connectFailed = VoiceRuntime.connectFailed;
	const isTargetConnecting = isConnectedToTarget && isConnecting;
	useEffect(() => {
		if (!isPending) return;
		if (connectFailed) {
			setIsPending(false);
			return;
		}
		if (isConnectedToTarget && isConnected) {
			setIsPending(false);
			onConnectedRef.current?.();
			return;
		}
		if (!isConnecting && !isConnectedToTarget) {
			setIsPending(false);
		}
	}, [isPending, isConnectedToTarget, isConnected, isConnecting, connectFailed]);
	const connect = useCallback(
		(options?: PendingVoiceConnectionStartOptions) => {
			if (!channelId) return;
			setIsPending(true);
			void VoiceRuntime.withEngine('join', (engine) =>
				engine.connectToVoiceChannel(guildId ?? null, channelId, {
					initialViewerStreamKeys: options?.initialViewerStreamKeys,
				}),
			);
		},
		[guildId, channelId],
	);
	const startConnection = useCallback(
		(options?: PendingVoiceConnectionStartOptions) => {
			if (!channelId) return;
			if (isConnectedToTarget && (isConnected || isConnecting)) {
				setIsPending(isConnecting);
				return;
			}
			if (guildId && Accessibility.confirmBeforeJoiningVoiceChannels && !options?.skipConfirm) {
				ModalCommands.push(
					modal(() => (
						<ConfirmModal
							title={i18n._(JOIN_VOICE_CHANNEL_DESCRIPTOR)}
							description={i18n._(ARE_YOU_SURE_YOU_WANT_TO_JOIN_THIS_DESCRIPTOR)}
							primaryText={i18n._(JOIN_VOICE_CHANNEL_DESCRIPTOR)}
							primaryVariant="primary"
							onPrimary={() => {
								setTimeout(() => connect(options), 0);
							}}
							data-flx="voice.use-pending-voice-connection.start-connection.confirm-modal"
						/>
					)),
				);
				return;
			}
			connect(options);
		},
		[channelId, connect, guildId, i18n, isConnected, isConnectedToTarget, isConnecting],
	);
	const markPending = useCallback(() => {
		setIsPending(true);
	}, []);
	const cancel = useCallback(() => {
		setIsPending(false);
	}, []);
	return {isPending: isPending || isTargetConnecting, startConnection, markPending, cancel};
}
