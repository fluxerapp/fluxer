// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import {createNamedLoadableComponent} from '@app/features/platform/components/loadable/LoadableComponent';
import {PopoutWindow} from '@app/features/voice/components/popout/PopoutWindow';
import styles from '@app/features/voice/components/popout/VoicePopoutHost.module.css';
import {VoicePopoutScopeContext} from '@app/features/voice/components/popout/VoicePopoutScopeContext';
import {useVoiceEngineConnectionState} from '@app/features/voice/components/useVoiceEngineConnectionState';
import {VoiceCallView} from '@app/features/voice/components/VoiceCallView';
import {
	asVoiceEngineConnectionState,
	VoiceEngineConnectionState,
} from '@app/features/voice/engine/VoiceConnectionStateMachine';
import PopoutWindowManager, {
	isVoicePopoutAlwaysOnTopSupported,
	VOICE_CALL_POPOUT_DEFAULT_HEIGHT,
	VOICE_CALL_POPOUT_DEFAULT_WIDTH,
	VOICE_TILE_POPOUT_DEFAULT_HEIGHT,
	VOICE_TILE_POPOUT_DEFAULT_WIDTH,
	type VoiceCallPopoutDescriptor,
	type VoicePopoutDescriptor,
	type VoiceTilePopoutDescriptor,
} from '@app/features/voice/state/PopoutWindowManager';
import {VOICE_CALL_DESCRIPTOR} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect} from 'react';

const VoiceTilePopoutContent = createNamedLoadableComponent<{descriptor: VoiceTilePopoutDescriptor}>({
	displayName: 'VoiceTilePopoutContent',
	load: async () =>
		(await import('@app/features/voice/components/popout/VoiceTilePopoutContent')).VoiceTilePopoutContent,
});

const VoiceCallPopoutContent = observer(function VoiceCallPopoutContent({
	descriptor,
}: {
	descriptor: VoiceCallPopoutDescriptor;
}) {
	const channel = Channels.getChannel(descriptor.channelId);
	useEffect(() => {
		if (channel) return;
		PopoutWindowManager.close(descriptor.key, descriptor.generation);
	}, [channel, descriptor.generation, descriptor.key]);
	if (!channel) return null;
	return (
		<div className={styles.callContent} data-flx="voice.voice-popout-host.call-content">
			<VoiceCallView channel={channel} inPopout data-flx="voice.voice-popout-host.voice-call-view" />
		</div>
	);
});

const VoicePopoutWindowRenderer = observer(function VoicePopoutWindowRenderer({
	descriptor,
}: {
	descriptor: VoicePopoutDescriptor;
}) {
	const key = descriptor.key;
	const handleClosed = useCallback(
		(windowGeneration: number) => {
			PopoutWindowManager.handleWindowClosed(key, windowGeneration);
		},
		[key],
	);
	const handleRestore = useCallback(() => {
		PopoutWindowManager.close(key, descriptor.generation);
	}, [descriptor.generation, key]);
	const handleToggleAlwaysOnTop = useCallback(() => {
		PopoutWindowManager.toggleAlwaysOnTop(key);
	}, [key]);
	const handleWindowOpened = useCallback(
		(childWindow: Window, windowGeneration: number) => {
			PopoutWindowManager.attachWindow(key, windowGeneration, childWindow);
		},
		[key],
	);
	const {i18n} = useLingui();
	const isCallPopout = descriptor.kind === 'call';
	const callChannelName = isCallPopout ? Channels.getChannel(descriptor.channelId)?.name : null;
	const title = isCallPopout ? (callChannelName ?? i18n._(VOICE_CALL_DESCRIPTOR)) : descriptor.title;
	return (
		<PopoutWindow
			windowKey={key}
			windowGeneration={descriptor.generation}
			title={title}
			showTitlebarTitle={!isCallPopout}
			width={isCallPopout ? VOICE_CALL_POPOUT_DEFAULT_WIDTH : VOICE_TILE_POPOUT_DEFAULT_WIDTH}
			height={isCallPopout ? VOICE_CALL_POPOUT_DEFAULT_HEIGHT : VOICE_TILE_POPOUT_DEFAULT_HEIGHT}
			isAlwaysOnTop={PopoutWindowManager.isAlwaysOnTop(key)}
			onToggleAlwaysOnTop={isVoicePopoutAlwaysOnTopSupported() ? handleToggleAlwaysOnTop : undefined}
			onRestore={handleRestore}
			onClosed={handleClosed}
			onWindowOpened={handleWindowOpened}
			data-flx="voice.voice-popout-host.popout-window"
		>
			<VoicePopoutScopeContext.Provider value={descriptor.kind}>
				{descriptor.kind === 'tile' ? (
					<VoiceTilePopoutContent
						descriptor={descriptor}
						data-flx="voice.voice-popout-host.voice-tile-popout-content"
					/>
				) : (
					<VoiceCallPopoutContent
						descriptor={descriptor}
						data-flx="voice.voice-popout-host.voice-call-popout-content"
					/>
				)}
			</VoicePopoutScopeContext.Provider>
		</PopoutWindow>
	);
});

export const VoicePopoutHost: React.FC = observer(function VoicePopoutHost() {
	const connectionState = asVoiceEngineConnectionState(useVoiceEngineConnectionState());
	const hasOpenPopouts = PopoutWindowManager.openPopoutCount > 0;
	const hasConnectionBoundPopouts = PopoutWindowManager.openPopouts.some(
		(descriptor) => descriptor.kind === 'call' || descriptor.source !== 'user',
	);
	useEffect(() => {
		if (connectionState !== VoiceEngineConnectionState.Disconnected) return;
		if (!hasConnectionBoundPopouts) return;
		PopoutWindowManager.closeConnectionBoundPopouts();
	}, [connectionState, hasConnectionBoundPopouts]);
	if (!hasOpenPopouts) return null;
	return (
		<>
			{PopoutWindowManager.openPopouts.map((descriptor) => (
				<VoicePopoutWindowRenderer
					key={`${descriptor.key}:${descriptor.generation}`}
					descriptor={descriptor}
					data-flx="voice.voice-popout-host.voice-popout-window-renderer"
				/>
			))}
		</>
	);
});
