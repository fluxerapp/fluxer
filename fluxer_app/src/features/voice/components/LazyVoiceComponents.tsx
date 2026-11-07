// SPDX-License-Identifier: AGPL-3.0-or-later

import {createNamedLoadableComponent} from '@app/features/platform/components/loadable/LoadableComponent';
import type {DirectCallLobbyBottomSheet} from '@app/features/voice/components/bottomsheets/DirectCallLobbyBottomSheet';
import type {CompactVoiceCallStreamHeaderInfo} from '@app/features/voice/components/CompactVoiceCallStreamHeaderInfo';
import type {CompactVoiceCallViewProps} from '@app/features/voice/components/compact_voice_call_view/shared';
import type {CameraPreviewModalInRoom} from '@app/features/voice/components/modals/CameraPreviewModal';
import type {StreamSettingsMenuContent} from '@app/features/voice/components/StreamSettingsMenuContent';
import type {VoiceCallView} from '@app/features/voice/components/VoiceCallView';
import type {VoiceAudioSettingsMenu} from '@app/features/voice/components/VoiceSettingsMenus';
import type React from 'react';

type NoProps = Record<string, never>;

export const LazyVoiceCallView = createNamedLoadableComponent<React.ComponentProps<typeof VoiceCallView>>({
	displayName: 'LazyVoiceCallView',
	load: async () => (await import('@app/features/voice/components/VoiceCallView')).VoiceCallView,
});

export const LazyCompactVoiceCallStreamHeaderInfo = createNamedLoadableComponent<
	React.ComponentProps<typeof CompactVoiceCallStreamHeaderInfo>
>({
	displayName: 'LazyCompactVoiceCallStreamHeaderInfo',
	load: async () =>
		(await import('@app/features/voice/components/CompactVoiceCallStreamHeaderInfo')).CompactVoiceCallStreamHeaderInfo,
});

export const LazyCompactVoiceCallViewInner = createNamedLoadableComponent<CompactVoiceCallViewProps>({
	displayName: 'LazyCompactVoiceCallViewInner',
	load: async () =>
		(await import('@app/features/voice/components/compact_voice_call_view/CompactVoiceCallViewInner'))
			.CompactVoiceCallViewInner,
});

export const LazyDirectCallLobbyBottomSheet = createNamedLoadableComponent<
	React.ComponentProps<typeof DirectCallLobbyBottomSheet>
>({
	displayName: 'LazyDirectCallLobbyBottomSheet',
	load: async () =>
		(await import('@app/features/voice/components/bottomsheets/DirectCallLobbyBottomSheet')).DirectCallLobbyBottomSheet,
});

export const LazyVoiceAudioSettingsMenu = createNamedLoadableComponent<
	React.ComponentProps<typeof VoiceAudioSettingsMenu>
>({
	displayName: 'LazyVoiceAudioSettingsMenu',
	load: async () => (await import('@app/features/voice/components/VoiceSettingsMenus')).VoiceAudioSettingsMenu,
});

export const LazyVoiceConnectionStatusInner = createNamedLoadableComponent<NoProps>({
	displayName: 'LazyVoiceConnectionStatusInner',
	load: async () =>
		(await import('@app/features/voice/components/voice_connection_status/VoiceConnectionStatusInner'))
			.VoiceConnectionStatusInner,
});

export const LazyVoiceReconnectionManager = createNamedLoadableComponent<NoProps>({
	displayName: 'LazyVoiceReconnectionManager',
	load: async () => (await import('@app/features/voice/components/VoiceReconnectionManager')).VoiceReconnectionManager,
});

export const LazyPiPOverlay = createNamedLoadableComponent<NoProps>({
	displayName: 'LazyPiPOverlay',
	load: async () => (await import('@app/features/voice/components/PiPOverlay')).PiPOverlay,
});

export const LazyVoicePopoutHost = createNamedLoadableComponent<NoProps>({
	displayName: 'LazyVoicePopoutHost',
	load: async () => (await import('@app/features/voice/components/popout/VoicePopoutHost')).VoicePopoutHost,
});

export const LazyStreamSettingsMenuContent = createNamedLoadableComponent<
	React.ComponentProps<typeof StreamSettingsMenuContent>
>({
	displayName: 'LazyStreamSettingsMenuContent',
	load: async () =>
		(await import('@app/features/voice/components/StreamSettingsMenuContent')).StreamSettingsMenuContent,
});

export const LazyCameraPreviewModalInRoom = createNamedLoadableComponent<
	React.ComponentProps<typeof CameraPreviewModalInRoom>
>({
	displayName: 'LazyCameraPreviewModalInRoom',
	load: async () => (await import('@app/features/voice/components/modals/CameraPreviewModal')).CameraPreviewModalInRoom,
});

export async function openScreenSharePreviewPrivacyModal(): Promise<void> {
	const {openScreenSharePreviewPrivacyModal: open} = await import(
		'@app/features/voice/components/modals/ScreenSharePickerModal'
	);
	open();
}
