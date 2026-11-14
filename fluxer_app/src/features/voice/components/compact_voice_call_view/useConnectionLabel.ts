// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	asVoiceEngineConnectionState,
	VoiceEngineConnectionState,
} from '@app/features/voice/engine/VoiceConnectionStateMachine';
import {CONNECTING_DESCRIPTOR, DISCONNECTED_DESCRIPTOR} from '@app/features/voice/utils/VoiceSharedMessageDescriptors';
import {msg, plural} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {useMemo} from 'react';

const RECONNECTING_DESCRIPTOR = msg({
	message: 'Reconnecting…',
	comment:
		'Overlay status text in the compact / floating voice call tile while reconnecting. Trailing ellipsis indicates in-progress.',
});

export function useConnectionLabel(state: unknown, participantCount: number) {
	const {i18n} = useLingui();
	return useMemo(() => {
		const normalizedState = asVoiceEngineConnectionState(state);
		switch (normalizedState) {
			case VoiceEngineConnectionState.Connecting:
				return i18n._(CONNECTING_DESCRIPTOR);
			case VoiceEngineConnectionState.Reconnecting:
			case VoiceEngineConnectionState.SignalReconnecting:
				return i18n._(RECONNECTING_DESCRIPTOR);
			case VoiceEngineConnectionState.Disconnected:
				return i18n._(DISCONNECTED_DESCRIPTOR);
			default:
				return plural(
					{count: participantCount},
					{
						one: 'In call',
						other: '# in call',
					},
				);
		}
	}, [state, participantCount, i18n.locale]);
}
