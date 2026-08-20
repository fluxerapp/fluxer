// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/voice/components/VoiceE2EEIndicator.module.css';
import MediaEngine, {useMediaEngineVersion} from '@app/features/voice/engine/MediaEngineFacade';
import {computeChannelE2EEStatus} from '@app/features/voice/state/ChannelE2EEStatus';
import {
	VOICE_CALL_E2EE_BROKEN_DESCRIPTOR,
	VOICE_CALL_E2EE_ENCRYPTED_DESCRIPTOR,
	VOICE_CHANNEL_E2EE_BROKEN_DESCRIPTOR,
	VOICE_CHANNEL_E2EE_ENCRYPTED_DESCRIPTOR,
} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

interface VoiceE2EEIndicatorProps {
	guildId: string | null;
	channelId: string;
	variant: 'voice_channel' | 'call';
}

export const VoiceE2EEIndicator = observer(function VoiceE2EEIndicator({
	guildId,
	channelId,
	variant,
}: VoiceE2EEIndicatorProps) {
	const {i18n} = useLingui();
	useMediaEngineVersion();
	void MediaEngine.getAllVoiceStates();
	const gatewayStatus = computeChannelE2EEStatus(guildId, channelId, {emptyChannelStatus: 'encrypted'});
	const status = gatewayStatus;
	if (status === 'none') return null;
	const isEncrypted = status === 'encrypted';
	const descriptor = isEncrypted
		? variant === 'call'
			? VOICE_CALL_E2EE_ENCRYPTED_DESCRIPTOR
			: VOICE_CHANNEL_E2EE_ENCRYPTED_DESCRIPTOR
		: variant === 'call'
			? VOICE_CALL_E2EE_BROKEN_DESCRIPTOR
			: VOICE_CHANNEL_E2EE_BROKEN_DESCRIPTOR;
	return (
		<div
			className={isEncrypted ? styles.indicatorEncrypted : styles.indicatorBroken}
			role="status"
			data-flx={`voice.voice-e2ee-indicator.${isEncrypted ? 'encrypted' : 'broken'}.${variant}`}
		>
			{i18n._(descriptor)}
		</div>
	);
});
