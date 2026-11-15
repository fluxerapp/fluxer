// SPDX-License-Identifier: AGPL-3.0-or-later

import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import styles from '@app/features/voice/components/VoiceE2EEIndicator.module.css';
import VoiceMeshPeers from '@app/features/voice/engine/mesh/VoiceMeshPeers';
import VoiceRuntime, {useVoiceRuntimeVersion} from '@app/features/voice/engine/VoiceRuntime';
import {computeChannelE2EEStatus} from '@app/features/voice/state/ChannelE2EEStatus';
import {isChannelP2p} from '@app/features/voice/state/ChannelP2pStatus';
import {
	VOICE_CALL_E2EE_BROKEN_DESCRIPTOR,
	VOICE_CALL_E2EE_ENCRYPTED_DESCRIPTOR,
	VOICE_CHANNEL_E2EE_BROKEN_DESCRIPTOR,
	VOICE_CHANNEL_E2EE_ENCRYPTED_DESCRIPTOR,
	VOICE_P2P_INDICATOR_DESCRIPTOR,
	VOICE_P2P_INDICATOR_TOOLTIP_DESCRIPTOR,
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
	useVoiceRuntimeVersion();
	void VoiceRuntime.getAllVoiceStates();
	if (isChannelP2p(guildId, channelId)) {
		const isBroken = VoiceRuntime.channelId === channelId && VoiceMeshPeers.hasFailedPeer;
		return (
			<Tooltip
				text={i18n._(VOICE_P2P_INDICATOR_TOOLTIP_DESCRIPTOR)}
				position="top"
				data-flx="voice.voice-e2ee-indicator.p2p-tooltip"
			>
				<div
					className={isBroken ? styles.indicatorP2pBroken : styles.indicatorP2p}
					role="status"
					data-flx={`voice.voice-e2ee-indicator.p2p.${isBroken ? 'broken' : 'connected'}.${variant}`}
				>
					{i18n._(VOICE_P2P_INDICATOR_DESCRIPTOR)}
				</div>
			</Tooltip>
		);
	}
	const dmCallEncrypted = VoiceRuntime.channelId === channelId && VoiceRuntime.room?.isE2EEEnabled === true;
	const knownEncrypted = guildId !== null || dmCallEncrypted;
	const status = knownEncrypted
		? computeChannelE2EEStatus(guildId, channelId, {emptyChannelStatus: guildId === null ? 'none' : 'encrypted'})
		: 'none';
	if (status === 'none') return null;
	const isEncrypted = status === 'encrypted';
	const encryptedDescriptor =
		variant === 'call' ? VOICE_CALL_E2EE_ENCRYPTED_DESCRIPTOR : VOICE_CHANNEL_E2EE_ENCRYPTED_DESCRIPTOR;
	const brokenDescriptor =
		variant === 'call' ? VOICE_CALL_E2EE_BROKEN_DESCRIPTOR : VOICE_CHANNEL_E2EE_BROKEN_DESCRIPTOR;
	const descriptor = isEncrypted ? encryptedDescriptor : brokenDescriptor;
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
