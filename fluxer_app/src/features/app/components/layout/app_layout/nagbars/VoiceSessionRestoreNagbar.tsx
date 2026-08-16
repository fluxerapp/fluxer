// SPDX-License-Identifier: AGPL-3.0-or-later

import {Nagbar} from '@app/features/app/components/layout/Nagbar';
import {NagbarButton} from '@app/features/app/components/layout/NagbarButton';
import {NagbarContent} from '@app/features/app/components/layout/NagbarContent';
import {NAGBAR_TONES, NagbarToneKind} from '@app/features/app/components/layout/NagbarTones';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import Authentication from '@app/features/auth/state/Authentication';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import Guilds from '@app/features/guild/state/Guilds';
import {Logger} from '@app/features/platform/utils/AppLogger';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import CallState from '@app/features/voice/state/CallState';
import VoiceSessionRestore from '@app/features/voice/state/VoiceSessionRestore';
import {getVoiceSessionRestoreChannelDisplayName} from '@app/features/voice/utils/VoiceSessionRestoreUtils';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useState} from 'react';

const COMMUNITY_VOICE_SESSION_RESTORE_MESSAGE_DESCRIPTOR = msg({
	message:
		'{productName} restarted while you were connected to the {channelName} channel in the {communityName} community.',
	comment:
		'Nagbar shown after the app restarts while the user was connected to a community voice channel. {productName} is Fluxer, {channelName} is the voice channel name, and {communityName} is the community name.',
});
const DIRECT_CALL_VOICE_SESSION_RESTORE_MESSAGE_DESCRIPTOR = msg({
	message: '{productName} restarted while you were connected to the call in {directCallName}.',
	comment:
		'Nagbar shown after the app restarts while the user can reconnect to an active DM or group DM call. {productName} is Fluxer and {directCallName} is the DM or group DM display name.',
});
const NOT_NOW_DESCRIPTOR = msg({
	message: 'Not now',
	comment: 'Secondary button on the voice-session-restore nagbar. Dismisses the reconnect prompt.',
});
const RECONNECT_DESCRIPTOR = msg({
	message: 'Reconnect',
	comment: 'Primary button on the voice-session-restore nagbar. Rejoins the previous voice channel or active call.',
});
const logger = new Logger('VoiceSessionRestoreNagbar');

function isDirectCallChannel(channel: Channel): boolean {
	return channel.type === ChannelTypes.DM || channel.type === ChannelTypes.GROUP_DM;
}

function getRestoreNagbarMessage(channel: Channel, i18n: I18n): string | null {
	if (isDirectCallChannel(channel)) {
		if (!CallState.hasActiveCall(channel.id)) return null;
		const directCallName = getVoiceSessionRestoreChannelDisplayName(channel, '');
		return i18n._(DIRECT_CALL_VOICE_SESSION_RESTORE_MESSAGE_DESCRIPTOR, {
			productName: PRODUCT_NAME,
			directCallName,
		});
	}
	if (channel.type !== ChannelTypes.GUILD_VOICE || !channel.guildId) return null;
	const guild = Guilds.getGuild(channel.guildId);
	const channelName = channel.name?.trim() ?? '';
	const communityName = guild?.name.trim() ?? '';
	if (!channelName || !communityName) return null;
	return i18n._(COMMUNITY_VOICE_SESSION_RESTORE_MESSAGE_DESCRIPTOR, {
		productName: PRODUCT_NAME,
		channelName,
		communityName,
	});
}

export const VoiceSessionRestoreNagbar = observer(({isMobile}: {isMobile: boolean}) => {
	const {i18n} = useLingui();
	const [submitting, setSubmitting] = useState(false);
	const snapshot = VoiceSessionRestore.getSnapshotForUser(Authentication.currentUserId);
	const channel = snapshot ? Channels.getChannel(snapshot.channelId) : null;
	const restoreMessage = channel ? getRestoreNagbarMessage(channel, i18n) : null;
	if (!snapshot || !channel || !restoreMessage) {
		return null;
	}
	const handleDismiss = () => {
		VoiceSessionRestore.clearSnapshot();
	};
	const handleReconnect = async () => {
		setSubmitting(true);
		try {
			const restoreOptions = {
				restoreVideo: snapshot.selfVideo,
				restoreStream: snapshot.selfStream,
			};
			if (isDirectCallChannel(channel) && !CallState.hasActiveCall(channel.id)) {
				VoiceSessionRestore.clearSnapshot();
				return;
			}
			await MediaEngine.restoreVoiceSession(snapshot, restoreOptions);
		} catch (error) {
			logger.error('Failed to restore voice session', error);
		} finally {
			setSubmitting(false);
		}
	};
	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor={NAGBAR_TONES[NagbarToneKind.VOICE].backgroundColor}
			textColor={NAGBAR_TONES[NagbarToneKind.VOICE].textColor}
			data-flx="app.app-layout.nagbars.voice-session-restore-nagbar.nagbar"
		>
			<NagbarContent
				isMobile={isMobile}
				message={restoreMessage}
				actions={
					<>
						<NagbarButton
							isMobile={isMobile}
							onClick={handleDismiss}
							disabled={submitting}
							data-flx="app.app-layout.nagbars.voice-session-restore-nagbar.nagbar-button.dismiss"
						>
							{i18n._(NOT_NOW_DESCRIPTOR)}
						</NagbarButton>
						<NagbarButton
							isMobile={isMobile}
							onClick={handleReconnect}
							submitting={submitting}
							data-flx="app.app-layout.nagbars.voice-session-restore-nagbar.nagbar-button.reconnect"
						>
							{i18n._(RECONNECT_DESCRIPTOR)}
						</NagbarButton>
					</>
				}
				data-flx="app.app-layout.nagbars.voice-session-restore-nagbar.nagbar-content"
			/>
		</Nagbar>
	);
});
