// SPDX-License-Identifier: AGPL-3.0-or-later

import i18nGlobal from '@app/app/I18n';
import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {promptForAccountTier} from '@app/features/auth/utils/AccountTierPrompt';
import Channels from '@app/features/channel/state/Channels';
import type {Guild} from '@app/features/guild/models/Guild';
import {showGuildJoinRefusal} from '@app/features/invite/commands/InviteCommands';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {failureCode} from '@app/features/platform/utils/ResponseInspection';
import * as PremiumCommands from '@app/features/premium/commands/PremiumCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {GUILD_TEXT_BASED_CHANNEL_TYPES} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useCallback, useState} from 'react';

const REJOIN_NO_ACCESS_TITLE_DESCRIPTOR = msg({
	message: 'This community is for Visionary members',
	comment: 'Title of the error modal shown when a non-Visionary user tries to rejoin a gated Plutonium community.',
});
const REJOIN_NO_ACCESS_MESSAGE_DESCRIPTOR = msg({
	message: 'Only Visionary members can join this community. Your access may have changed since this screen was opened.',
	comment: 'Body of the error modal shown when a non-Visionary user tries to rejoin a gated Plutonium community.',
});
const REJOIN_VISIONARY_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Couldn't rejoin the Visionary community",
	comment: 'Title of the generic fallback error modal shown when rejoining the Visionary community fails.',
});
const REJOIN_FAILED_MESSAGE_DESCRIPTOR = msg({
	message: 'Something went wrong while rejoining. Please try again in a moment.',
	comment: 'Body of the generic fallback error modal shown when rejoining a gated Plutonium community fails.',
});

function showRejoinCommunityErrorModal(error: unknown): void {
	const code = failureCode(error);
	const isNoAccess = code === APIErrorCodes.MISSING_ACCESS;
	ModalCommands.push(
		modal(() => {
			const title = isNoAccess
				? i18nGlobal._(REJOIN_NO_ACCESS_TITLE_DESCRIPTOR)
				: i18nGlobal._(REJOIN_VISIONARY_FAILED_TITLE_DESCRIPTOR);
			const message = isNoAccess
				? i18nGlobal._(REJOIN_NO_ACCESS_MESSAGE_DESCRIPTOR)
				: i18nGlobal._(REJOIN_FAILED_MESSAGE_DESCRIPTOR);
			return (
				<GenericErrorModal
					title={title}
					message={message}
					data-flx="app.plutonium.use-community-actions.rejoin-community.generic-error-modal"
				/>
			);
		}),
	);
}
const logger = new Logger('useCommunityActions');
export const useCommunityActions = (visionaryGuild: Guild | undefined) => {
	const [loadingRejoinCommunity, setLoadingRejoinCommunity] = useState(false);
	const getFirstViewableChannel = useCallback((guildId: string) => {
		const channels = Channels.getGuildChannels(guildId);
		return channels.find((channel) => GUILD_TEXT_BASED_CHANNEL_TYPES.has(channel.type));
	}, []);
	const handleRejoinCommunity = useCallback(async () => {
		setLoadingRejoinCommunity(true);
		try {
			await PremiumCommands.rejoinVisionaryGuild();
			if (visionaryGuild) {
				const firstChannel = getFirstViewableChannel(visionaryGuild.id);
				ModalCommands.popAll();
				NavigationCommands.selectChannel(visionaryGuild.id, firstChannel?.id);
			}
		} catch (error) {
			logger.error('Failed to rejoin visionary guild', error);
			if (!promptForAccountTier(error) && !showGuildJoinRefusal(error)) {
				showRejoinCommunityErrorModal(error);
			}
		} finally {
			setLoadingRejoinCommunity(false);
		}
	}, [visionaryGuild, getFirstViewableChannel]);
	const handleCommunityButtonClick = useCallback(() => {
		if (visionaryGuild) {
			const firstChannel = getFirstViewableChannel(visionaryGuild.id);
			ModalCommands.popAll();
			NavigationCommands.selectChannel(visionaryGuild.id, firstChannel?.id);
		} else {
			void handleRejoinCommunity();
		}
	}, [visionaryGuild, getFirstViewableChannel, handleRejoinCommunity]);
	return {
		loadingRejoinCommunity,
		handleCommunityButtonClick,
	};
};
