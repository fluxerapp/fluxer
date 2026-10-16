// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import GuildMatureContentAgree, {MatureContentGateReason} from '@app/features/guild/state/GuildMatureContentAgree';
import Guilds from '@app/features/guild/state/Guilds';
import {getEffectiveChannelMatureContent} from '@app/features/messaging/utils/ContentWarningUtils';
import Relationships from '@app/features/relationship/state/Relationships';
import UserSettings from '@app/features/user/state/UserSettings';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {RelationshipTypes, SensitiveMediaFilterLevel} from '@fluxer/constants/src/UserConstants';
import {useCallback, useEffect, useState} from 'react';

interface MatureMediaResult {
	shouldBlur: boolean;
	shouldBlock: boolean;
	gateReason: MatureContentGateReason;
	canReveal: boolean;
	reveal: () => void;
}

function getSensitiveFilterLevel(channelId: string | undefined): number {
	if (!channelId) {
		return SensitiveMediaFilterLevel.SHOW;
	}
	const requestedChannel = Channels.getChannel(channelId);
	const channel =
		requestedChannel?.isThread() && requestedChannel.parentId
			? Channels.getChannel(requestedChannel.parentId)
			: requestedChannel;
	if (!channel) {
		return SensitiveMediaFilterLevel.SHOW;
	}
	if (channel.type === ChannelTypes.DM || channel.type === ChannelTypes.GROUP_DM) {
		if (channel.type === ChannelTypes.DM && channel.recipientIds.length > 0) {
			const recipientId = channel.recipientIds[0];
			const relationship = Relationships.getRelationship(recipientId);
			if (relationship?.type === RelationshipTypes.FRIEND) {
				return UserSettings.sensitiveContentFriendDmFilter;
			}
		}
		return UserSettings.sensitiveContentNonFriendDmFilter;
	}
	if (channel.guildId) {
		const guild = Guilds.getGuild(channel.guildId);
		if (getEffectiveChannelMatureContent(channel, guild)) {
			return SensitiveMediaFilterLevel.SHOW;
		}
		return UserSettings.sensitiveContentGuildFilter;
	}
	return SensitiveMediaFilterLevel.SHOW;
}

export function useMatureMedia(mature: boolean | undefined, channelId: string | undefined): MatureMediaResult {
	const mockMatureMediaGateReason = DeveloperOptions.mockMatureMediaGateReason;
	const forceMatureMedia = DeveloperOptions.forceMatureMedia;
	const [isRevealed, setIsRevealed] = useState(false);
	const hasGateableContext = channelId ? GuildMatureContentAgree.isGatedContent({channelId}) : false;
	const effectiveMatureMedia =
		forceMatureMedia || mockMatureMediaGateReason !== 'none' || !!mature || hasGateableContext;
	const gateReasonFromState = GuildMatureContentAgree.getGateReason({channelId: channelId ?? null});
	let gateReason: MatureContentGateReason;
	if (mockMatureMediaGateReason !== 'none') {
		gateReason =
			mockMatureMediaGateReason === 'geo_restricted'
				? MatureContentGateReason.GEO_RESTRICTED
				: MatureContentGateReason.MATURE_CONTENT_CHECK_REQUIRED;
	} else if (effectiveMatureMedia && channelId) {
		gateReason = gateReasonFromState;
	} else {
		gateReason = MatureContentGateReason.NONE;
	}
	const filterLevel = getSensitiveFilterLevel(channelId);
	const isFilterBlur = effectiveMatureMedia && filterLevel === SensitiveMediaFilterLevel.BLUR;
	const isFilterBlock = effectiveMatureMedia && filterLevel === SensitiveMediaFilterLevel.BLOCK;
	const isGateBlur =
		effectiveMatureMedia &&
		gateReason !== MatureContentGateReason.NONE &&
		gateReason !== MatureContentGateReason.CONSENT_REQUIRED;
	const canReveal = isFilterBlur && !isGateBlur;
	const shouldBlur = (isGateBlur || isFilterBlur) && !(canReveal && isRevealed);
	const shouldBlock: boolean = isFilterBlock;
	useEffect(() => {
		setIsRevealed(false);
	}, [channelId, gateReason, mockMatureMediaGateReason, forceMatureMedia, mature, filterLevel]);
	const reveal = useCallback(() => {
		if (!canReveal) return;
		setIsRevealed(true);
	}, [canReveal]);
	return {shouldBlur, gateReason, shouldBlock, canReveal, reveal};
}
