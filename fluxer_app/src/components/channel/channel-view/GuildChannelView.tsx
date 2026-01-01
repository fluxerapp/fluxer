/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {observer} from 'mobx-react-lite';
import React from 'react';
import * as MessageActionCreators from '~/actions/MessageActionCreators';
import {ChannelTypes} from '~/Constants';
import {
	AccountTooNewBarrier,
	NoPhoneNumberBarrier,
	NotMemberLongEnoughBarrier,
	SendMessageDisabledBarrier,
	UnclaimedAccountBarrier,
	UnverifiedEmailBarrier,
} from '~/components/channel/barriers/BarrierComponents';
import {ChannelChatLayout} from '~/components/channel/ChannelChatLayout';
import {ChannelHeader} from '~/components/channel/ChannelHeader';
import {ChannelMembers} from '~/components/channel/ChannelMembers';
import {ChannelTextarea} from '~/components/channel/ChannelTextarea';
import {NSFWChannelGate} from '~/components/channel/NSFWChannelGate';
import {VerificationBarrier} from '~/components/channel/VerificationBarrier';
import {Button} from '~/components/uikit/Button/Button';
import {VoiceCallView} from '~/components/voice/VoiceCallView';
import {useChannelMemberListVisibility} from '~/hooks/useChannelMemberListVisibility';
import {useChannelSearchVisibility} from '~/hooks/useChannelSearchVisibility';
import {useFluxerDocumentTitle} from '~/hooks/useFluxerDocumentTitle';
import {useMemberListVisible} from '~/hooks/useMemberListVisible';
import {useLocation} from '~/lib/router';
import ChannelStore from '~/stores/ChannelStore';
import DeveloperOptionsStore from '~/stores/DeveloperOptionsStore';
import GuildNSFWAgreeStore, {NSFWGateReason} from '~/stores/GuildNSFWAgreeStore';
import GuildStore from '~/stores/GuildStore';
import GuildVerificationStore from '~/stores/GuildVerificationStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';
import styles from '../ChannelIndexPage.module.css';
import {ChannelSearchResults} from '../ChannelSearchResults';
import {Messages} from '../Messages';
import {ChannelViewScaffold} from './ChannelViewScaffold';
import {useChannelSearchState} from './useChannelSearchState';

interface GuildChannelViewProps {
	channelId: string;
	guildId?: string | null;
	messageId?: string;
}

export const GuildChannelView = observer(({channelId, guildId, messageId}: GuildChannelViewProps) => {
	const location = useLocation();
	const channel = ChannelStore.getChannel(channelId);
	const guild = guildId ? GuildStore.getGuild(guildId) : null;
	const isMemberListVisible = useMemberListVisible();
	const {enabled: isMobileLayout} = MobileLayoutStore;
	const room = MediaEngineStore.room;
	const connectedChannelId = MediaEngineStore.channelId;
	const nsfwGateReason = GuildNSFWAgreeStore.getGateReason(channelId);
	const showNSFWGate = nsfwGateReason !== NSFWGateReason.NONE;
	const forceMockNSFWGate = DeveloperOptionsStore.mockNSFWGateReason !== 'none';
	const searchState = useChannelSearchState(channel);
	const {
		isSearchActive,
		handleSearchClose,
		handleSearchSubmit,
		searchRefreshKey,
		activeSearchQuery,
		activeSearchSegments,
	} = searchState;

	useChannelSearchVisibility(channelId, isSearchActive);
	useChannelMemberListVisibility(channelId, isMemberListVisible && !isMobileLayout);

	React.useEffect(() => {
		if (messageId && channelId) {
			MessageActionCreators.jumpToMessage(channelId, messageId, true);
		}
	}, [location.pathname, channelId, messageId]);

	React.useEffect(() => {
		const handleGlobalKeydown = (event: KeyboardEvent) => {
			if (event.key === 'Escape' && isSearchActive) {
				searchState.setIsSearchActive(false);
			}
		};

		document.addEventListener('keydown', handleGlobalKeydown, {capture: true});
		return () => {
			document.removeEventListener('keydown', handleGlobalKeydown, {capture: true} as any);
		};
	}, [isSearchActive, searchState]);

	const channelTitlePart = channel
		? `${channel.type === ChannelTypes.GUILD_VOICE ? '' : '#'}${channel.name ?? ''}`
		: null;
	const guildTitlePart = guild ? guild.name : null;
	useFluxerDocumentTitle(channel ? [channelTitlePart, guildTitlePart] : undefined);

	if (!(guild && channel)) {
		return null;
	}

	const isVoiceChannel = channel.type === ChannelTypes.GUILD_VOICE;
	const isConnectedToThisChannel = isVoiceChannel && connectedChannelId === channelId && room;

	const passesVerification = channel.isPrivate() || GuildVerificationStore.canAccessGuild(channel.guildId || '');

	const renderChatArea = () => {
		if (DeveloperOptionsStore.mockVerificationBarrier !== 'none' && !channel.isPrivate()) {
			switch (DeveloperOptionsStore.mockVerificationBarrier) {
				case 'unclaimed_account':
					return <UnclaimedAccountBarrier />;
				case 'unverified_email':
					return <UnverifiedEmailBarrier />;
				case 'account_too_new':
					return (
						<AccountTooNewBarrier initialTimeRemaining={DeveloperOptionsStore.mockBarrierTimeRemaining || 300000} />
					);
				case 'not_member_long':
					return (
						<NotMemberLongEnoughBarrier
							initialTimeRemaining={DeveloperOptionsStore.mockBarrierTimeRemaining || 600000}
						/>
					);
				case 'no_phone':
					return <NoPhoneNumberBarrier />;
				case 'send_message_disabled':
					return <SendMessageDisabledBarrier />;
				default:
					return passesVerification ? <ChannelTextarea channel={channel} /> : <VerificationBarrier channel={channel} />;
			}
		}

		return passesVerification ? <ChannelTextarea channel={channel} /> : <VerificationBarrier channel={channel} />;
	};

	if (isVoiceChannel) {
		if (isConnectedToThisChannel && room) {
			return (
				<div className={styles.voiceChannelContainer}>
					<VoiceCallView channel={channel} />
				</div>
			);
		}

		return (
			<div className={styles.channelGrid}>
				<ChannelHeader channel={channel} showMembersToggle={false} showPins={false} />
				<div className={styles.emptyStateContent}>
					<div className={styles.centeredText}>
						<h2 className={styles.voiceChannelTitle}>{channel.name}</h2>
						<p className={styles.voiceChannelDescription}>This is a voice channel. Connect to start talking!</p>
					</div>
					<div className={styles.buttonContainer}>
						<Button
							type="button"
							onClick={() => MediaEngineStore.connectToVoiceChannel(channel.guildId!, channel.id)}
							fitContainer={false}
							fitContent
						>
							Join Voice Channel
						</Button>
					</div>
				</div>
			</div>
		);
	}

	if ((channel.isNSFW() && showNSFWGate) || forceMockNSFWGate) {
		return (
			<div className={styles.channelGrid}>
				<ChannelHeader channel={channel} showMembersToggle={false} showPins={false} />
				<NSFWChannelGate channelId={channelId} reason={nsfwGateReason} />
			</div>
		);
	}

	const shouldRenderMemberList = isMemberListVisible && !isMobileLayout && !isSearchActive;

	return (
		<ChannelViewScaffold
			header={
				<ChannelHeader
					channel={channel}
					showMembersToggle={true}
					showPins={true}
					onSearchSubmit={handleSearchSubmit}
					onSearchClose={handleSearchClose}
					isSearchResultsOpen={isSearchActive}
				/>
			}
			chatArea={
				<ChannelChatLayout
					channel={channel}
					messages={<Messages key={channel.id} channel={channel} />}
					textarea={renderChatArea()}
				/>
			}
			sidePanel={
				isSearchActive ? (
					<div className={styles.searchPanel}>
						<ChannelSearchResults
							channel={channel}
							searchQuery={activeSearchQuery}
							searchSegments={activeSearchSegments}
							refreshKey={searchRefreshKey}
							onClose={() => searchState.setIsSearchActive(false)}
						/>
					</div>
				) : shouldRenderMemberList ? (
					<ChannelMembers channel={channel} guild={guild} />
				) : null
			}
			showMemberListDivider={shouldRenderMemberList && !isSearchActive}
		/>
	);
});
