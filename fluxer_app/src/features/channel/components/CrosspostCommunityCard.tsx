// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import styles from '@app/features/channel/components/CrosspostCommunityCard.module.css';
import CrosspostSourceCards from '@app/features/channel/state/CrosspostSourceCards';
import {getCrosspostSourceDisplayName, getCrosspostSourceGuildId} from '@app/features/channel/utils/CrosspostUtils';
import {joinDiscoveryGuild} from '@app/features/discovery/commands/DiscoveryJoinCommands';
import {resolveBannerTintClassName} from '@app/features/discovery/utils/DiscoveryBannerTint';
import {GuildBadge} from '@app/features/guild/components/GuildBadge';
import {GuildIcon} from '@app/features/guild/components/popouts/GuildIcon';
import GuildCount from '@app/features/guild/state/GuildCount';
import Guilds from '@app/features/guild/state/Guilds';
import {JOIN_COMMUNITY_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import {Button} from '@app/features/ui/button/Button';
import {Spinner} from '@app/features/ui/components/Spinner';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {getCurrentLocale} from '@app/features/user/utils/LocaleUtils';
import {msg} from '@lingui/core/macro';
import {Plural, Trans, useLingui} from '@lingui/react/macro';
import {formatNumber} from '@pkgs/number_utils/src/NumberFormatting';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useState} from 'react';

const GO_TO_COMMUNITY_DESCRIPTOR = msg({
	message: 'Go to community',
	comment:
		'Button on the community card that opens from a published message copy, shown when the viewer is already a member of the source community.',
});
const ONLINE_COUNT_DESCRIPTOR = msg({
	message: '{renderedOnlineCount} online',
	comment:
		'Presence summary on the community card that opens from a published message copy. renderedOnlineCount is a formatted count of online members.',
});
const COMMUNITY_UNAVAILABLE_DESCRIPTOR = msg({
	message: 'This community is no longer available',
	comment:
		'Shown on the community card that opens from a published message copy when the source community was deleted or cannot be viewed.',
});
const COMMUNITY_LOAD_FAILED_DESCRIPTOR = msg({
	message: "Couldn't load this community. Try again in a moment.",
	comment:
		'Shown on the community card that opens from a published message copy when loading the source community failed.',
});

const ICON_SIZE_PX = 56;

interface CommunityCardData {
	id: string;
	name: string;
	icon: string | null;
	banner: string | null;
	description: string | null;
	features: ReadonlySet<string> | ReadonlyArray<string>;
	onlineCount: number | null;
	memberCount: number | null;
	isMember: boolean;
	canJoin: boolean;
}

type CommunityCardView =
	| {kind: 'card'; data: CommunityCardData}
	| {kind: 'loading'}
	| {kind: 'unavailable'}
	| {kind: 'failed'};

function useCommunityCardView(message: Message, sourceGuildId: string | null): CommunityCardView {
	const localGuild = sourceGuildId != null ? Guilds.getGuild(sourceGuildId) : null;
	useEffect(() => {
		if (sourceGuildId == null) {
			return;
		}
		CrosspostSourceCards.ensure(sourceGuildId, message.channelId, message.id);
	}, [sourceGuildId, message.channelId, message.id]);
	if (sourceGuildId == null) {
		return {kind: 'unavailable'};
	}
	const entry = CrosspostSourceCards.get(sourceGuildId);
	if (localGuild != null) {
		const counts = GuildCount.getCounts(localGuild.id);
		const fetched = entry?.status === 'ready' ? entry.card : null;
		return {
			kind: 'card',
			data: {
				id: localGuild.id,
				name: localGuild.name,
				icon: localGuild.icon,
				banner: localGuild.banner,
				description: fetched?.description ?? null,
				features: localGuild.features,
				onlineCount: counts?.onlineCount ?? null,
				memberCount: counts?.memberCount ?? null,
				isMember: true,
				canJoin: false,
			},
		};
	}
	if (entry == null || entry.status === 'loading') {
		return {kind: 'loading'};
	}
	if (entry.status === 'unavailable') {
		return {kind: 'unavailable'};
	}
	if (entry.status === 'failed') {
		return {kind: 'failed'};
	}
	const {card} = entry;
	return {
		kind: 'card',
		data: {
			id: card.id,
			name: card.name,
			icon: card.icon ?? null,
			banner: card.banner ?? null,
			description: card.description ?? null,
			features: card.features,
			onlineCount: card.approximate_presence_count,
			memberCount: card.approximate_member_count,
			isMember: false,
			canJoin: card.discoverable && !RuntimeConfig.singleCommunityEnabled,
		},
	};
}

interface CrosspostCommunityCardProps {
	message: Message;
	onClose: () => void;
	variant?: 'popout' | 'sheet';
}

export const CrosspostCommunityCard = observer(function CrosspostCommunityCard({
	message,
	onClose,
	variant = 'popout',
}: CrosspostCommunityCardProps) {
	const {i18n} = useLingui();
	const sourceGuildId = getCrosspostSourceGuildId(message);
	const view = useCommunityCardView(message, sourceGuildId);
	const [joining, setJoining] = useState(false);
	const data = view.kind === 'card' ? view.data : null;
	const guildId = data?.id ?? sourceGuildId ?? message.author.id;
	const name = data?.name ?? getCrosspostSourceDisplayName(message);
	const handleGoToCommunity = useCallback(() => {
		onClose();
		NavigationCommands.selectGuild(guildId);
	}, [onClose, guildId]);
	const handleJoin = useCallback(async () => {
		setJoining(true);
		const joined = await joinDiscoveryGuild(guildId);
		setJoining(false);
		if (joined) {
			onClose();
		}
	}, [guildId, onClose]);
	const bannerUrl = data?.banner ? AvatarUtils.getGuildBannerURL({id: data.id, banner: data.banner}) : null;
	const locale = getCurrentLocale();
	const hasCounts = data != null && data.onlineCount != null && data.memberCount != null;
	const renderedOnlineCount = hasCounts ? formatNumber(data.onlineCount ?? 0, locale) : '';
	const renderedMemberCount = hasCounts ? formatNumber(data.memberCount ?? 0, locale) : '';
	const memberCount = data?.memberCount ?? 0;
	return (
		<div
			className={clsx(styles.card, variant === 'sheet' && styles.sheetCard)}
			data-flx="channel.crosspost-community-card.card"
		>
			<div className={styles.banner} aria-hidden data-flx="channel.crosspost-community-card.banner">
				{bannerUrl != null ? (
					<>
						<span
							className={styles.bannerImage}
							style={{backgroundImage: `url(${bannerUrl})`}}
							data-flx="channel.crosspost-community-card.banner-image"
						/>
						<span className={styles.bannerScrim} data-flx="channel.crosspost-community-card.banner-scrim" />
					</>
				) : (
					<span
						className={resolveBannerTintClassName(guildId)}
						data-flx="channel.crosspost-community-card.banner-tint"
					/>
				)}
			</div>
			<GuildIcon
				id={guildId}
				name={name}
				icon={data?.icon ?? null}
				sizePx={ICON_SIZE_PX}
				className={styles.icon}
				containerProps={{'aria-hidden': true}}
				data-flx="channel.crosspost-community-card.icon"
			/>
			<div className={styles.body} data-flx="channel.crosspost-community-card.body">
				<div className={styles.titleRow} data-flx="channel.crosspost-community-card.title-row">
					<span className={styles.nameText} data-flx="channel.crosspost-community-card.name">
						{name}
					</span>
					{data != null && (
						<GuildBadge
							features={data.features}
							tooltipPosition="bottom"
							data-flx="channel.crosspost-community-card.guild-badge"
						/>
					)}
				</div>
				{data?.description && (
					<span className={styles.description} data-flx="channel.crosspost-community-card.description">
						{data.description}
					</span>
				)}
				{view.kind === 'loading' && (
					<div className={styles.loading} data-flx="channel.crosspost-community-card.loading">
						<Spinner size="small" data-flx="channel.crosspost-community-card.spinner" />
					</div>
				)}
				{view.kind === 'unavailable' && (
					<span className={styles.notice} data-flx="channel.crosspost-community-card.notice">
						{i18n._(COMMUNITY_UNAVAILABLE_DESCRIPTOR)}
					</span>
				)}
				{view.kind === 'failed' && (
					<span className={styles.notice} data-flx="channel.crosspost-community-card.notice--2">
						{i18n._(COMMUNITY_LOAD_FAILED_DESCRIPTOR)}
					</span>
				)}
			</div>
			{hasCounts && (
				<div className={styles.footer} data-flx="channel.crosspost-community-card.footer">
					<span className={styles.stat} data-flx="channel.crosspost-community-card.stat">
						<span
							className={styles.statDotOnline}
							aria-hidden
							data-flx="channel.crosspost-community-card.stat-dot-online"
						/>
						<span className={styles.statText} data-flx="channel.crosspost-community-card.stat-text">
							{i18n._(ONLINE_COUNT_DESCRIPTOR, {renderedOnlineCount})}
						</span>
					</span>
					<span className={styles.stat} data-flx="channel.crosspost-community-card.stat--2">
						<span
							className={styles.statDotMembers}
							aria-hidden
							data-flx="channel.crosspost-community-card.stat-dot-members"
						/>
						<span className={styles.statText} data-flx="channel.crosspost-community-card.stat-text--2">
							<Trans comment="Member count on the community card that opens from a published message copy. renderedMemberCount is the formatted count.">
								{renderedMemberCount}{' '}
								<Plural
									value={memberCount}
									one="member"
									other="members"
									data-flx="channel.crosspost-community-card.plural"
								/>
							</Trans>
						</span>
					</span>
				</div>
			)}
			{data?.isMember && (
				<div className={styles.action} data-flx="channel.crosspost-community-card.action">
					<Button
						variant="primary"
						fitContainer
						onClick={handleGoToCommunity}
						data-flx="channel.crosspost-community-card.button.go-to-community"
					>
						{i18n._(GO_TO_COMMUNITY_DESCRIPTOR)}
					</Button>
				</div>
			)}
			{data != null && !data.isMember && data.canJoin && (
				<div className={styles.action} data-flx="channel.crosspost-community-card.action--2">
					<Button
						variant="primary"
						fitContainer
						submitting={joining}
						onClick={handleJoin}
						data-flx="channel.crosspost-community-card.button.join"
					>
						{i18n._(JOIN_COMMUNITY_DESCRIPTOR)}
					</Button>
				</div>
			)}
		</div>
	);
});
