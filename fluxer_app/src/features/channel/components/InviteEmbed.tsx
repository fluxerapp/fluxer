// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import styles from '@app/features/channel/components/InviteEmbed.module.css';
import {
	getGroupDMTitle,
	getGuildEmbedSplashAspectRatio,
	getImageAspectRatioFromBase64,
} from '@app/features/channel/components/invite_embed/InviteEmbedUtils';
import {useMaybeMessageViewContext} from '@app/features/channel/components/MessageViewContext';
import type {Channel} from '@app/features/channel/models/Channel';
import {clampWideAssetAspectRatio} from '@app/features/expressions/utils/AssetImageGeometry';
import {GuildBadge} from '@app/features/guild/components/GuildBadge';
import {GuildIcon} from '@app/features/guild/components/popouts/GuildIcon';
import GuildCount from '@app/features/guild/state/GuildCount';
import Guilds from '@app/features/guild/state/Guilds';
import {
	JOIN_COMMUNITY_DESCRIPTOR,
	NO_DESCRIPTION_PROVIDED_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import * as InviteCommands from '@app/features/invite/commands/InviteCommands';
import Invites from '@app/features/invite/state/Invites';
import {
	isGroupDmInvite,
	isGuildInvite,
	isPackInvite as isPackInviteGuard,
} from '@app/features/invite/types/InviteTypes';
import {getGroupDmInviteCounts} from '@app/features/invite/utils/GroupDmInviteCounts';
import {
	GuildInvitePrimaryAction,
	getGuildInviteActionState,
	getGuildInvitePrimaryAction,
	isGuildInviteActionDisabled,
} from '@app/features/invite/utils/GuildInviteActionState';
import {
	EmbedCard,
	EmbedSkeletonButton,
	EmbedSkeletonCircle,
	EmbedSkeletonDot,
	EmbedSkeletonIcon,
	EmbedSkeletonStatLong,
	EmbedSkeletonStatShort,
	EmbedSkeletonTitle,
} from '@app/features/messaging/components/embeds/embed_card/EmbedCard';
import cardStyles from '@app/features/messaging/components/embeds/embed_card/EmbedCard.module.css';
import {useEmbedSkeletonOverride} from '@app/features/messaging/components/embeds/embed_card/useEmbedSkeletonOverride';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import StreamerMode from '@app/features/streamer_mode/state/StreamerMode';
import {InviteEmbedContextMenu} from '@app/features/ui/action_menu/InviteEmbedContextMenu';
import {Button} from '@app/features/ui/button/Button';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {Avatar} from '@app/features/ui/components/Avatar';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {getCurrentLocale} from '@app/features/user/utils/LocaleUtils';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {msg} from '@lingui/core/macro';
import {Plural, Trans, useLingui} from '@lingui/react/macro';
import {QuestionIcon} from '@phosphor-icons/react';
import {formatNumber} from '@pkgs/number_utils/src/NumberFormatting';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useMemo, useRef, useState} from 'react';

const UNNAMED_GROUP_DESCRIPTOR = msg({
	message: 'Unnamed group',
	comment: 'Fallback title in an invite embed for a group DM with no name set.',
});
const ALREADY_JOINED_DESCRIPTOR = msg({
	message: 'Already joined',
	comment: 'Status label on a group DM invite embed when the current user is already a member.',
});
const JOIN_GROUP_DESCRIPTOR = msg({
	message: 'Join group',
	comment: 'Button label on a group DM invite embed that accepts the invite.',
});
const EMOJI_PACK_DESCRIPTOR = msg({
	message: 'Emoji pack',
	comment: 'Kind label on an emoji pack invite embed.',
});
const STICKER_PACK_DESCRIPTOR = msg({
	message: 'Sticker pack',
	comment: 'Kind label on a sticker pack invite embed.',
});
const INSTALL_EMOJI_PACK_DESCRIPTOR = msg({
	message: 'Install emoji pack',
	comment: 'Button label on an emoji pack invite embed that installs the pack.',
});
const INSTALL_STICKER_PACK_DESCRIPTOR = msg({
	message: 'Install sticker pack',
	comment: 'Button label on a sticker pack invite embed that installs the pack.',
});
const ACCEPTING_THIS_INVITE_INSTALLS_THE_PACK_AUTOMATICALLY_DESCRIPTOR = msg({
	message: 'Accepting this invite installs the pack automatically.',
	comment: 'Helper text on a pack invite embed explaining that accepting installs the pack.',
});
const INVITES_DISABLED_DESCRIPTOR = msg({
	message: 'Invites disabled',
	comment: 'Status label on a community invite embed when invites are paused or anti-raid mode is active.',
});
const GO_TO_COMMUNITY_DESCRIPTOR = msg({
	message: 'Go to community',
	comment: 'Button label on a community invite embed when the current user is already a member.',
});
const ONLINE_DESCRIPTOR = msg({
	message: '{renderedPresenceCount} online',
	comment:
		'Presence summary on a community invite embed. renderedPresenceCount is a formatted count of online members.',
});
const DETECTED_A_POTENTIAL_RAID_SO_NEW_USERS_CAN_DESCRIPTOR = msg({
	message: "{productName} detected a potential raid, so new users can't join right now.",
	comment: 'Anti-raid status message on a community invite embed. productName is the product brand name placeholder.',
});
const INVITES_ARE_CURRENTLY_PAUSED_FOR_THIS_COMMUNITY_DESCRIPTOR = msg({
	message: 'Invites are paused for this community.',
	comment: 'Status message on a community invite embed when invites are paused by community admins.',
});
const UNKNOWN_INVITE_DESCRIPTOR = msg({
	message: 'Unknown invite',
	comment: 'Title shown on an invite embed when the invite code resolves to nothing (expired, revoked, or invalid).',
});
const TRY_ASKING_FOR_A_NEW_INVITE_DESCRIPTOR = msg({
	message: 'Try asking for a new invite.',
	comment: 'Helper text on an unknown invite embed suggesting the user request a fresh invite.',
});
const INVITE_UNAVAILABLE_DESCRIPTOR = msg({
	message: 'Invite unavailable',
	comment: 'Title shown on an invite embed when the invite cannot be resolved due to a non-permanent failure.',
});
const INVITE_MASKED_DESCRIPTOR = msg({
	message: 'Invite masked',
	comment: 'Title shown on an invite embed while streaming privacy is active.',
});
const THIS_INVITE_IS_MASKED_WHILE_SHARING_DESCRIPTOR = msg({
	message: 'This invite is hidden while sharing is protected.',
	comment: 'Helper text shown on an invite embed while streaming privacy is active.',
});
const createTitleKeyDownHandler = (callback: () => void) => (event: React.KeyboardEvent<HTMLButtonElement>) => {
	if (isKeyboardActivationKey(event.key)) {
		event.preventDefault();
		callback();
	}
};

function formatInviteCount(value: number): string {
	return formatNumber(value, getCurrentLocale());
}

interface InviteEmbedProps {
	code: string;
	message?: Message;
	sourceChannel?: Channel | null;
	onDelete?: (bypassConfirm?: boolean) => void;
}

export const InviteEmbed = observer(function InviteEmbed({code, message, sourceChannel, onDelete}: InviteEmbedProps) {
	if (StreamerMode.shouldHideInviteLinks) {
		return <InviteHiddenState data-flx="channel.invite-embed.invite-hidden-state" />;
	}
	return (
		<InviteEmbedInner
			code={code}
			message={message}
			sourceChannel={sourceChannel}
			onDelete={onDelete}
			data-flx="channel.invite-embed.inner"
		/>
	);
});

const InviteEmbedInner = observer(function InviteEmbedInner({
	code,
	message,
	sourceChannel,
	onDelete,
}: InviteEmbedProps) {
	const {i18n} = useLingui();
	const inviteState = Invites.invites.get(code) ?? null;
	const shouldForceSkeleton = useEmbedSkeletonOverride();
	const invite = inviteState?.data ?? null;
	const isPackInvite = invite != null && isPackInviteGuard(invite);
	const isGuildInviteType = invite != null && isGuildInvite(invite);
	const packCreatorRecord = useMemo(() => {
		if (!isPackInvite || !invite) return null;
		return new User(invite.pack.creator);
	}, [invite, isPackInvite]);
	const guildFromInvite = isGuildInviteType ? invite!.guild : null;
	const guild = Guilds.getGuild(guildFromInvite?.id ?? '') || guildFromInvite;
	const embedSplash = guild != null ? ('embedSplash' in guild ? guild.embedSplash : guild.embed_splash) : undefined;
	const splashURL =
		guild != null ? AvatarUtils.getGuildEmbedSplashURL({id: guild.id, embedSplash: embedSplash || null}) : null;
	const messageViewContext = useMaybeMessageViewContext();
	const currentChannelId = messageViewContext?.channel.id;
	const inviteWrapperRef = useRef<HTMLDivElement | null>(null);
	const isLoading = shouldForceSkeleton || !inviteState || inviteState.loading;
	const prevLoadingRef = useRef(true);
	const prevCodeRef = useRef(code);
	useEffect(() => {
		if (prevCodeRef.current !== code) {
			prevLoadingRef.current = true;
			prevCodeRef.current = code;
		}
	}, [code]);
	useEffect(() => {
		if (prevLoadingRef.current && !isLoading && currentChannelId) {
			ComponentBus.dispatch('LAYOUT_RESIZED', {channelId: currentChannelId});
		}
		prevLoadingRef.current = isLoading;
	}, [isLoading, currentChannelId]);
	useEffect(() => {
		if (!inviteState) {
			void InviteCommands.fetchWithCoalescing(code).catch(() => {});
		}
	}, [code, inviteState]);
	const memberGuildId = isGuildInviteType && Guilds.getGuild(guildFromInvite?.id ?? '') ? guild!.id : null;
	useEffect(() => {
		if (memberGuildId) {
			GuildCount.requestCounts(memberGuildId);
		}
	}, [memberGuildId]);
	let content: React.ReactNode;
	if (shouldForceSkeleton || !inviteState || inviteState.loading) {
		content = <InviteLoadingState data-flx="channel.invite-embed.invite-loading-state" />;
	} else if (inviteState.error || !invite) {
		content = <InviteNotFoundError data-flx="channel.invite-embed.invite-not-found-error" />;
	} else if (isGroupDmInvite(invite)) {
		const inviter = Users.getUser(invite.inviter?.id ?? '');
		const groupDMTitle = getGroupDMTitle(invite.channel, i18n._(UNNAMED_GROUP_DESCRIPTOR));
		const groupDMPath = Routes.dmChannel(invite.channel.id);
		const handleAcceptInvite = () => InviteCommands.acceptAndTransitionToChannel(invite.code, i18n);
		const handleNavigateToGroup = () => RouterUtils.transitionTo(groupDMPath);
		const groupDMCounts = getGroupDmInviteCounts({
			channelId: invite.channel.id,
			inviteMemberCount: invite.member_count,
		});
		const isAlreadyInGroupDM = groupDMCounts.hasLocalChannel;
		const memberCount = groupDMCounts.memberCount;
		const renderedMemberCount = formatInviteCount(memberCount);
		content = (
			<EmbedCard
				splashURL={null}
				headerClassName={styles.headerInvite}
				icon={
					inviter ? (
						<Avatar user={inviter} size={48} className={styles.icon} data-flx="channel.invite-embed.icon" />
					) : (
						<div className={styles.iconFallback} data-flx="channel.invite-embed.icon-fallback" />
					)
				}
				title={
					<div className={styles.titleContainer} data-flx="channel.invite-embed.title-container">
						<h3
							className={`${cardStyles.title} ${cardStyles.titlePrimary} ${styles.titleText}`}
							data-flx="channel.invite-embed.title-text"
						>
							<FocusRing offset={-2} data-flx="channel.invite-embed.focus-ring">
								<button
									type="button"
									className={cardStyles.titleButton}
									onClick={handleNavigateToGroup}
									onKeyDown={createTitleKeyDownHandler(handleNavigateToGroup)}
									data-flx="channel.invite-embed.button.navigate-to-group"
								>
									{groupDMTitle}
								</button>
							</FocusRing>
						</h3>
					</div>
				}
				body={
					<div className={styles.stats} data-flx="channel.invite-embed.stats">
						<div className={styles.stat} data-flx="channel.invite-embed.stat">
							<div className={`${styles.statDot} ${styles.statDotMembers}`} data-flx="channel.invite-embed.stat-dot" />
							<span className={styles.statText} data-flx="channel.invite-embed.stat-text">
								<Trans>
									{renderedMemberCount}{' '}
									<Plural value={memberCount} one="member" other="members" data-flx="channel.invite-embed.plural" />
								</Trans>
							</span>
						</div>
					</div>
				}
				footer={
					<Button
						variant="primary"
						fitContainer
						matchSkeletonHeight
						onClick={handleAcceptInvite}
						disabled={isAlreadyInGroupDM}
						data-flx="channel.invite-embed.button.accept-invite"
					>
						{isAlreadyInGroupDM ? i18n._(ALREADY_JOINED_DESCRIPTOR) : i18n._(JOIN_GROUP_DESCRIPTOR)}
					</Button>
				}
				data-flx="channel.invite-embed.embed-card"
			/>
		);
	} else if (isPackInviteGuard(invite)) {
		const pack = invite.pack;
		const packCreator = packCreatorRecord ?? new User(pack.creator);
		const packKindLabel = pack.type === 'emoji' ? i18n._(EMOJI_PACK_DESCRIPTOR) : i18n._(STICKER_PACK_DESCRIPTOR);
		const packActionLabel =
			pack.type === 'emoji' ? i18n._(INSTALL_EMOJI_PACK_DESCRIPTOR) : i18n._(INSTALL_STICKER_PACK_DESCRIPTOR);
		const inviterTag = invite.inviter
			? NicknameUtils.formatTagForStreamerMode(`${invite.inviter.username}#${invite.inviter.discriminator}`)
			: null;
		const handleAcceptInvite = () => InviteCommands.acceptAndTransitionToChannel(invite.code, i18n);
		content = (
			<EmbedCard
				splashURL={null}
				headerClassName={styles.headerInvite}
				icon={<Avatar user={packCreator} size={48} className={styles.icon} data-flx="channel.invite-embed.icon--2" />}
				title={
					<div
						className={`${styles.titleContainer} ${styles.packTitleRow}`}
						data-flx="channel.invite-embed.title-container--2"
					>
						<h3
							className={`${cardStyles.title} ${cardStyles.titlePrimary} ${styles.titleText}`}
							data-flx="channel.invite-embed.title-text--2"
						>
							{pack.name}
						</h3>
						<span className={styles.packBadge} data-flx="channel.invite-embed.pack-badge">
							{packKindLabel}
						</span>
					</div>
				}
				body={
					<div className={styles.packBody} data-flx="channel.invite-embed.pack-body">
						<p className={styles.packDescription} data-flx="channel.invite-embed.pack-description">
							{pack.description || i18n._(NO_DESCRIPTION_PROVIDED_DESCRIPTOR)}
						</p>
						<div className={styles.packMeta} data-flx="channel.invite-embed.pack-meta">
							<span data-flx="channel.invite-embed.span">
								<Trans>Created by {NicknameUtils.getDisplayName(packCreator)}</Trans>
							</span>
							{inviterTag ? (
								<span data-flx="channel.invite-embed.span--2">
									<Trans>Invited by {inviterTag}</Trans>
								</span>
							) : null}
						</div>
						<p className={styles.packNote} data-flx="channel.invite-embed.pack-note">
							{i18n._(ACCEPTING_THIS_INVITE_INSTALLS_THE_PACK_AUTOMATICALLY_DESCRIPTOR)}
						</p>
					</div>
				}
				footer={
					<Button
						variant="primary"
						fitContainer
						matchSkeletonHeight
						onClick={handleAcceptInvite}
						data-flx="channel.invite-embed.button.accept-invite--2"
					>
						{packActionLabel}
					</Button>
				}
				data-flx="channel.invite-embed.embed-card--2"
			/>
		);
	} else if (!guild || !isGuildInvite(invite)) {
		content = <InviteNotFoundError data-flx="channel.invite-embed.invite-not-found-error--2" />;
	} else {
		const guildActionState = getGuildInviteActionState({invite, guild});
		const {features, isMember} = guildActionState;
		const liveCounts = isMember ? GuildCount.getCounts(guild.id) : null;
		const presenceCount = liveCounts?.onlineCount ?? guildActionState.presenceCount;
		const memberCount = liveCounts?.memberCount ?? guildActionState.memberCount;
		const splashAspectRatio = getGuildEmbedSplashAspectRatio(guild);
		const renderedPresenceCount = formatInviteCount(presenceCount);
		const renderedMemberCount = formatInviteCount(memberCount);
		const handleAcceptInvite = () => InviteCommands.acceptAndTransitionToChannel(invite.code, i18n);
		const guildPath = Routes.guildChannel(guild.id, invite.channel.id);
		const handleNavigateToGuild = () => RouterUtils.transitionTo(guildPath);
		const actionType = getGuildInvitePrimaryAction(guildActionState);
		const isButtonDisabled = isGuildInviteActionDisabled(guildActionState);
		const getButtonLabel = () => {
			switch (actionType) {
				case GuildInvitePrimaryAction.InvitesDisabled:
					return i18n._(INVITES_DISABLED_DESCRIPTOR);
				case GuildInvitePrimaryAction.GoToCommunity:
					return i18n._(GO_TO_COMMUNITY_DESCRIPTOR);
				default:
					return i18n._(JOIN_COMMUNITY_DESCRIPTOR);
			}
		};
		content = (
			<EmbedCard
				splashURL={splashURL}
				splashAspectRatio={splashAspectRatio}
				headerClassName={styles.headerInvite}
				icon={
					<GuildIcon
						id={guild.id}
						name={guild.name}
						icon={guild.icon}
						className={styles.icon}
						data-flx="channel.invite-embed.icon--3"
					/>
				}
				title={
					<div className={styles.titleContainer} data-flx="channel.invite-embed.title-container--3">
						<div className={styles.titleRowWithIcon} data-flx="channel.invite-embed.title-row-with-icon">
							<h3
								className={`${cardStyles.title} ${cardStyles.titlePrimary} ${styles.titleText}`}
								data-flx="channel.invite-embed.title-text--3"
							>
								<FocusRing offset={-2} data-flx="channel.invite-embed.focus-ring--2">
									<button
										type="button"
										className={cardStyles.titleButton}
										onClick={handleNavigateToGuild}
										onKeyDown={createTitleKeyDownHandler(handleNavigateToGuild)}
										data-flx="channel.invite-embed.button.navigate-to-guild"
									>
										{guild.name}
									</button>
								</FocusRing>
							</h3>
							<GuildBadge
								features={features}
								variant="large"
								onLightSurface
								data-flx="channel.invite-embed.guild-badge"
							/>
						</div>
					</div>
				}
				body={
					<div className={styles.stats} data-flx="channel.invite-embed.stats--2">
						<div className={styles.stat} data-flx="channel.invite-embed.stat--2">
							<div
								className={`${styles.statDot} ${styles.statDotOnline}`}
								data-flx="channel.invite-embed.stat-dot--2"
							/>
							<span className={styles.statText} data-flx="channel.invite-embed.stat-text--2">
								{i18n._(ONLINE_DESCRIPTOR, {renderedPresenceCount})}
							</span>
						</div>
						<div className={styles.stat} data-flx="channel.invite-embed.stat--3">
							<div
								className={`${styles.statDot} ${styles.statDotMembers}`}
								data-flx="channel.invite-embed.stat-dot--3"
							/>
							<span className={styles.statText} data-flx="channel.invite-embed.stat-text--3">
								<Trans>
									{renderedMemberCount}{' '}
									<Plural value={memberCount} one="member" other="members" data-flx="channel.invite-embed.plural--2" />
								</Trans>
							</span>
						</div>
						{actionType === GuildInvitePrimaryAction.InvitesDisabled && (
							<p className={styles.statText} data-flx="channel.invite-embed.stat-text--4">
								{guildActionState.isRaidDetected
									? i18n._(DETECTED_A_POTENTIAL_RAID_SO_NEW_USERS_CAN_DESCRIPTOR, {productName: PRODUCT_NAME})
									: i18n._(INVITES_ARE_CURRENTLY_PAUSED_FOR_THIS_COMMUNITY_DESCRIPTOR)}
							</p>
						)}
					</div>
				}
				footer={
					<Button
						variant="primary"
						fitContainer
						matchSkeletonHeight
						onClick={handleAcceptInvite}
						disabled={isButtonDisabled}
						data-flx="channel.invite-embed.button.accept-invite--3"
					>
						{getButtonLabel()}
					</Button>
				}
				data-flx="channel.invite-embed.embed-card--3"
			/>
		);
	}
	const handleContextMenu = (event: React.MouseEvent) => {
		const channelId = invite && (isGuildInvite(invite) || isGroupDmInvite(invite)) ? invite.channel.id : null;
		const guildForMenu = guild ? {id: guild.id, name: guild.name} : null;
		const inviteCodeForMenu = guildForMenu && invite ? invite.code : null;
		if (!message && !guildForMenu && !channelId) return;
		event.preventDefault();
		event.stopPropagation();
		ContextMenuCommands.openFromEvent(event, (props) => (
			<InviteEmbedContextMenu
				message={message}
				sourceChannel={sourceChannel}
				linkUrl={`${RuntimeConfig.inviteEndpoint}/${code}`}
				guild={guildForMenu}
				channelId={channelId}
				inviteCode={inviteCodeForMenu}
				onDelete={onDelete}
				onClose={props.onClose}
				data-flx="channel.invite-embed.handle-context-menu.invite-embed-context-menu"
			/>
		));
	};
	return (
		<div
			ref={inviteWrapperRef}
			role="group"
			className={styles.inviteWrapper}
			onContextMenu={handleContextMenu}
			data-flx="channel.invite-embed.invite-wrapper.context-menu"
		>
			{content}
		</div>
	);
});
const InviteHiddenState = observer(() => {
	const {i18n} = useLingui();
	return (
		<EmbedCard
			splashURL={null}
			headerClassName={styles.headerInvite}
			icon={
				<div className={styles.iconFallback} data-flx="channel.invite-embed.invite-hidden-state.icon-fallback">
					<QuestionIcon weight="bold" data-flx="channel.invite-embed.invite-hidden-state.question-icon" />
				</div>
			}
			title={
				<div className={styles.titleContainer} data-flx="channel.invite-embed.invite-hidden-state.title-container">
					<h3
						className={`${cardStyles.title} ${cardStyles.titlePrimary} ${styles.titleText}`}
						data-flx="channel.invite-embed.invite-hidden-state.title"
					>
						{i18n._(INVITE_MASKED_DESCRIPTOR)}
					</h3>
				</div>
			}
			body={
				<p className={styles.statText} data-flx="channel.invite-embed.invite-hidden-state.body">
					{i18n._(THIS_INVITE_IS_MASKED_WHILE_SHARING_DESCRIPTOR)}
				</p>
			}
			footer={null}
			data-flx="channel.invite-embed.invite-hidden-state.embed-card"
		/>
	);
});
const InviteLoadingState = observer(() => {
	return (
		<EmbedCard
			splashURL={null}
			headerClassName={styles.headerInvite}
			icon={<EmbedSkeletonCircle data-flx="channel.invite-embed.invite-loading-state.embed-skeleton-circle" />}
			title={
				<div className={styles.titleContainer} data-flx="channel.invite-embed.invite-loading-state.title-container">
					<div
						className={styles.titleRowWithIcon}
						data-flx="channel.invite-embed.invite-loading-state.title-row-with-icon"
					>
						<EmbedSkeletonTitle data-flx="channel.invite-embed.invite-loading-state.embed-skeleton-title" />
						<EmbedSkeletonIcon data-flx="channel.invite-embed.invite-loading-state.embed-skeleton-icon" />
					</div>
				</div>
			}
			body={
				<div className={styles.stats} data-flx="channel.invite-embed.invite-loading-state.stats">
					<div className={styles.stat} data-flx="channel.invite-embed.invite-loading-state.stat">
						<EmbedSkeletonDot data-flx="channel.invite-embed.invite-loading-state.embed-skeleton-dot" />
						<EmbedSkeletonStatShort data-flx="channel.invite-embed.invite-loading-state.embed-skeleton-stat-short" />
					</div>
					<div className={styles.stat} data-flx="channel.invite-embed.invite-loading-state.stat--2">
						<EmbedSkeletonDot data-flx="channel.invite-embed.invite-loading-state.embed-skeleton-dot--2" />
						<EmbedSkeletonStatLong data-flx="channel.invite-embed.invite-loading-state.embed-skeleton-stat-long" />
					</div>
				</div>
			}
			footer={<EmbedSkeletonButton data-flx="channel.invite-embed.invite-loading-state.embed-skeleton-button" />}
			data-flx="channel.invite-embed.invite-loading-state.embed-card"
		/>
	);
});
const InviteNotFoundError = observer(() => {
	const {i18n} = useLingui();
	return (
		<EmbedCard
			splashURL={null}
			icon={
				<div className={cardStyles.iconCircleDisabled} data-flx="channel.invite-embed.invite-not-found-error.div">
					<QuestionIcon
						className={cardStyles.iconError}
						data-flx="channel.invite-embed.invite-not-found-error.question-icon"
					/>
				</div>
			}
			title={
				<h3
					className={`${cardStyles.title} ${cardStyles.titleDanger} ${styles.titleText}`}
					data-flx="channel.invite-embed.invite-not-found-error.title-text"
				>
					{i18n._(UNKNOWN_INVITE_DESCRIPTOR)}
				</h3>
			}
			subtitle={
				<span className={cardStyles.helpText} data-flx="channel.invite-embed.invite-not-found-error.span">
					{i18n._(TRY_ASKING_FOR_A_NEW_INVITE_DESCRIPTOR)}
				</span>
			}
			footer={
				<Button
					variant="primary"
					fitContainer
					matchSkeletonHeight
					disabled
					data-flx="channel.invite-embed.invite-not-found-error.button"
				>
					{i18n._(INVITE_UNAVAILABLE_DESCRIPTOR)}
				</Button>
			}
			data-flx="channel.invite-embed.invite-not-found-error.embed-card"
		/>
	);
});

interface GuildInviteEmbedPreviewProps {
	guildId: string;
	splashURLOverride?: string | null;
}

export const GuildInviteEmbedPreview = observer(function GuildInviteEmbedPreview({
	guildId,
	splashURLOverride,
}: GuildInviteEmbedPreviewProps) {
	const {i18n} = useLingui();
	const guild = Guilds.getGuild(guildId);
	const [base64AspectRatio, setBase64AspectRatio] = useState<number | undefined>();
	const splashAspectRatio = useMemo(() => {
		if (!guild) return undefined;
		if (splashURLOverride) {
			return base64AspectRatio;
		}
		return getGuildEmbedSplashAspectRatio(guild);
	}, [guild, splashURLOverride, base64AspectRatio]);
	useEffect(() => {
		if (splashURLOverride) {
			getImageAspectRatioFromBase64(splashURLOverride)
				.then((ratio) => setBase64AspectRatio(clampWideAssetAspectRatio(ratio)))
				.catch(() => {
					setBase64AspectRatio(undefined);
				});
		} else {
			setBase64AspectRatio(undefined);
		}
	}, [splashURLOverride]);
	useEffect(() => {
		GuildCount.requestCounts(guildId);
	}, [guildId]);
	if (!guild) return null;
	const splashURL =
		splashURLOverride !== undefined
			? splashURLOverride
			: AvatarUtils.getGuildEmbedSplashURL({id: guild.id, embedSplash: guild.embedSplash || null});
	const liveCounts = GuildCount.getCounts(guild.id);
	const presenceCount = liveCounts?.onlineCount ?? 0;
	const memberCount = liveCounts?.memberCount ?? 0;
	const renderedPresenceCount = formatInviteCount(presenceCount);
	const renderedMemberCount = formatInviteCount(memberCount);
	return (
		<EmbedCard
			splashURL={splashURL}
			splashAspectRatio={splashAspectRatio}
			headerClassName={styles.headerInvite}
			icon={
				<GuildIcon
					id={guild.id}
					name={guild.name}
					icon={guild.icon}
					className={styles.icon}
					data-flx="channel.invite-embed.guild-invite-embed-preview.icon"
				/>
			}
			title={
				<div
					className={styles.titleContainer}
					data-flx="channel.invite-embed.guild-invite-embed-preview.title-container"
				>
					<div
						className={styles.titleRowWithIcon}
						data-flx="channel.invite-embed.guild-invite-embed-preview.title-row-with-icon"
					>
						<h3
							className={`${cardStyles.title} ${cardStyles.titlePrimary} ${styles.titleText}`}
							data-flx="channel.invite-embed.guild-invite-embed-preview.title-text"
						>
							{guild.name}
						</h3>
						<GuildBadge
							features={guild.features}
							variant="large"
							onLightSurface
							data-flx="channel.invite-embed.guild-invite-embed-preview.guild-badge"
						/>
					</div>
				</div>
			}
			body={
				<div className={styles.stats} data-flx="channel.invite-embed.guild-invite-embed-preview.stats">
					<div className={styles.stat} data-flx="channel.invite-embed.guild-invite-embed-preview.stat">
						<div
							className={`${styles.statDot} ${styles.statDotOnline}`}
							data-flx="channel.invite-embed.guild-invite-embed-preview.stat-dot"
						/>
						<span className={styles.statText} data-flx="channel.invite-embed.guild-invite-embed-preview.stat-text">
							{i18n._(ONLINE_DESCRIPTOR, {renderedPresenceCount})}
						</span>
					</div>
					<div className={styles.stat} data-flx="channel.invite-embed.guild-invite-embed-preview.stat--2">
						<div
							className={`${styles.statDot} ${styles.statDotMembers}`}
							data-flx="channel.invite-embed.guild-invite-embed-preview.stat-dot--2"
						/>
						<span className={styles.statText} data-flx="channel.invite-embed.guild-invite-embed-preview.stat-text--2">
							<Trans>
								{renderedMemberCount}{' '}
								<Plural
									value={memberCount}
									one="member"
									other="members"
									data-flx="channel.invite-embed.guild-invite-embed-preview.plural"
								/>
							</Trans>
						</span>
					</div>
				</div>
			}
			footer={
				<Button
					variant="primary"
					fitContainer
					matchSkeletonHeight
					disabled
					data-flx="channel.invite-embed.guild-invite-embed-preview.button"
				>
					{i18n._(JOIN_COMMUNITY_DESCRIPTOR)}
				</Button>
			}
			data-flx="channel.invite-embed.guild-invite-embed-preview.embed-card"
		/>
	);
});
