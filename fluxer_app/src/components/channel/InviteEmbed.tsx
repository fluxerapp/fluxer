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

import {Trans, useLingui} from '@lingui/react/macro';
import {QuestionIcon, SealCheckIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as InviteActionCreators from '~/actions/InviteActionCreators';
import {GuildFeatures} from '~/Constants';
import {
	EmbedCard,
	EmbedSkeletonButton,
	EmbedSkeletonCircle,
	EmbedSkeletonDot,
	EmbedSkeletonIcon,
	EmbedSkeletonStatLong,
	EmbedSkeletonStatShort,
	EmbedSkeletonTitle,
} from '~/components/embeds/EmbedCard/EmbedCard';
import cardStyles from '~/components/embeds/EmbedCard/EmbedCard.module.css';
import {useEmbedSkeletonOverride} from '~/components/embeds/EmbedCard/useEmbedSkeletonOverride';
import {GuildIcon} from '~/components/popouts/GuildIcon';
import {Avatar} from '~/components/uikit/Avatar';
import {Button} from '~/components/uikit/Button/Button';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {ComponentDispatch} from '~/lib/ComponentDispatch';
import {Routes} from '~/Routes';
import {UserRecord} from '~/records/UserRecord';
import GuildMemberStore from '~/stores/GuildMemberStore';
import GuildStore from '~/stores/GuildStore';
import InviteStore from '~/stores/InviteStore';
import PresenceStore from '~/stores/PresenceStore';
import UserStore from '~/stores/UserStore';

import {isGroupDmInvite, isGuildInvite, isPackInvite as isPackInviteGuard} from '~/types/InviteTypes';
import * as AvatarUtils from '~/utils/AvatarUtils';
import {getGroupDmInviteCounts} from '~/utils/invite/GroupDmInviteCounts';
import {
	GuildInvitePrimaryAction,
	getGuildInviteActionState,
	getGuildInvitePrimaryAction,
	isGuildInviteActionDisabled,
} from '~/utils/invite/GuildInviteActionState';
import * as RouterUtils from '~/utils/RouterUtils';
import {getGroupDMTitle, getGuildEmbedSplashAspectRatio, getImageAspectRatioFromBase64} from './InviteEmbed/utils';
import styles from './InviteEmbed.module.css';
import {useMaybeMessageViewContext} from './MessageViewContext';

const createTitleKeyDownHandler = (callback: () => void) => (event: React.KeyboardEvent<HTMLButtonElement>) => {
	if (event.key === 'Enter' || event.key === ' ') {
		event.preventDefault();
		callback();
	}
};

interface InviteEmbedProps {
	code: string;
}

export const InviteEmbed = observer(function InviteEmbed({code}: InviteEmbedProps) {
	const {t, i18n} = useLingui();

	const inviteState = InviteStore.invites.get(code) ?? null;
	const shouldForceSkeleton = useEmbedSkeletonOverride();
	const invite = inviteState?.data ?? null;
	const isPackInvite = invite != null && isPackInviteGuard(invite);
	const isGuildInviteType = invite != null && isGuildInvite(invite);
	const packCreatorRecord = React.useMemo(() => {
		if (!isPackInvite || !invite) return null;
		return new UserRecord(invite.pack.creator);
	}, [invite, isPackInvite]);
	const guildFromInvite = isGuildInviteType ? invite!.guild : null;
	const guild = GuildStore.getGuild(guildFromInvite?.id ?? '') || guildFromInvite;
	const embedSplash = guild != null ? ('embedSplash' in guild ? guild.embedSplash : guild.embed_splash) : undefined;
	const splashURL =
		guild != null ? AvatarUtils.getGuildEmbedSplashURL({id: guild.id, embedSplash: embedSplash || null}) : null;
	const messageViewContext = useMaybeMessageViewContext();
	const currentChannelId = messageViewContext?.channel.id;
	const inviteWrapperRef = React.useRef<HTMLDivElement | null>(null);
	const previousHeightRef = React.useRef<number | null>(null);

	React.useLayoutEffect(() => {
		if (!currentChannelId) return;
		const node = inviteWrapperRef.current;
		if (!node || typeof ResizeObserver === 'undefined') return;

		previousHeightRef.current = node.offsetHeight;

		const resizeObserver = new ResizeObserver(() => {
			const nextHeight = node.offsetHeight;
			const prevHeight = previousHeightRef.current ?? nextHeight;
			const heightDelta = nextHeight - prevHeight;
			if (heightDelta !== 0) {
				ComponentDispatch.dispatch('LAYOUT_RESIZED', {
					channelId: currentChannelId,
					heightDelta,
				});
			}
			previousHeightRef.current = nextHeight;
		});

		resizeObserver.observe(node);
		return () => resizeObserver.disconnect();
	}, [currentChannelId]);

	const isLoading = shouldForceSkeleton || !inviteState || inviteState.loading;

	const prevLoadingRef = React.useRef(true);
	const prevCodeRef = React.useRef(code);
	React.useLayoutEffect(() => {
		if (prevCodeRef.current !== code) {
			prevLoadingRef.current = true;
			prevCodeRef.current = code;
		}
	}, [code]);
	React.useLayoutEffect(() => {
		if (prevLoadingRef.current && !isLoading && currentChannelId) {
			ComponentDispatch.dispatch('LAYOUT_RESIZED', {channelId: currentChannelId});
		}
		prevLoadingRef.current = isLoading;
	}, [isLoading, currentChannelId]);

	React.useEffect(() => {
		if (!inviteState) {
			void InviteActionCreators.fetchWithCoalescing(code).catch(() => {});
		}
	}, [code, inviteState]);

	let content: React.ReactNode;

	if (shouldForceSkeleton || !inviteState || inviteState.loading) {
		content = <InviteLoadingState />;
	} else if (inviteState.error || !invite) {
		content = <InviteNotFoundError />;
	} else if (isGroupDmInvite(invite)) {
		const inviter = UserStore.getUser(invite.inviter?.id ?? '');
		const groupDMTitle = getGroupDMTitle(invite.channel);
		const groupDMPath = Routes.dmChannel(invite.channel.id);
		const handleAcceptInvite = () => InviteActionCreators.acceptAndTransitionToChannel(invite.code, i18n);
		const handleNavigateToGroup = () => RouterUtils.transitionTo(groupDMPath);
		const groupDMCounts = getGroupDmInviteCounts({
			channelId: invite.channel.id,
			inviteMemberCount: invite.member_count,
		});
		const isAlreadyInGroupDM = groupDMCounts.hasLocalChannel;
		const memberCount = groupDMCounts.memberCount;

		content = (
			<EmbedCard
				splashURL={null}
				headerClassName={styles.headerInvite}
				icon={
					inviter ? (
						<Avatar user={inviter} size={48} className={styles.icon} />
					) : (
						<div className={styles.iconFallback} />
					)
				}
				title={
					<div className={styles.titleContainer}>
						<h3 className={`${cardStyles.title} ${cardStyles.titlePrimary} ${styles.titleText}`}>
							<button
								type="button"
								className={cardStyles.titleButton}
								onClick={handleNavigateToGroup}
								onKeyDown={createTitleKeyDownHandler(handleNavigateToGroup)}
							>
								{groupDMTitle}
							</button>
						</h3>
					</div>
				}
				body={
					<div className={styles.stats}>
						<div className={styles.stat}>
							<div className={`${styles.statDot} ${styles.statDotMembers}`} />
							<span className={styles.statText}>
								{memberCount === 1 ? t`${memberCount} Member` : t`${memberCount} Members`}
							</span>
						</div>
					</div>
				}
				footer={
					<Button variant="primary" matchSkeletonHeight onClick={handleAcceptInvite} disabled={isAlreadyInGroupDM}>
						{isAlreadyInGroupDM ? t`Already joined` : t`Join Group`}
					</Button>
				}
			/>
		);
	} else if (isPackInviteGuard(invite)) {
		const pack = invite.pack;
		const packCreator = packCreatorRecord ?? new UserRecord(pack.creator);
		const packKindLabel = pack.type === 'emoji' ? t`Emoji pack` : t`Sticker pack`;
		const packActionLabel = pack.type === 'emoji' ? t`Install Emoji Pack` : t`Install Sticker Pack`;
		const inviterTag = invite.inviter ? `${invite.inviter.username}#${invite.inviter.discriminator}` : null;
		const handleAcceptInvite = () => InviteActionCreators.acceptAndTransitionToChannel(invite.code, i18n);

		content = (
			<EmbedCard
				splashURL={null}
				headerClassName={styles.headerInvite}
				icon={<Avatar user={packCreator} size={48} className={styles.icon} />}
				title={
					<div className={`${styles.titleContainer} ${styles.packTitleRow}`}>
						<h3 className={`${cardStyles.title} ${cardStyles.titlePrimary} ${styles.titleText}`}>{pack.name}</h3>
						<span className={styles.packBadge}>{packKindLabel}</span>
					</div>
				}
				body={
					<div className={styles.packBody}>
						<p className={styles.packDescription}>{pack.description || t`No description provided.`}</p>
						<div className={styles.packMeta}>
							<span>
								<Trans>Created by {pack.creator.username}</Trans>
							</span>
							{inviterTag ? (
								<span>
									<Trans>Invited by {inviterTag}</Trans>
								</span>
							) : null}
						</div>
						<p className={styles.packNote}>{t`Accepting this invite installs the pack automatically.`}</p>
					</div>
				}
				footer={
					<Button variant="primary" matchSkeletonHeight onClick={handleAcceptInvite}>
						{packActionLabel}
					</Button>
				}
			/>
		);
	} else if (!guild || !isGuildInvite(invite)) {
		content = <InviteNotFoundError />;
	} else {
		const guildActionState = getGuildInviteActionState({invite, guild});
		const {features, presenceCount, memberCount} = guildActionState;
		const isVerified = features.includes(GuildFeatures.VERIFIED);
		const splashAspectRatio = getGuildEmbedSplashAspectRatio(guild);

		const renderedPresenceCount = presenceCount;
		const renderedMemberCount = memberCount;

		const handleAcceptInvite = () => InviteActionCreators.acceptAndTransitionToChannel(invite.code, i18n);
		const guildPath = Routes.guildChannel(guild.id, invite.channel.id);
		const handleNavigateToGuild = () => RouterUtils.transitionTo(guildPath);

		const actionType = getGuildInvitePrimaryAction(guildActionState);
		const isButtonDisabled = isGuildInviteActionDisabled(guildActionState);
		const getButtonLabel = () => {
			switch (actionType) {
				case GuildInvitePrimaryAction.InvitesDisabled:
					return t`Invites Disabled`;
				case GuildInvitePrimaryAction.GoToCommunity:
					return t`Go to Community`;
				default:
					return t`Join Community`;
			}
		};

		content = (
			<EmbedCard
				splashURL={splashURL}
				splashAspectRatio={splashAspectRatio}
				headerClassName={styles.headerInvite}
				icon={<GuildIcon id={guild.id} name={guild.name} icon={guild.icon} className={styles.icon} />}
				title={
					<div className={styles.titleContainer}>
						<div className={styles.titleRowWithIcon}>
							<h3 className={`${cardStyles.title} ${cardStyles.titlePrimary} ${styles.titleText}`}>
								<button
									type="button"
									className={cardStyles.titleButton}
									onClick={handleNavigateToGuild}
									onKeyDown={createTitleKeyDownHandler(handleNavigateToGuild)}
								>
									{guild.name}
								</button>
							</h3>
							{isVerified ? (
								<Tooltip text={t`Verified Community`} position="top">
									<SealCheckIcon className={styles.verifiedIcon} />
								</Tooltip>
							) : null}
						</div>
					</div>
				}
				body={
					<div className={styles.stats}>
						<div className={styles.stat}>
							<div className={`${styles.statDot} ${styles.statDotOnline}`} />
							<span className={styles.statText}>{t`${renderedPresenceCount} Online`}</span>
						</div>
						<div className={styles.stat}>
							<div className={`${styles.statDot} ${styles.statDotMembers}`} />
							<span className={styles.statText}>
								{renderedMemberCount === 1 ? t`${renderedMemberCount} Member` : t`${renderedMemberCount} Members`}
							</span>
						</div>
					</div>
				}
				footer={
					<Button variant="primary" matchSkeletonHeight onClick={handleAcceptInvite} disabled={isButtonDisabled}>
						{getButtonLabel()}
					</Button>
				}
			/>
		);
	}

	return (
		<div ref={inviteWrapperRef} className={styles.inviteWrapper}>
			{content}
		</div>
	);
});

const InviteLoadingState = observer(() => {
	return (
		<EmbedCard
			splashURL={null}
			headerClassName={styles.headerInvite}
			icon={<EmbedSkeletonCircle />}
			title={
				<div className={styles.titleContainer}>
					<div className={styles.titleRowWithIcon}>
						<EmbedSkeletonTitle />
						<EmbedSkeletonIcon />
					</div>
				</div>
			}
			body={
				<div className={styles.stats}>
					<div className={styles.stat}>
						<EmbedSkeletonDot />
						<EmbedSkeletonStatShort />
					</div>
					<div className={styles.stat}>
						<EmbedSkeletonDot />
						<EmbedSkeletonStatLong />
					</div>
				</div>
			}
			footer={<EmbedSkeletonButton />}
		/>
	);
});

const InviteNotFoundError = observer(() => {
	const {t} = useLingui();
	return (
		<EmbedCard
			splashURL={null}
			icon={
				<div className={cardStyles.iconCircleDisabled}>
					<QuestionIcon className={cardStyles.iconError} />
				</div>
			}
			title={
				<h3 className={`${cardStyles.title} ${cardStyles.titleDanger} ${styles.titleText}`}>{t`Unknown Invite`}</h3>
			}
			subtitle={<span className={cardStyles.helpText}>{t`Try asking for a new invite.`}</span>}
			footer={
				<Button variant="primary" matchSkeletonHeight disabled>
					{t`Invite Unavailable`}
				</Button>
			}
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
	const {t} = useLingui();

	const guild = GuildStore.getGuild(guildId);

	const [base64AspectRatio, setBase64AspectRatio] = React.useState<number | undefined>();

	const splashAspectRatio = React.useMemo(() => {
		if (!guild) return undefined;

		if (splashURLOverride) {
			return base64AspectRatio;
		}
		return getGuildEmbedSplashAspectRatio(guild);
	}, [guild, splashURLOverride, base64AspectRatio]);

	React.useEffect(() => {
		if (splashURLOverride) {
			getImageAspectRatioFromBase64(splashURLOverride)
				.then(setBase64AspectRatio)
				.catch(() => {
					setBase64AspectRatio(undefined);
				});
		} else {
			setBase64AspectRatio(undefined);
		}
	}, [splashURLOverride]);

	if (!guild) return null;

	const isVerified = guild.features.has(GuildFeatures.VERIFIED);

	const splashURL =
		splashURLOverride !== undefined
			? splashURLOverride
			: AvatarUtils.getGuildEmbedSplashURL({id: guild.id, embedSplash: guild.embedSplash || null});

	const presenceCount = PresenceStore.getPresenceCount(guild.id);
	const memberCount = GuildMemberStore.getMemberCount(guild.id);

	return (
		<EmbedCard
			splashURL={splashURL}
			splashAspectRatio={splashAspectRatio}
			headerClassName={styles.headerInvite}
			icon={<GuildIcon id={guild.id} name={guild.name} icon={guild.icon} className={styles.icon} />}
			title={
				<div className={styles.titleContainer}>
					<div className={styles.titleRowWithIcon}>
						<h3 className={`${cardStyles.title} ${cardStyles.titlePrimary} ${styles.titleText}`}>{guild.name}</h3>
						{isVerified ? (
							<Tooltip text={t`Verified Community`} position="top">
								<SealCheckIcon className={styles.verifiedIcon} />
							</Tooltip>
						) : null}
					</div>
				</div>
			}
			body={
				<div className={styles.stats}>
					<div className={styles.stat}>
						<div className={`${styles.statDot} ${styles.statDotOnline}`} />
						<span className={styles.statText}>{t`${presenceCount} Online`}</span>
					</div>
					<div className={styles.stat}>
						<div className={`${styles.statDot} ${styles.statDotMembers}`} />
						<span className={styles.statText}>
							{memberCount === 1 ? t`${memberCount} Member` : t`${memberCount} Members`}
						</span>
					</div>
				</div>
			}
			footer={
				<Button variant="primary" matchSkeletonHeight disabled>
					{t`Join Community`}
				</Button>
			}
		/>
	);
});
