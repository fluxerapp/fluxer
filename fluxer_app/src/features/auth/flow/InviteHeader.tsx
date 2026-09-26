// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/auth/flow/AuthPageStyles.module.css';
import {GuildBadge} from '@app/features/guild/components/GuildBadge';
import {GuildIcon} from '@app/features/guild/components/popouts/GuildIcon';
import {isGroupDmInvite, isGuildInvite} from '@app/features/invite/types/InviteTypes';
import {BaseAvatar} from '@app/features/ui/components/BaseAvatar';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {getCurrentLocale} from '@app/features/user/utils/LocaleUtils';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import type {GroupDmInvite, GuildInvite, Invite} from '@fluxer/schema/src/domains/invite/InviteSchemas';
import {Plural, Trans} from '@lingui/react/macro';
import {formatNumber} from '@pkgs/number_utils/src/NumberFormatting';
import {observer} from 'mobx-react-lite';
import {useEffect, useState} from 'react';

interface InviteHeaderProps {
	invite: Invite;
}

interface GuildInviteHeaderProps {
	invite: GuildInvite;
}

interface GroupDMInviteHeaderProps {
	invite: GroupDmInvite;
}

interface PreviewGuildInviteHeaderProps {
	guildId: string;
	guildName: string;
	guildIcon: string | null;
	features: ReadonlyArray<string>;
	presenceCount: number;
	memberCount: number;
	previewIconUrl?: string | null;
	previewName?: string | null;
	eyebrowText?: string | null;
}

function formatInviteCount(value: number): string {
	return formatNumber(value, getCurrentLocale());
}

export const GuildInviteHeader = observer(function GuildInviteHeader({invite}: GuildInviteHeaderProps) {
	const guild = invite.guild;
	const features = Array.isArray(guild.features) ? guild.features : [...guild.features];
	const presenceCount = invite.presence_count ?? 0;
	const memberCount = invite.member_count ?? 0;
	const formattedPresenceCount = formatInviteCount(presenceCount);
	const formattedMemberCount = formatInviteCount(memberCount);
	return (
		<div className={styles.entityHeader} data-flx="auth.flow.invite-header.guild-invite-header.entity-header">
			<div
				className={styles.entityIconWrapper}
				data-flx="auth.flow.invite-header.guild-invite-header.entity-icon-wrapper"
			>
				<GuildIcon
					id={guild.id}
					name={guild.name}
					icon={guild.icon}
					className={styles.entityIcon}
					sizePx={80}
					data-flx="auth.flow.invite-header.guild-invite-header.entity-icon"
				/>
			</div>
			<div className={styles.entityDetails} data-flx="auth.flow.invite-header.guild-invite-header.entity-details">
				<p className={styles.entityText} data-flx="auth.flow.invite-header.guild-invite-header.entity-text">
					<Trans>You've been invited to join</Trans>
				</p>
				<div
					className={styles.entityTitleWrapper}
					data-flx="auth.flow.invite-header.guild-invite-header.entity-title-wrapper"
				>
					<h2 className={styles.entityTitle} data-flx="auth.flow.invite-header.guild-invite-header.entity-title">
						{guild.name}
					</h2>
					<GuildBadge features={features} data-flx="auth.flow.invite-header.guild-invite-header.guild-badge" />
				</div>
				<div className={styles.entityStats} data-flx="auth.flow.invite-header.guild-invite-header.entity-stats">
					<div className={styles.entityStat} data-flx="auth.flow.invite-header.guild-invite-header.entity-stat">
						<div className={styles.onlineDot} data-flx="auth.flow.invite-header.guild-invite-header.online-dot" />
						<span className={styles.statText} data-flx="auth.flow.invite-header.guild-invite-header.stat-text">
							<Trans>{formattedPresenceCount} online</Trans>
						</span>
					</div>
					<div className={styles.entityStat} data-flx="auth.flow.invite-header.guild-invite-header.entity-stat--2">
						<div className={styles.offlineDot} data-flx="auth.flow.invite-header.guild-invite-header.offline-dot" />
						<span className={styles.statText} data-flx="auth.flow.invite-header.guild-invite-header.stat-text--2">
							<Trans>
								{formattedMemberCount}{' '}
								<Plural
									value={memberCount}
									one="member"
									other="members"
									data-flx="auth.flow.invite-header.guild-invite-header.plural"
								/>
							</Trans>
						</span>
					</div>
				</div>
			</div>
		</div>
	);
});
export const GroupDMInviteHeader = observer(function GroupDMInviteHeader({invite}: GroupDMInviteHeaderProps) {
	const inviter = invite.inviter;
	const inviterDisplayName = inviter ? NicknameUtils.getDisplayName(inviter) : null;
	const avatarUrl = inviter ? AvatarUtils.getUserAvatarURL(inviter, false) : null;
	const memberCount = invite.member_count ?? 0;
	const formattedMemberCount = formatInviteCount(memberCount);
	return (
		<div className={styles.entityHeader} data-flx="auth.flow.invite-header.group-dm-invite-header.entity-header">
			{inviter && avatarUrl ? (
				<BaseAvatar
					size={80}
					avatarUrl={avatarUrl}
					shouldPlayAnimated={false}
					data-flx="auth.flow.invite-header.group-dm-invite-header.base-avatar"
				/>
			) : null}
			<div className={styles.entityDetails} data-flx="auth.flow.invite-header.group-dm-invite-header.entity-details">
				<p className={styles.entityText} data-flx="auth.flow.invite-header.group-dm-invite-header.entity-text">
					<Trans>You've been invited to join a group DM by</Trans>
				</p>
				{inviter ? (
					<h2 className={styles.entityTitle} data-flx="auth.flow.invite-header.group-dm-invite-header.entity-title">
						{inviterDisplayName}
					</h2>
				) : null}
				<div className={styles.entityStats} data-flx="auth.flow.invite-header.group-dm-invite-header.entity-stats">
					<div className={styles.entityStat} data-flx="auth.flow.invite-header.group-dm-invite-header.entity-stat">
						<div className={styles.offlineDot} data-flx="auth.flow.invite-header.group-dm-invite-header.offline-dot" />
						<span className={styles.statText} data-flx="auth.flow.invite-header.group-dm-invite-header.stat-text">
							<Trans>
								{formattedMemberCount}{' '}
								<Plural
									value={memberCount}
									one="member"
									other="members"
									data-flx="auth.flow.invite-header.group-dm-invite-header.plural"
								/>
							</Trans>
						</span>
					</div>
				</div>
			</div>
		</div>
	);
});
export function InviteHeader({invite}: InviteHeaderProps) {
	if (isGroupDmInvite(invite)) {
		return <GroupDMInviteHeader invite={invite} data-flx="auth.flow.invite-header.group-dm-invite-header" />;
	}
	if (isGuildInvite(invite)) {
		return <GuildInviteHeader invite={invite} data-flx="auth.flow.invite-header.guild-invite-header" />;
	}
	return null;
}

export const PreviewGuildInviteHeader = observer(function PreviewGuildInviteHeader({
	guildId,
	guildName,
	guildIcon,
	features,
	presenceCount,
	memberCount,
	previewIconUrl,
	previewName,
	eyebrowText,
}: PreviewGuildInviteHeaderProps) {
	const displayName = previewName ?? guildName;
	const formattedPresenceCount = formatInviteCount(presenceCount);
	const formattedMemberCount = formatInviteCount(memberCount);
	const [hasPreviewIconError, setPreviewIconError] = useState(false);
	useEffect(() => {
		setPreviewIconError(false);
	}, [previewIconUrl]);
	const shouldShowPreviewIcon = Boolean(previewIconUrl && !hasPreviewIconError);
	return (
		<div className={styles.entityHeader} data-flx="auth.flow.invite-header.preview-guild-invite-header.entity-header">
			<div
				className={styles.entityIconWrapper}
				data-flx="auth.flow.invite-header.preview-guild-invite-header.entity-icon-wrapper"
			>
				{shouldShowPreviewIcon ? (
					<img
						src={previewIconUrl as string}
						alt=""
						className={styles.entityIcon}
						onError={(e) => {
							e.currentTarget.style.display = 'none';
							setPreviewIconError(true);
						}}
						data-flx="auth.flow.invite-header.preview-guild-invite-header.entity-icon"
					/>
				) : (
					<GuildIcon
						id={guildId}
						name={displayName}
						icon={guildIcon}
						className={styles.entityIcon}
						sizePx={80}
						data-flx="auth.flow.invite-header.preview-guild-invite-header.entity-icon--2"
					/>
				)}
			</div>
			<div
				className={styles.entityDetails}
				data-flx="auth.flow.invite-header.preview-guild-invite-header.entity-details"
			>
				<p className={styles.entityText} data-flx="auth.flow.invite-header.preview-guild-invite-header.entity-text">
					{eyebrowText ?? <Trans>You've been invited to join</Trans>}
				</p>
				<div
					className={styles.entityTitleWrapper}
					data-flx="auth.flow.invite-header.preview-guild-invite-header.entity-title-wrapper"
				>
					<h2
						className={styles.entityTitle}
						data-flx="auth.flow.invite-header.preview-guild-invite-header.entity-title"
					>
						{displayName}
					</h2>
					<GuildBadge features={features} data-flx="auth.flow.invite-header.preview-guild-invite-header.guild-badge" />
				</div>
				<div className={styles.entityStats} data-flx="auth.flow.invite-header.preview-guild-invite-header.entity-stats">
					<div className={styles.entityStat} data-flx="auth.flow.invite-header.preview-guild-invite-header.entity-stat">
						<div
							className={styles.onlineDot}
							data-flx="auth.flow.invite-header.preview-guild-invite-header.online-dot"
						/>
						<span className={styles.statText} data-flx="auth.flow.invite-header.preview-guild-invite-header.stat-text">
							<Trans>{formattedPresenceCount} online</Trans>
						</span>
					</div>
					<div
						className={styles.entityStat}
						data-flx="auth.flow.invite-header.preview-guild-invite-header.entity-stat--2"
					>
						<div
							className={styles.offlineDot}
							data-flx="auth.flow.invite-header.preview-guild-invite-header.offline-dot"
						/>
						<span
							className={styles.statText}
							data-flx="auth.flow.invite-header.preview-guild-invite-header.stat-text--2"
						>
							<Trans>
								{formattedMemberCount}{' '}
								<Plural
									value={memberCount}
									one="member"
									other="members"
									data-flx="auth.flow.invite-header.preview-guild-invite-header.plural"
								/>
							</Trans>
						</span>
					</div>
				</div>
			</div>
		</div>
	);
});
