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
import {SealCheckIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {GuildFeatures} from '~/Constants';
import {GuildIcon} from '~/components/popouts/GuildIcon';
import {Avatar} from '~/components/uikit/Avatar';
import {BaseAvatar} from '~/components/uikit/BaseAvatar';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {UserRecord} from '~/records/UserRecord';
import type {GroupDmInvite, GuildInvite, Invite, PackInvite} from '~/types/InviteTypes';
import {isGroupDmInvite, isGuildInvite, isPackInvite} from '~/types/InviteTypes';
import * as AvatarUtils from '~/utils/AvatarUtils';
import styles from './AuthPageStyles.module.css';

interface InviteHeaderProps {
	invite: Invite;
}

interface GuildInviteHeaderProps {
	invite: GuildInvite;
}

interface GroupDMInviteHeaderProps {
	invite: GroupDmInvite;
}

interface PackInviteHeaderProps {
	invite: PackInvite;
}

interface PreviewGuildInviteHeaderProps {
	guildId: string;
	guildName: string;
	guildIcon: string | null;
	isVerified: boolean;
	presenceCount: number;
	memberCount: number;
	previewIconUrl?: string | null;
	previewName?: string | null;
}

export const GuildInviteHeader = observer(function GuildInviteHeader({invite}: GuildInviteHeaderProps) {
	const {t} = useLingui();
	const guild = invite.guild;
	const features = Array.isArray(guild.features) ? guild.features : [...guild.features];
	const isVerified = features.includes(GuildFeatures.VERIFIED);
	const memberCount = invite.member_count ?? 0;

	return (
		<div className={styles.entityHeader}>
			<div className={styles.entityIconWrapper}>
				<GuildIcon id={guild.id} name={guild.name} icon={guild.icon} className={styles.entityIcon} sizePx={80} />
			</div>
			<div className={styles.entityDetails}>
				<p className={styles.entityText}>
					<Trans>You've been invited to join</Trans>
				</p>
				<div className={styles.entityTitleWrapper}>
					<h2 className={styles.entityTitle}>{guild.name}</h2>
					{isVerified ? (
						<Tooltip text={t`Verified Community`} position="top">
							<SealCheckIcon className={styles.verifiedIcon} />
						</Tooltip>
					) : null}
				</div>
				<div className={styles.entityStats}>
					<div className={styles.entityStat}>
						<div className={styles.onlineDot} />
						<span className={styles.statText}>
							<Trans>{invite.presence_count} Online</Trans>
						</span>
					</div>
					<div className={styles.entityStat}>
						<div className={styles.offlineDot} />
						<span className={styles.statText}>
							{memberCount === 1 ? t`${memberCount} Member` : t`${memberCount} Members`}
						</span>
					</div>
				</div>
			</div>
		</div>
	);
});

export const GroupDMInviteHeader = observer(function GroupDMInviteHeader({invite}: GroupDMInviteHeaderProps) {
	const {t} = useLingui();
	const inviter = invite.inviter;
	const avatarUrl = inviter ? AvatarUtils.getUserAvatarURL(inviter, false) : null;
	const memberCount = invite.member_count ?? 0;

	return (
		<div className={styles.entityHeader}>
			{inviter && avatarUrl ? <BaseAvatar size={80} avatarUrl={avatarUrl} shouldPlayAnimated={false} /> : null}
			<div className={styles.entityDetails}>
				<p className={styles.entityText}>
					<Trans>You've been invited to join a group DM by</Trans>
				</p>
				{inviter ? (
					<h2 className={styles.entityTitle}>
						{inviter.username}#{inviter.discriminator}
					</h2>
				) : null}
				<div className={styles.entityStats}>
					<div className={styles.entityStat}>
						<div className={styles.offlineDot} />
						<span className={styles.statText}>
							{memberCount === 1 ? t`${memberCount} Member` : t`${memberCount} Members`}
						</span>
					</div>
				</div>
			</div>
		</div>
	);
});

export const PackInviteHeader = observer(function PackInviteHeader({invite}: PackInviteHeaderProps) {
	const {t} = useLingui();
	const pack = invite.pack;
	const creatorRecord = React.useMemo(() => new UserRecord(pack.creator), [pack.creator]);
	const packKindLabel = pack.type === 'emoji' ? t`Emoji pack` : t`Sticker pack`;
	const inviterTag = invite.inviter ? `${invite.inviter.username}#${invite.inviter.discriminator}` : null;

	return (
		<div className={styles.entityHeader}>
			<div className={styles.entityIconWrapper}>
				<Avatar user={creatorRecord} size={80} className={styles.entityIcon} />
			</div>
			<div className={styles.entityDetails}>
				<p className={styles.entityText}>
					<Trans>You've been invited to install</Trans>
				</p>
				<div className={styles.entityTitleWrapper}>
					<h2 className={styles.entityTitle}>{pack.name}</h2>
					<span className={styles.packBadge}>{packKindLabel}</span>
				</div>
				<p className={styles.packDescription}>{pack.description || t`No description provided.`}</p>
				<div className={styles.packMeta}>
					<span className={styles.packMetaText}>{t`Created by ${pack.creator.username}`}</span>
					{inviterTag ? <span className={styles.packMetaText}>{t`Invited by ${inviterTag}`}</span> : null}
				</div>
			</div>
		</div>
	);
});

export function InviteHeader({invite}: InviteHeaderProps) {
	if (isGroupDmInvite(invite)) {
		return <GroupDMInviteHeader invite={invite} />;
	}

	if (isPackInvite(invite)) {
		return <PackInviteHeader invite={invite} />;
	}

	if (isGuildInvite(invite)) {
		return <GuildInviteHeader invite={invite} />;
	}

	return null;
}

export const PreviewGuildInviteHeader = observer(function PreviewGuildInviteHeader({
	guildId,
	guildName,
	guildIcon,
	isVerified,
	presenceCount,
	memberCount,
	previewIconUrl,
	previewName,
}: PreviewGuildInviteHeaderProps) {
	const {t} = useLingui();
	const displayName = previewName ?? guildName;
	const [hasPreviewIconError, setPreviewIconError] = React.useState(false);

	React.useEffect(() => {
		setPreviewIconError(false);
	}, [previewIconUrl]);

	const shouldShowPreviewIcon = Boolean(previewIconUrl && !hasPreviewIconError);

	return (
		<div className={styles.entityHeader}>
			<div className={styles.entityIconWrapper}>
				{shouldShowPreviewIcon ? (
					<img
						src={previewIconUrl as string}
						alt=""
						className={styles.entityIcon}
						onError={(e) => {
							e.currentTarget.style.display = 'none';
							setPreviewIconError(true);
						}}
					/>
				) : (
					<GuildIcon id={guildId} name={displayName} icon={guildIcon} className={styles.entityIcon} sizePx={80} />
				)}
			</div>
			<div className={styles.entityDetails}>
				<p className={styles.entityText}>
					<Trans>You've been invited to join</Trans>
				</p>
				<div className={styles.entityTitleWrapper}>
					<h2 className={styles.entityTitle}>{displayName}</h2>
					{isVerified ? (
						<Tooltip text={t`Verified Community`} position="top">
							<SealCheckIcon className={styles.verifiedIcon} />
						</Tooltip>
					) : null}
				</div>
				<div className={styles.entityStats}>
					<div className={styles.entityStat}>
						<div className={styles.onlineDot} />
						<span className={styles.statText}>
							<Trans>{presenceCount} Online</Trans>
						</span>
					</div>
					<div className={styles.entityStat}>
						<div className={styles.offlineDot} />
						<span className={styles.statText}>
							{memberCount === 1 ? t`${memberCount} Member` : t`${memberCount} Members`}
						</span>
					</div>
				</div>
			</div>
		</div>
	);
});
