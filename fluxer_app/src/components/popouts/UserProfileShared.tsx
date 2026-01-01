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
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {AddRoleButton, RoleList} from '~/components/guild/RoleManagement';
import {FluxerIcon} from '~/components/icons/FluxerIcon';
import {GuildIcon} from '~/components/popouts/GuildIcon';
import styles from '~/components/popouts/UserProfileShared.module.css';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {SafeMarkdown} from '~/lib/markdown';
import {MarkdownContext} from '~/lib/markdown/renderers';
import type {GuildRoleRecord} from '~/records/GuildRoleRecord';
import type {ProfileRecord} from '~/records/ProfileRecord';
import type {UserProfile, UserRecord} from '~/records/UserRecord';
import markupStyles from '~/styles/Markup.module.css';
import * as DateUtils from '~/utils/DateUtils';

export const UserProfileBio: React.FC<{
	profile: ProfileRecord;
	profileData?: Readonly<UserProfile> | null;
	onShowMore?: () => void;
}> = observer(({profile, profileData, onShowMore}) => {
	const resolvedProfile = profileData ?? profile?.getEffectiveProfile() ?? null;

	if (!resolvedProfile?.bio) {
		return null;
	}

	const lineHeight = 1.28571;
	const shouldTruncate = !!onShowMore;

	if (!shouldTruncate) {
		return (
			<div className={styles.bioContainer}>
				<div className={clsx(markupStyles.markup, markupStyles.bio)}>
					<SafeMarkdown content={resolvedProfile.bio} options={{context: MarkdownContext.RESTRICTED_USER_BIO}} />
				</div>
			</div>
		);
	}

	return (
		<div className={styles.bioContainer}>
			<div
				className={clsx(markupStyles.markup, markupStyles.bio)}
				style={{
					display: '-webkit-box',
					WebkitLineClamp: 5,
					WebkitBoxOrient: 'vertical',
					overflow: 'hidden',
					lineHeight: `${lineHeight}em`,
				}}
			>
				<SafeMarkdown content={resolvedProfile.bio} options={{context: MarkdownContext.RESTRICTED_USER_BIO}} />
			</div>

			<FocusRing offset={-2}>
				<button type="button" onClick={onShowMore} className={styles.viewFullButton}>
					<Trans>View Full Profile</Trans>
				</button>
			</FocusRing>
		</div>
	);
});

export const UserProfileMembershipInfo: React.FC<{profile: ProfileRecord; user: UserRecord}> = observer(
	({profile, user}) => {
		const {t} = useLingui();
		if (profile?.guild && profile.guildMember) {
			return (
				<div className={styles.membershipContainer}>
					<span className={styles.membershipTitle}>
						<Trans>Member Since</Trans>
					</span>
					<div className={styles.membershipDates}>
						<div className={styles.membershipDate}>
							<Tooltip text={t`Fluxer`}>
								<div className={styles.membershipIcon}>
									<FluxerIcon className={clsx(styles.iconSmall, styles.textChat)} />
								</div>
							</Tooltip>
							<span className={styles.membershipDateText}>{DateUtils.getFormattedShortDate(user.createdAt)}</span>
						</div>
						<div className={styles.membershipDate}>
							<Tooltip text={profile.guild.name}>
								<div className={styles.membershipIcon}>
									<GuildIcon
										id={profile.guild.id}
										name={profile.guild.name}
										icon={profile.guild.icon}
										className={clsx(styles.membershipGuildIcon, styles.textXs)}
										sizePx={16}
									/>
								</div>
							</Tooltip>
							<span className={styles.membershipDateText}>
								{DateUtils.getFormattedShortDate(profile.guildMember.joinedAt)}
							</span>
						</div>
					</div>
				</div>
			);
		}

		return (
			<div className={styles.membershipContainer}>
				<span className={styles.membershipTitle}>
					<Trans>Fluxer Member Since</Trans>
				</span>
				<span className={styles.membershipDateText}>{DateUtils.getFormattedShortDate(user.createdAt)}</span>
			</div>
		);
	},
);

export const UserProfileRoles: React.FC<{
	profile: ProfileRecord;
	user: UserRecord;
	memberRoles: Array<GuildRoleRecord>;
	canManageRoles: boolean;
	forceMobile?: boolean;
}> = observer(({profile, user, memberRoles, canManageRoles, forceMobile}) => {
	return profile?.guild && profile?.guildMember && (memberRoles.length > 0 || canManageRoles) ? (
		<div className={styles.rolesContainer}>
			<div className={styles.rolesHeader}>
				<span className={styles.rolesTitle}>
					<Trans>Roles</Trans>
				</span>
				{canManageRoles && !forceMobile && <AddRoleButton guildId={profile.guild.id} userId={user.id} variant="icon" />}
			</div>
			<RoleList guildId={profile.guild.id} userId={user.id} roles={memberRoles} canManage={canManageRoles} />
			{canManageRoles && forceMobile && <AddRoleButton guildId={profile.guild.id} userId={user.id} variant="mobile" />}
		</div>
	) : null;
});
