// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/GuildMembersPage.module.css';
import {VANITY_URL_DESCRIPTOR} from '@app/features/channel/components/guild_members_page/GuildMembersPageDescriptors';
import {formatRecentOrFallback} from '@app/features/channel/components/guild_members_page/GuildMembersPageFormatting';
import type {MemberTableRowProps} from '@app/features/channel/components/guild_members_page/GuildMembersPageShared';
import type {GuildRole} from '@app/features/guild/models/GuildRole';
import Guilds from '@app/features/guild/state/Guilds';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import GuildMembers from '@app/features/member/state/GuildMembers';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import * as ColorUtils from '@app/features/theme/utils/ColorUtils';
import {ContextMenuCloseProvider, MenuItem} from '@app/features/ui/action_menu/ContextMenu';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {openRoleContextMenu} from '@app/features/ui/action_menu/RoleContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {BaseAvatar} from '@app/features/ui/components/BaseAvatar';
import {StatusAwareAvatar} from '@app/features/ui/components/StatusAwareAvatar';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import Users from '@app/features/user/state/Users';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {JoinSourceTypes} from '@fluxer/constants/src/GuildConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CrownIcon, DotsThreeVerticalIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {DateTime} from 'luxon';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo} from 'react';

const COMMUNITY_CREATOR_DESCRIPTOR = msg({
	message: 'Community creator',
	comment:
		'Join source label on the community members page. Shown for the member who originally created the community.',
});
const INVITE_DESCRIPTOR = msg({
	message: 'Invite ({sourceInviteCode})',
	comment:
		'Join source label on the community members page. Shows the invite code the member joined through, in parentheses.',
});
const INVITE_2_DESCRIPTOR = msg({
	message: 'Invite',
	comment: 'Join source label on the community members page. Shown when the member joined via an invite link.',
});
const BOT_INVITE_DESCRIPTOR = msg({
	message: 'Bot invite',
	comment: 'Join source label on the community members page. Shown when a bot was added via the OAuth bot invite flow.',
});
const PLATFORM_ADMIN_DESCRIPTOR = msg({
	message: 'Platform admin',
	comment: 'Join source label on the community members page. Shown when a platform admin force-added the member.',
});
const DISCOVERY_DESCRIPTOR = msg({
	message: 'Discovery',
	comment:
		'Join source label on the community members page. Shown when the member found the community via the Discovery surface.',
});
const UNKNOWN_USER_DESCRIPTOR = msg({
	message: 'Unknown user',
	comment: 'Fallback label on the community members page when the inviter user record is not available.',
});
const INVITED_BY_DESCRIPTOR = msg({
	message: 'Invited by',
	comment: 'Prefix label on the community members page before the name of the user who invited this member.',
});
const THIS_USER_WAS_FORCE_ADDED_TO_THIS_COMMUNITY_DESCRIPTOR = msg({
	message: 'This user was force-added to this community by a platform administrator.',
	comment: 'Tooltip on the community members page explaining the Platform admin join source. Keep calm and factual.',
});
const VIEW_MEMBER_PROFILE_FOR_DESCRIPTOR = msg({
	message: 'View member profile for {displayName}',
	comment: 'Accessible label for the row click target on the community members page.',
});
const COMMUNITY_OWNER_DESCRIPTOR = msg({
	message: 'Community owner',
	comment: 'Tooltip on the owner crown icon next to a member name on the community members page.',
});
const VIEW_ALL_ROLES_DESCRIPTOR = msg({
	message: 'View all roles',
	comment: 'Tooltip on the overflow indicator for a member row that has more roles than fit inline.',
});
const VIEW_ALL_ROLES_FOR_DESCRIPTOR = msg({
	message: 'View all roles for {displayName}',
	comment: 'Accessible label for the overflow indicator showing extra member roles.',
});
const MEMBER_ACTIONS_FOR_DESCRIPTOR = msg({
	message: 'Member actions for {displayName}',
	comment: 'Accessible label for the actions menu button on a member row.',
});
export const MemberTableRow: React.FC<MemberTableRowProps> = observer(
	({data, guildId, isOwner, activeMenuMemberId, contextMenuMemberId, onActionsClick, onContextMenu, onRowClick}) => {
		const {i18n} = useLingui();
		const isActionsMenuActive = activeMenuMemberId === data.userId;
		const isContextMenuActive = contextMenuMemberId === data.userId;
		const user = Users.getUser(data.userId);
		const member = GuildMembers.getMember(guildId, data.userId);
		const roleColor = member?.getColorString();
		const roles = Guilds.getGuildRoles(guildId);
		const memberRoles = useMemo(() => {
			const roleIds = member ? Array.from(member.roles) : data.roleIds;
			return roleIds
				.map((id) => roles.find((r) => r.id === id))
				.filter((r): r is GuildRole => r != null)
				.sort((a, b) => b.position - a.position);
		}, [member, data.roleIds, roles]);
		const topRole = memberRoles[0];
		const extraRolesCount = memberRoles.length - 1;
		const joinedAt = member?.joinedAt ?? data.joinedAt;
		const joinedAtRelative = useMemo(() => formatRecentOrFallback(joinedAt, i18n), [joinedAt, i18n.locale]);
		const joinedAtAbsolute = useMemo(() => DateTime.fromJSDate(joinedAt).toFormat('d LLLL yyyy, HH:mm'), [joinedAt]);
		const userCreatedAtRelative = useMemo(
			() => formatRecentOrFallback(data.userCreatedAt, i18n),
			[data.userCreatedAt, i18n.locale],
		);
		const userCreatedAtAbsolute = useMemo(
			() => DateTime.fromJSDate(data.userCreatedAt).toFormat('d LLLL yyyy, HH:mm'),
			[data.userCreatedAt],
		);
		const sourceInviteCode = data.sourceInviteCode;
		const joinSourceType = data.joinSourceType;
		const inviterId = data.inviterId;
		const joinMethodLabel = useMemo(() => {
			switch (joinSourceType) {
				case JoinSourceTypes.CREATOR:
					return i18n._(COMMUNITY_CREATOR_DESCRIPTOR);
				case JoinSourceTypes.INSTANT_INVITE:
					return sourceInviteCode ? i18n._(INVITE_DESCRIPTOR, {sourceInviteCode}) : i18n._(INVITE_2_DESCRIPTOR);
				case JoinSourceTypes.VANITY_URL:
					return i18n._(VANITY_URL_DESCRIPTOR);
				case JoinSourceTypes.BOT_INVITE:
					return i18n._(BOT_INVITE_DESCRIPTOR);
				case JoinSourceTypes.ADMIN_FORCE_ADD:
					return i18n._(PLATFORM_ADMIN_DESCRIPTOR);
				case JoinSourceTypes.DISCOVERY:
					return i18n._(DISCOVERY_DESCRIPTOR);
				default:
					return i18n._(INVITE_2_DESCRIPTOR);
			}
		}, [sourceInviteCode, joinSourceType, i18n.locale]);
		const joinMethodTooltip = useMemo(() => {
			if (joinSourceType === JoinSourceTypes.INSTANT_INVITE && inviterId) {
				const inviterIdValue = inviterId;
				return () => {
					const inviterUser = Users.getUser(inviterIdValue);
					const inviterMember = GuildMembers.getMember(guildId, inviterIdValue);
					const inviterName = inviterUser
						? NicknameUtils.getNickname(inviterUser, guildId)
						: i18n._(UNKNOWN_USER_DESCRIPTOR);
					const inviterColor = inviterMember?.getColorString();
					return (
						<span
							className={styles.inviterTooltip}
							data-flx="channel.guild-members-page.join-method-tooltip.inviter-tooltip"
						>
							{i18n._(INVITED_BY_DESCRIPTOR)}
							<span
								className={styles.inviterUser}
								data-flx="channel.guild-members-page.join-method-tooltip.inviter-user"
							>
								{inviterUser && (
									<StatusAwareAvatar
										user={inviterUser}
										size={16}
										guildId={guildId}
										disablePresence
										data-flx="channel.guild-members-page.join-method-tooltip.status-aware-avatar"
									/>
								)}
								<span
									style={inviterColor ? {color: inviterColor} : undefined}
									data-flx="channel.guild-members-page.join-method-tooltip.span"
								>
									{inviterName}
								</span>
							</span>
						</span>
					);
				};
			}
			if (joinSourceType === JoinSourceTypes.ADMIN_FORCE_ADD) {
				return i18n._(THIS_USER_WAS_FORCE_ADDED_TO_THIS_COMMUNITY_DESCRIPTOR);
			}
			return undefined;
		}, [joinSourceType, inviterId, guildId, i18n.locale]);
		const handleContextMenu = useCallback(
			(event: React.MouseEvent<HTMLElement>) => {
				event.preventDefault();
				onContextMenu(data, event);
			},
			[data, onContextMenu],
		);
		const handleActionsClick = useCallback(
			(event: React.MouseEvent<HTMLElement>) => {
				event.stopPropagation();
				onActionsClick(data, event);
			},
			[data, onActionsClick],
		);
		const handleRowClick = useCallback(() => {
			onRowClick(data);
		}, [data, onRowClick]);
		const handleRowKeyDown = useCallback(
			(event: React.KeyboardEvent<HTMLDivElement>) => {
				if (event.target !== event.currentTarget || !isKeyboardActivationKey(event.key)) return;
				event.preventDefault();
				onRowClick(data);
			},
			[data, onRowClick],
		);
		const handleOverflowRolesClick = useCallback(
			(event: React.MouseEvent<HTMLButtonElement>) => {
				event.stopPropagation();
				ContextMenuCommands.openFromElementBottomRight(event, ({onClose}) => (
					<ContextMenuCloseProvider
						value={onClose}
						data-flx="channel.guild-members-page.handle-overflow-roles-click.context-menu-close-provider"
					>
						<MenuGroup data-flx="channel.guild-members-page.handle-overflow-roles-click.menu-group">
							{memberRoles.map((role) => (
								<MenuItem
									key={role.id}
									label={role.name}
									closeOnSelect={false}
									data-flx="channel.guild-members-page.handle-overflow-roles-click.menu-item"
								>
									{/* biome-ignore lint/a11y/noStaticElementInteractions: context-menu affordance inside a MenuItem. */}
									<div
										className={styles.readonlyRoleItem}
										onContextMenu={(event) => openRoleContextMenu(event, role.id)}
										data-flx="channel.guild-members-page.handle-overflow-roles-click.readonly-role-item.open-role-context-menu"
									>
										<div
											className={styles.readonlyRoleLabel}
											data-flx="channel.guild-members-page.handle-overflow-roles-click.readonly-role-label"
										>
											<span
												className={styles.readonlyRoleDot}
												style={{
													backgroundColor: role.color ? ColorUtils.int2rgb(role.color) : 'var(--text-tertiary)',
												}}
												data-flx="channel.guild-members-page.handle-overflow-roles-click.readonly-role-dot"
											/>
											<span data-flx="channel.guild-members-page.handle-overflow-roles-click.span">{role.name}</span>
										</div>
										<div
											className={styles.readonlyRoleSpacer}
											data-flx="channel.guild-members-page.handle-overflow-roles-click.readonly-role-spacer"
										/>
									</div>
								</MenuItem>
							))}
						</MenuGroup>
					</ContextMenuCloseProvider>
				));
			},
			[memberRoles],
		);
		const topRoleColor = topRole?.color ? ColorUtils.int2rgb(topRole.color) : undefined;
		const rolePillBg = topRoleColor
			? `color-mix(in srgb, ${topRoleColor} 20%, var(--background-secondary-alt))`
			: 'var(--background-secondary)';
		const handleTopRoleContextMenu = useCallback(
			(event: React.MouseEvent<HTMLSpanElement>) => {
				if (!topRole) return;
				openRoleContextMenu(event, topRole.id);
			},
			[topRole],
		);
		const displayName = member?.nick ?? data.nickname ?? data.displayName;
		const tag = user?.tag ?? data.tag;
		return (
			<div
				className={styles.row}
				role="row"
				data-menu-active={isContextMenuActive || isActionsMenuActive ? '' : undefined}
				onContextMenu={handleContextMenu}
				onClick={handleRowClick}
				onKeyDown={handleRowKeyDown}
				tabIndex={0}
				aria-label={i18n._(VIEW_MEMBER_PROFILE_FOR_DESCRIPTOR, {displayName})}
				data-flx="channel.guild-members-page.member-table-row.row"
			>
				<div
					className={clsx(styles.cell, styles.nameColumn)}
					role="cell"
					data-flx="channel.guild-members-page.member-table-row.cell"
				>
					<div className={styles.nameCell} data-flx="channel.guild-members-page.member-table-row.name-cell">
						{user ? (
							<StatusAwareAvatar
								user={user}
								size={32}
								guildId={guildId}
								disablePresence
								data-flx="channel.guild-members-page.member-table-row.status-aware-avatar"
							/>
						) : (
							<BaseAvatar
								size={32}
								avatarUrl={AvatarUtils.getUserAvatarURL({id: data.userId, avatar: null}, false)}
								userTag={tag}
								data-flx="channel.guild-members-page.member-table-row.base-avatar"
							/>
						)}
						<div className={styles.nameInfo} data-flx="channel.guild-members-page.member-table-row.name-info">
							<div className={styles.nameRow} data-flx="channel.guild-members-page.member-table-row.name-row">
								<span
									className={styles.displayName}
									style={roleColor ? {color: roleColor} : undefined}
									data-flx="channel.guild-members-page.member-table-row.display-name"
								>
									{displayName}
								</span>
								{isOwner && (
									<Tooltip
										text={i18n._(COMMUNITY_OWNER_DESCRIPTOR)}
										data-flx="channel.guild-members-page.member-table-row.tooltip"
									>
										<CrownIcon
											className={styles.ownerIcon}
											weight="fill"
											data-flx="channel.guild-members-page.member-table-row.owner-icon"
										/>
									</Tooltip>
								)}
							</div>
							<span className={styles.tag} data-flx="channel.guild-members-page.member-table-row.tag">
								{tag}
							</span>
						</div>
					</div>
				</div>
				<div
					className={clsx(styles.cell, styles.dateColumn)}
					role="cell"
					data-flx="channel.guild-members-page.member-table-row.cell--2"
				>
					<div className={styles.cellContent} data-flx="channel.guild-members-page.member-table-row.cell-content">
						<Tooltip text={joinedAtAbsolute} data-flx="channel.guild-members-page.member-table-row.tooltip--2">
							<span
								className={styles.timestampText}
								data-flx="channel.guild-members-page.member-table-row.timestamp-text"
							>
								{joinedAtRelative}
							</span>
						</Tooltip>
					</div>
				</div>
				<div
					className={clsx(styles.cell, styles.dateColumn)}
					role="cell"
					data-flx="channel.guild-members-page.member-table-row.cell--3"
				>
					<div className={styles.cellContent} data-flx="channel.guild-members-page.member-table-row.cell-content--2">
						<Tooltip text={userCreatedAtAbsolute} data-flx="channel.guild-members-page.member-table-row.tooltip--3">
							<span
								className={styles.timestampText}
								data-flx="channel.guild-members-page.member-table-row.timestamp-text--2"
							>
								{userCreatedAtRelative}
							</span>
						</Tooltip>
					</div>
				</div>
				<div
					className={clsx(styles.cell, styles.joinMethodColumn)}
					role="cell"
					data-flx="channel.guild-members-page.member-table-row.cell--4"
				>
					<div className={styles.cellContent} data-flx="channel.guild-members-page.member-table-row.cell-content--3">
						{joinMethodTooltip ? (
							<Tooltip text={joinMethodTooltip} data-flx="channel.guild-members-page.member-table-row.tooltip--4">
								<span className={styles.pill} data-flx="channel.guild-members-page.member-table-row.pill">
									{joinMethodLabel}
								</span>
							</Tooltip>
						) : (
							<span className={styles.pill} data-flx="channel.guild-members-page.member-table-row.pill--2">
								{joinMethodLabel}
							</span>
						)}
					</div>
				</div>
				<div
					className={clsx(styles.cell, styles.rolesColumn)}
					role="cell"
					data-flx="channel.guild-members-page.member-table-row.cell--5"
				>
					<div className={styles.rolesCell} data-flx="channel.guild-members-page.member-table-row.roles-cell">
						{topRole && (
							// biome-ignore lint/a11y/noStaticElementInteractions: context-menu affordance on a role pill.
							<span
								className={styles.rolePill}
								style={{backgroundColor: rolePillBg}}
								onContextMenu={handleTopRoleContextMenu}
								data-flx="channel.guild-members-page.member-table-row.role-pill.top-role-context-menu"
							>
								<span
									className={styles.roleDot}
									style={{backgroundColor: topRoleColor ?? 'var(--text-tertiary)'}}
									data-flx="channel.guild-members-page.member-table-row.role-dot"
								/>
								{topRole.name}
							</span>
						)}
						{extraRolesCount > 0 && (
							<Tooltip
								text={i18n._(VIEW_ALL_ROLES_DESCRIPTOR)}
								data-flx="channel.guild-members-page.member-table-row.tooltip--5"
							>
								<button
									type="button"
									className={styles.overflowPill}
									onClick={handleOverflowRolesClick}
									aria-label={i18n._(VIEW_ALL_ROLES_FOR_DESCRIPTOR, {displayName})}
									aria-haspopup="menu"
									data-flx="channel.guild-members-page.member-table-row.overflow-pill.overflow-roles-click.button"
								>
									+{extraRolesCount}
								</button>
							</Tooltip>
						)}
					</div>
				</div>
				<div
					className={clsx(styles.cell, styles.actionsColumn)}
					role="cell"
					data-flx="channel.guild-members-page.member-table-row.cell--6"
				>
					<button
						type="button"
						className={styles.actionsButton}
						data-menu-active={isActionsMenuActive ? '' : undefined}
						onClick={handleActionsClick}
						aria-label={i18n._(MEMBER_ACTIONS_FOR_DESCRIPTOR, {displayName})}
						aria-haspopup="menu"
						data-flx="channel.guild-members-page.member-table-row.actions-button.actions-click"
					>
						<DotsThreeVerticalIcon
							weight="bold"
							size={remFromPx(18)}
							data-flx="channel.guild-members-page.member-table-row.dots-three-vertical-icon"
						/>
					</button>
				</div>
			</div>
		);
	},
);
