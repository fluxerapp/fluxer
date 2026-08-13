// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {StatusSlate} from '@app/features/app/components/dialogs/shared/StatusSlate';
import type {GuildBan} from '@app/features/guild/commands/GuildCommands';
import * as GuildCommands from '@app/features/guild/commands/GuildCommands';
import {showGuildErrorModal} from '@app/features/guild/components/alerts/GuildErrorModalUtils';
import {BannedUserActionsSheet} from '@app/features/guild/components/modals/guild_tabs/BannedUserActionsSheet';
import styles from '@app/features/guild/components/modals/guild_tabs/MemberListStyles.module.css';
import {UserListItem} from '@app/features/guild/components/modals/guild_tabs/UserListItem';
import {CANCEL_DESCRIPTOR, TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {BannedUserContextMenu} from '@app/features/ui/action_menu/BannedUserContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Spinner} from '@app/features/ui/components/Spinner';
import {useContextMenuTrigger} from '@app/features/ui/hooks/useContextMenuTrigger';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon, ProhibitIcon} from '@phosphor-icons/react';
import {matchSorter} from 'match-sorter';
import {observer} from 'mobx-react-lite';
import React, {useCallback, useEffect, useMemo, useState} from 'react';

const REVOKE_BAN_DESCRIPTOR = msg({
	message: 'Revoke ban',
	comment: 'Short label in the guild bans tab. Keep it concise. Keep the tone plain and specific.',
});
const SEARCH_BANS_DESCRIPTOR = msg({
	message: 'Search bans',
	comment: 'Button or menu action label in the guild bans tab. Keep it concise. Keep the tone plain and specific.',
});
const COULDN_T_LOAD_BANS_DESCRIPTOR = msg({
	message: "Couldn't load bans",
	comment: 'Error modal title shown when loading the community ban list fails.',
});
const COULDN_T_REVOKE_BAN_DESCRIPTOR = msg({
	message: "Couldn't revoke ban",
	comment: 'Error modal title shown when revoking a community ban fails.',
});
const logger = new Logger('GuildBansTab');
const GuildBansTab: React.FC<{guildId: string}> = observer(({guildId}) => {
	const {i18n} = useLingui();
	const [bans, setBans] = useState<Array<GuildBan>>([]);
	const [isLoading, setIsLoading] = useState(true);
	const [searchQuery, setSearchQuery] = useState('');
	const [activeMenuUserId, setActiveMenuUserId] = useState<string | null>(null);
	const [activeSheetBan, setActiveSheetBan] = useState<GuildBan | null>(null);
	const {isOpen: isMenuOpen, withTracking} = useContextMenuTrigger();
	const isMobile = MobileLayout.enabled;
	const getRawTag = useCallback(
		(ban: GuildBan) => ban.user.tag ?? `${ban.user.username}#${(ban.user.discriminator ?? '').padStart(4, '0')}`,
		[],
	);
	const formatTag = useCallback((ban: GuildBan) => NicknameUtils.formatTagForStreamerMode(getRawTag(ban)), [getRawTag]);
	const fetchBans = useCallback(async () => {
		setIsLoading(true);
		try {
			const fetchedBans = await GuildCommands.fetchBans(guildId);
			setBans(fetchedBans);
		} catch (error) {
			logger.error('Failed to fetch bans', error);
			showGuildErrorModal({
				title: i18n._(COULDN_T_LOAD_BANS_DESCRIPTOR),
				message: i18n._(TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR),
				dataFlx: 'guild.guild-tabs.guild-bans-tab.load-bans-error-modal',
			});
		} finally {
			setIsLoading(false);
		}
	}, [guildId, i18n]);
	useEffect(() => {
		fetchBans();
	}, [fetchBans]);
	const filteredBans = useMemo(() => {
		if (!searchQuery) return bans;
		return matchSorter(bans, searchQuery, {
			keys: [
				(ban) => NicknameUtils.getDisplayName(ban.user),
				(ban) => ban.user.username,
				(ban) => getRawTag(ban),
				(ban) => ban.reason || '',
			],
		});
	}, [bans, getRawTag, searchQuery]);
	const handleUnban = useCallback(
		(ban: GuildBan) => {
			ModalCommands.push(
				modal(() => (
					<ConfirmModal
						title={i18n._(REVOKE_BAN_DESCRIPTOR)}
						description={
							<div data-flx="guild.guild-tabs.guild-bans-tab.handle-unban.div">
								<Trans>
									Are you sure you want to revoke the ban for{' '}
									<strong data-flx="guild.guild-tabs.guild-bans-tab.handle-unban.strong">
										{NicknameUtils.getDisplayName(ban.user)}
									</strong>
									? They will be able to rejoin the community.
								</Trans>
							</div>
						}
						primaryText={i18n._(REVOKE_BAN_DESCRIPTOR)}
						primaryVariant="danger"
						secondaryText={i18n._(CANCEL_DESCRIPTOR)}
						onPrimary={async () => {
							try {
								await GuildCommands.unbanMember(guildId, ban.user.id);
								ToastCommands.createToast({
									type: 'success',
									children: <Trans>Revoked ban for {NicknameUtils.getDisplayName(ban.user)}</Trans>,
								});
								await fetchBans();
							} catch (error) {
								logger.error('Failed to unban member', error);
								showGuildErrorModal({
									title: i18n._(COULDN_T_REVOKE_BAN_DESCRIPTOR),
									message: i18n._(TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR),
									dataFlx: 'guild.guild-tabs.guild-bans-tab.revoke-ban-error-modal',
								});
							}
						}}
						data-flx="guild.guild-tabs.guild-bans-tab.handle-unban.confirm-modal"
					/>
				)),
			);
		},
		[guildId, fetchBans, i18n],
	);
	const handleBanContextMenu = useCallback(
		(ban: GuildBan, event: React.MouseEvent<HTMLElement>, fromButton?: boolean) => {
			if (fromButton) {
				setActiveMenuUserId(ban.user.id);
			} else {
				setActiveMenuUserId(null);
			}
			ContextMenuCommands.openFromEvent(
				event,
				({onClose}) => (
					<BannedUserContextMenu
						ban={ban}
						onClose={onClose}
						onRevoke={() => handleUnban(ban)}
						data-flx="guild.guild-tabs.guild-bans-tab.handle-ban-context-menu.banned-user-context-menu"
					/>
				),
				withTracking(),
			);
		},
		[handleUnban, withTracking],
	);
	if (isLoading) {
		return (
			<div className={styles.loadingContainer} data-flx="guild.guild-tabs.guild-bans-tab.loading-container">
				<Spinner data-flx="guild.guild-tabs.guild-bans-tab.spinner" />
				<p className={styles.loadingText} data-flx="guild.guild-tabs.guild-bans-tab.loading-text">
					<Trans>Loading banned users</Trans>
				</p>
			</div>
		);
	}
	return (
		<div className={styles.container} data-flx="guild.guild-tabs.guild-bans-tab.container">
			<div className={styles.header} data-flx="guild.guild-tabs.guild-bans-tab.header">
				<h2 className={styles.title} data-flx="guild.guild-tabs.guild-bans-tab.title">
					<Trans>Banned users</Trans>
				</h2>
				<p className={styles.subtitle} data-flx="guild.guild-tabs.guild-bans-tab.subtitle">
					<Trans>View and manage banned users.</Trans>
				</p>
			</div>
			<div className={styles.controls} data-flx="guild.guild-tabs.guild-bans-tab.controls">
				<Input
					type="text"
					placeholder={i18n._(SEARCH_BANS_DESCRIPTOR)}
					value={searchQuery}
					onChange={(e) => setSearchQuery(e.target.value)}
					leftIcon={
						<MagnifyingGlassIcon
							size={remFromPx(16)}
							weight="bold"
							data-flx="guild.guild-tabs.guild-bans-tab.magnifying-glass-icon"
						/>
					}
					className={styles.searchInput}
					data-flx="guild.guild-tabs.guild-bans-tab.search-input.set-search-query.text"
				/>
			</div>
			{bans.length === 0 && (
				<StatusSlate
					Icon={ProhibitIcon}
					title={<Trans>No banned users</Trans>}
					description={<Trans>No users are currently banned from this community.</Trans>}
					fullHeight={true}
					data-flx="guild.guild-tabs.guild-bans-tab.status-slate"
				/>
			)}
			{filteredBans.length === 0 && searchQuery && bans.length > 0 && (
				<div className={styles.notice} data-flx="guild.guild-tabs.guild-bans-tab.notice">
					<p className={styles.noticeText} data-flx="guild.guild-tabs.guild-bans-tab.notice-text">
						<Trans>No bans found matching your search.</Trans>
					</p>
				</div>
			)}
			{filteredBans.length > 0 && (
				<div className={styles.scrollContainer} data-flx="guild.guild-tabs.guild-bans-tab.scroll-container">
					<div data-flx="guild.guild-tabs.guild-bans-tab.div">
						<div className={styles.memberList} data-flx="guild.guild-tabs.guild-bans-tab.member-list">
							<div className={styles.memberGroup} data-flx="guild.guild-tabs.guild-bans-tab.member-group">
								{filteredBans.map((ban, index) => {
									const userTag = formatTag(ban);
									const userDisplayName = NicknameUtils.getDisplayName(ban.user);
									const avatarUrl = AvatarUtils.getUserAvatarURL(ban.user, false);
									return (
										<React.Fragment key={ban.user.id}>
											<UserListItem
												user={ban.user}
												avatarUrl={avatarUrl}
												displayName={userDisplayName}
												tag={userTag}
												isMobile={isMobile}
												isMenuActive={isMenuOpen && activeMenuUserId === ban.user.id}
												onContextMenu={(e, fromButton) => handleBanContextMenu(ban, e, fromButton)}
												onActivate={() => setActiveSheetBan(ban)}
												data-flx="guild.guild-tabs.guild-bans-tab.user-list-item.ban-context-menu"
											/>
											{index < filteredBans.length - 1 && (
												<div className={styles.divider} data-flx="guild.guild-tabs.guild-bans-tab.divider" />
											)}
										</React.Fragment>
									);
								})}
							</div>
						</div>
					</div>
				</div>
			)}
			{activeSheetBan && (
				<BannedUserActionsSheet
					isOpen={true}
					onClose={() => setActiveSheetBan(null)}
					ban={activeSheetBan}
					onRevoke={() => handleUnban(activeSheetBan)}
					data-flx="guild.guild-tabs.guild-bans-tab.banned-user-actions-sheet"
				/>
			)}
		</div>
	);
});

export default GuildBansTab;
