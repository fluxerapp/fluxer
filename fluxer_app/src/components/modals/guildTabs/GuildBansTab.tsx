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
import {MagnifyingGlassIcon, ProhibitIcon} from '@phosphor-icons/react';
import {matchSorter} from 'match-sorter';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import type {GuildBan} from '~/actions/GuildActionCreators';
import * as GuildActionCreators from '~/actions/GuildActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {Input} from '~/components/form/Input';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {BannedUserActionsSheet} from '~/components/modals/guildTabs/BannedUserActionsSheet';
import {UserListItem} from '~/components/modals/guildTabs/UserListItem';
import {StatusSlate} from '~/components/modals/shared/StatusSlate';
import {BannedUserContextMenu} from '~/components/uikit/ContextMenu/BannedUserContextMenu';
import {Scroller} from '~/components/uikit/Scroller';
import {Spinner} from '~/components/uikit/Spinner';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import * as AvatarUtils from '~/utils/AvatarUtils';
import styles from './MemberListStyles.module.css';

const GuildBansTab: React.FC<{guildId: string}> = observer(({guildId}) => {
	const {t} = useLingui();
	const [bans, setBans] = React.useState<Array<GuildBan>>([]);
	const [isLoading, setIsLoading] = React.useState(true);
	const [searchQuery, setSearchQuery] = React.useState('');
	const [activeMenuUserId, setActiveMenuUserId] = React.useState<string | null>(null);
	const [activeSheetBan, setActiveSheetBan] = React.useState<GuildBan | null>(null);
	const isMobile = MobileLayoutStore.enabled;

	const formatTag = React.useCallback(
		(ban: GuildBan) => ban.user.tag ?? `${ban.user.username}#${(ban.user.discriminator ?? '').padStart(4, '0')}`,
		[],
	);

	const fetchBans = React.useCallback(async () => {
		setIsLoading(true);
		try {
			const fetchedBans = await GuildActionCreators.fetchBans(guildId);
			setBans(fetchedBans);
		} catch (error) {
			console.error('Failed to fetch bans:', error);
			ToastActionCreators.createToast({
				type: 'error',
				children: <Trans>Failed to load bans. Please try again.</Trans>,
			});
		} finally {
			setIsLoading(false);
		}
	}, [guildId]);

	React.useEffect(() => {
		fetchBans();
	}, [fetchBans]);

	const filteredBans = React.useMemo(() => {
		if (!searchQuery) return bans;

		return matchSorter(bans, searchQuery, {
			keys: [(ban) => ban.user.username, (ban) => formatTag(ban), (ban) => ban.reason || ''],
		});
	}, [bans, searchQuery]);

	const handleUnban = React.useCallback(
		(ban: GuildBan) => {
			ModalActionCreators.push(
				modal(() => (
					<ConfirmModal
						title={t`Revoke Ban`}
						description={
							<div>
								<Trans>
									Are you sure you want to revoke the ban for <strong>{ban.user.username}</strong>? They will be able to
									rejoin the community.
								</Trans>
							</div>
						}
						primaryText={t`Revoke Ban`}
						primaryVariant="danger-primary"
						secondaryText={t`Cancel`}
						onPrimary={async () => {
							try {
								await GuildActionCreators.unbanMember(guildId, ban.user.id);
								ToastActionCreators.createToast({
									type: 'success',
									children: <Trans>Successfully revoked ban for {ban.user.username}</Trans>,
								});
								await fetchBans();
							} catch (error) {
								console.error('Failed to unban member:', error);
								ToastActionCreators.createToast({
									type: 'error',
									children: <Trans>Failed to revoke ban. Please try again.</Trans>,
								});
							}
						}}
					/>
				)),
			);
		},
		[guildId, fetchBans],
	);

	const handleBanContextMenu = React.useCallback(
		(ban: GuildBan, event: React.MouseEvent<HTMLElement>, fromButton?: boolean) => {
			if (fromButton) {
				setActiveMenuUserId(ban.user.id);
			} else {
				setActiveMenuUserId(null);
			}
			ContextMenuActionCreators.openFromEvent(
				event,
				({onClose}) => <BannedUserContextMenu ban={ban} onClose={onClose} onRevoke={() => handleUnban(ban)} />,
				{onClose: () => setActiveMenuUserId(null)},
			);
		},
		[handleUnban],
	);

	if (isLoading) {
		return (
			<div className={styles.loadingContainer}>
				<Spinner />
				<p className={styles.loadingText}>
					<Trans>Loading bans...</Trans>
				</p>
			</div>
		);
	}

	return (
		<div className={styles.container}>
			<div className={styles.header}>
				<h2 className={styles.title}>
					<Trans>Banned Users</Trans>
				</h2>
				<p className={styles.subtitle}>
					<Trans>View and manage banned users.</Trans>
				</p>
			</div>

			<div className={styles.controls}>
				<Input
					type="text"
					placeholder={t`Search bans...`}
					value={searchQuery}
					onChange={(e) => setSearchQuery(e.target.value)}
					leftIcon={<MagnifyingGlassIcon size={16} weight="bold" />}
					className={styles.searchInput}
				/>
			</div>

			{bans.length === 0 && (
				<StatusSlate
					Icon={ProhibitIcon}
					title={<Trans>No Banned Users</Trans>}
					description={<Trans>No users are currently banned from this community.</Trans>}
					fullHeight={true}
				/>
			)}

			{filteredBans.length === 0 && searchQuery && bans.length > 0 && (
				<div className={styles.notice}>
					<p className={styles.noticeText}>
						<Trans>No bans found matching your search.</Trans>
					</p>
				</div>
			)}

			{filteredBans.length > 0 && (
				<div className={styles.scrollContainer}>
					<Scroller fade={true} reserveScrollbarTrack={false} key="guild-bans-scroller">
						<div className={styles.memberList}>
							<div className={styles.memberGroup}>
								{filteredBans.map((ban, index) => {
									const userTag = formatTag(ban);
									const avatarUrl = AvatarUtils.getUserAvatarURL(ban.user, false);

									return (
										<React.Fragment key={ban.user.id}>
											<UserListItem
												user={ban.user}
												avatarUrl={avatarUrl}
												displayName={ban.user.username}
												tag={userTag}
												isMobile={isMobile}
												isMenuActive={activeMenuUserId === ban.user.id}
												onContextMenu={(e, fromButton) => handleBanContextMenu(ban, e, fromButton)}
												onActivate={() => setActiveSheetBan(ban)}
											/>
											{index < filteredBans.length - 1 && <div className={styles.divider} />}
										</React.Fragment>
									);
								})}
							</div>
						</div>
					</Scroller>
				</div>
			)}

			{activeSheetBan && (
				<BannedUserActionsSheet
					isOpen={true}
					onClose={() => setActiveSheetBan(null)}
					ban={activeSheetBan}
					onRevoke={() => handleUnban(activeSheetBan)}
				/>
			)}
		</div>
	);
});

export default GuildBansTab;
