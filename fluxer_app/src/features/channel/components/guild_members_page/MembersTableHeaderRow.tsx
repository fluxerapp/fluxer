// SPDX-License-Identifier: AGPL-3.0-or-later

import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import styles from '@app/features/channel/components/GuildMembersPage.module.css';
import {ROLES_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {FunnelIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';

const FILTER_BY_MEMBER_SINCE_DESCRIPTOR = msg({
	message: 'Filter by member since',
	comment: 'Accessible label for the join-date filter chip on the community members page.',
});
const FILTER_BY_ACCOUNT_CREATION_DATE_DESCRIPTOR = msg({
	message: 'Filter by account creation date',
	comment: 'Accessible label for the account-age filter chip on the community members page.',
});
const FILTER_BY_JOIN_METHOD_DESCRIPTOR = msg({
	message: 'Filter by join method',
	comment: 'Accessible label for the join-source filter chip on the community members page.',
});
const FILTER_BY_ROLES_DESCRIPTOR = msg({
	message: 'Filter by roles',
	comment: 'Accessible label for the role filter chip on the community members page.',
});
const JOINED_PRODUCT_DESCRIPTOR = msg({
	message: 'Joined {productName}',
	comment: 'Community members table column header for the date the account joined Fluxer.',
});

export interface MembersTableHeaderRowProps {
	memberSinceActive: boolean;
	joinedFluxerActive: boolean;
	joinMethodActive: boolean;
	rolesActive: boolean;
	onMemberSinceFilterOpen: (event: React.MouseEvent<HTMLButtonElement>) => void;
	onJoinedFluxerFilterOpen: (event: React.MouseEvent<HTMLButtonElement>) => void;
	onJoinMethodFilterOpen: (event: React.MouseEvent<HTMLButtonElement>) => void;
	onRolesFilterOpen: (event: React.MouseEvent<HTMLButtonElement>) => void;
}

export function MembersTableHeaderRow({
	memberSinceActive,
	joinedFluxerActive,
	joinMethodActive,
	rolesActive,
	onMemberSinceFilterOpen,
	onJoinedFluxerFilterOpen,
	onJoinMethodFilterOpen,
	onRolesFilterOpen,
}: MembersTableHeaderRowProps) {
	const {i18n} = useLingui();
	return (
		<div
			className={styles.tableHead}
			role="rowgroup"
			data-flx="channel.guild-members-page.members-table-view.table-head"
		>
			<div
				className={styles.tableHeadRow}
				role="row"
				tabIndex={0}
				data-flx="channel.guild-members-page.members-table-view.table-head-row"
			>
				<div
					className={clsx(styles.headerCell, styles.nameColumn)}
					role="columnheader"
					tabIndex={0}
					data-flx="channel.guild-members-page.members-table-view.header-cell"
				>
					<Trans>Name</Trans>
				</div>
				<div
					className={clsx(styles.headerCell, styles.dateColumn)}
					role="columnheader"
					tabIndex={0}
					data-flx="channel.guild-members-page.members-table-view.header-cell--2"
				>
					<div className={styles.thContent} data-flx="channel.guild-members-page.members-table-view.th-content">
						<Trans>Member since</Trans>
						<button
							type="button"
							className={clsx(styles.filterButton, memberSinceActive && styles.filterButtonActive)}
							onClick={onMemberSinceFilterOpen}
							aria-label={i18n._(FILTER_BY_MEMBER_SINCE_DESCRIPTOR)}
							aria-pressed={memberSinceActive}
							aria-haspopup="menu"
							data-flx="channel.guild-members-page.members-table-view.filter-button.member-since-filter-open"
						>
							<FunnelIcon
								size={remFromPx(12)}
								weight={memberSinceActive ? 'fill' : 'bold'}
								data-flx="channel.guild-members-page.members-table-view.funnel-icon"
							/>
						</button>
					</div>
				</div>
				<div
					className={clsx(styles.headerCell, styles.dateColumn)}
					role="columnheader"
					tabIndex={0}
					data-flx="channel.guild-members-page.members-table-view.header-cell--3"
				>
					<div className={styles.thContent} data-flx="channel.guild-members-page.members-table-view.th-content--2">
						{i18n._(JOINED_PRODUCT_DESCRIPTOR, {productName: PRODUCT_NAME})}
						<button
							type="button"
							className={clsx(styles.filterButton, joinedFluxerActive && styles.filterButtonActive)}
							onClick={onJoinedFluxerFilterOpen}
							aria-label={i18n._(FILTER_BY_ACCOUNT_CREATION_DATE_DESCRIPTOR)}
							aria-pressed={joinedFluxerActive}
							aria-haspopup="menu"
							data-flx="channel.guild-members-page.members-table-view.filter-button.joined-fluxer-filter-open"
						>
							<FunnelIcon
								size={remFromPx(12)}
								weight={joinedFluxerActive ? 'fill' : 'bold'}
								data-flx="channel.guild-members-page.members-table-view.funnel-icon--2"
							/>
						</button>
					</div>
				</div>
				<div
					className={clsx(styles.headerCell, styles.joinMethodColumn)}
					role="columnheader"
					tabIndex={0}
					data-flx="channel.guild-members-page.members-table-view.header-cell--4"
				>
					<div className={styles.thContent} data-flx="channel.guild-members-page.members-table-view.th-content--3">
						<Trans>Join method</Trans>
						<button
							type="button"
							className={clsx(styles.filterButton, joinMethodActive && styles.filterButtonActive)}
							onClick={onJoinMethodFilterOpen}
							aria-label={i18n._(FILTER_BY_JOIN_METHOD_DESCRIPTOR)}
							aria-pressed={joinMethodActive}
							aria-haspopup="menu"
							data-flx="channel.guild-members-page.members-table-view.filter-button.join-method-filter-open"
						>
							<FunnelIcon
								size={remFromPx(12)}
								weight={joinMethodActive ? 'fill' : 'bold'}
								data-flx="channel.guild-members-page.members-table-view.funnel-icon--3"
							/>
						</button>
					</div>
				</div>
				<div
					className={clsx(styles.headerCell, styles.rolesColumn)}
					role="columnheader"
					tabIndex={0}
					data-flx="channel.guild-members-page.members-table-view.header-cell--5"
				>
					<div className={styles.thContent} data-flx="channel.guild-members-page.members-table-view.th-content--4">
						{i18n._(ROLES_DESCRIPTOR)}
						<button
							type="button"
							className={clsx(styles.filterButton, rolesActive && styles.filterButtonActive)}
							onClick={onRolesFilterOpen}
							aria-label={i18n._(FILTER_BY_ROLES_DESCRIPTOR)}
							aria-pressed={rolesActive}
							aria-haspopup="menu"
							data-flx="channel.guild-members-page.members-table-view.filter-button.roles-filter-open"
						>
							<FunnelIcon
								size={remFromPx(12)}
								weight={rolesActive ? 'fill' : 'bold'}
								data-flx="channel.guild-members-page.members-table-view.funnel-icon--4"
							/>
						</button>
					</div>
				</div>
				<div
					className={clsx(styles.headerCell, styles.actionsColumn)}
					role="columnheader"
					tabIndex={0}
					data-flx="channel.guild-members-page.members-table-view.header-cell--6"
				/>
			</div>
		</div>
	);
}
