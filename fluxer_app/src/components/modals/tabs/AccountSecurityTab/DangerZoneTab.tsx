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

import {Trans} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {AccountDeleteModal} from '~/components/modals/AccountDeleteModal';
import {AccountDisableModal} from '~/components/modals/AccountDisableModal';
import {GuildOwnershipWarningModal} from '~/components/modals/GuildOwnershipWarningModal';
import {SettingsTabSection} from '~/components/modals/shared/SettingsTabLayout';
import {Button} from '~/components/uikit/Button/Button';
import type {UserRecord} from '~/records/UserRecord';
import GuildStore from '~/stores/GuildStore';
import styles from './AccountTab.module.css';

interface DangerZoneTabProps {
	user: UserRecord;
	isClaimed: boolean;
}

export const DangerZoneTabContent: React.FC<DangerZoneTabProps> = observer(({user, isClaimed}) => {
	const handleDisableAccount = () => {
		const ownedGuilds = GuildStore.getOwnedGuilds(user.id);
		if (ownedGuilds.length > 0) {
			ModalActionCreators.push(modal(() => <GuildOwnershipWarningModal ownedGuilds={ownedGuilds} action="disable" />));
		} else {
			ModalActionCreators.push(modal(() => <AccountDisableModal />));
		}
	};

	const handleDeleteAccount = () => {
		const ownedGuilds = GuildStore.getOwnedGuilds(user.id);
		if (ownedGuilds.length > 0) {
			ModalActionCreators.push(modal(() => <GuildOwnershipWarningModal ownedGuilds={ownedGuilds} action="delete" />));
		} else {
			ModalActionCreators.push(modal(() => <AccountDeleteModal />));
		}
	};

	return (
		<>
			{isClaimed && (
				<SettingsTabSection
					title={<Trans>Disable Account</Trans>}
					description={<Trans>Temporarily disable your account. You can reactivate it later.</Trans>}
				>
					<div className={styles.row}>
						<div className={styles.rowContent}>
							<div className={styles.label}>
								<Trans>Disable Account</Trans>
							</div>
							<div className={styles.description}>
								<Trans>Temporarily disable your account. You can reactivate it later by signing back in.</Trans>
							</div>
						</div>
						<Button variant="danger-secondary" small={true} onClick={handleDisableAccount}>
							<Trans>Disable Account</Trans>
						</Button>
					</div>
				</SettingsTabSection>
			)}

			<SettingsTabSection
				title={<Trans>Delete Account</Trans>}
				description={<Trans>Permanently delete your account and all associated data. This cannot be undone.</Trans>}
			>
				<div className={styles.row}>
					<div className={styles.rowContent}>
						<div className={styles.label}>
							<Trans>Delete Account</Trans>
						</div>
						<div className={styles.description}>
							<Trans>Permanently delete your account and all associated data. This action cannot be undone.</Trans>
						</div>
					</div>
					<Button variant="danger-primary" small={true} onClick={handleDeleteAccount}>
						<Trans>Delete Account</Trans>
					</Button>
				</div>
			</SettingsTabSection>
		</>
	);
});
