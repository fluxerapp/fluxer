// SPDX-License-Identifier: AGPL-3.0-or-later

import {AccountDeleteModal} from '@app/features/auth/components/modals/AccountDeleteModal';
import {AccountDisableModal} from '@app/features/auth/components/modals/AccountDisableModal';
import {GuildOwnershipWarningModal} from '@app/features/guild/components/modals/GuildOwnershipWarningModal';
import Guilds from '@app/features/guild/state/Guilds';
import {DELETE_ACCOUNT_DESCRIPTOR, DISABLE_ACCOUNT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {
	AccountSecurityCard,
	AccountSecurityRow,
} from '@app/features/user/components/modals/tabs/account_security_tab/AccountSecurityCard';
import type {User} from '@app/features/user/models/User';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const DANGER_ZONE_ACTIONS_DESCRIPTOR = msg({
	message: 'Disable or delete your account',
	comment: 'Accessible name for the group of disable and delete account actions in account settings.',
});

interface DangerZoneTabProps {
	user: User;
	isClaimed: boolean;
}

export const DangerZoneTabContent: React.FC<DangerZoneTabProps> = observer(({user, isClaimed}) => {
	const {i18n} = useLingui();
	const handleDisableAccount = () => {
		ModalCommands.push(
			modal(() => (
				<AccountDisableModal data-flx="user.account-security-tab.danger-zone-tab.handle-disable-account.account-disable-modal" />
			)),
		);
	};
	const handleDeleteAccount = () => {
		const ownedGuilds = Guilds.getOwnedGuilds(user.id);
		if (ownedGuilds.length > 0) {
			ModalCommands.push(
				modal(() => (
					<GuildOwnershipWarningModal
						ownedGuilds={ownedGuilds}
						data-flx="user.account-security-tab.danger-zone-tab.handle-delete-account.guild-ownership-warning-modal"
					/>
				)),
			);
		} else {
			ModalCommands.push(
				modal(() => (
					<AccountDeleteModal data-flx="user.account-security-tab.danger-zone-tab.handle-delete-account.account-delete-modal" />
				)),
			);
		}
	};
	return (
		<AccountSecurityCard
			aria-label={i18n._(DANGER_ZONE_ACTIONS_DESCRIPTOR)}
			data-flx="user.account-security-tab.danger-zone-tab.card"
		>
			{isClaimed && (
				<AccountSecurityRow
					label={i18n._(DISABLE_ACCOUNT_DESCRIPTOR)}
					description={<Trans>Temporarily disable your account. You can reactivate it later by signing back in.</Trans>}
					data-flx="user.account-security-tab.danger-zone-tab.disable-row"
				>
					<Button
						variant="secondary"
						small={true}
						onClick={handleDisableAccount}
						data-flx="user.account-security-tab.danger-zone-tab.button.disable-account"
					>
						{i18n._(DISABLE_ACCOUNT_DESCRIPTOR)}
					</Button>
				</AccountSecurityRow>
			)}
			<AccountSecurityRow
				label={i18n._(DELETE_ACCOUNT_DESCRIPTOR)}
				description={
					<Trans>Permanently delete your account and all associated data. This action cannot be undone.</Trans>
				}
				data-flx="user.account-security-tab.danger-zone-tab.delete-row"
			>
				<Button
					variant="danger"
					small={true}
					onClick={handleDeleteAccount}
					data-flx="user.account-security-tab.danger-zone-tab.button.delete-account"
				>
					{i18n._(DELETE_ACCOUNT_DESCRIPTOR)}
				</Button>
			</AccountSecurityRow>
		</AccountSecurityCard>
	);
});
