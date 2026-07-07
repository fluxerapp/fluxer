// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import * as OAuth2AuthorizationCommands from '@app/features/auth/commands/OAuth2AuthorizationCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {AuthorizedAppsManagementModal} from '@app/features/user/components/modals/tabs/account_security_tab/AccountManagementModals';
import {AccountTabContent} from '@app/features/user/components/modals/tabs/account_security_tab/AccountTab';
import {DangerZoneTabContent} from '@app/features/user/components/modals/tabs/account_security_tab/DangerZoneTab';
import {SecurityTabContent} from '@app/features/user/components/modals/tabs/account_security_tab/SecurityTab';
import {BlockedUsersContent} from '@app/features/user/components/modals/tabs/BlockedUsersTab';
import {LinkedDevicesManagementModal} from '@app/features/user/components/modals/tabs/DevicesTab';
import type {AccountSettingsManagementSectionId} from '@app/features/user/components/settings_utils/SettingsNavigationGroups';
import type {User} from '@app/features/user/models/User';
import type {WebAuthnCredential} from '@app/features/user/state/WebAuthnCredentials';
import {getAccountSecurityCapabilities} from '@app/features/user/utils/AccountSecurityCapabilities';
import * as FormUtils from '@app/lib/forms';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useRef, useState} from 'react';
import styles from './SecurityTab.module.css';

const SIGN_IN_DETAILS_DESCRIPTOR = msg({
	message: 'Sign-in details',
	comment: 'Section title for email address and password controls in the account security tab. Keep it concise.',
});
const SECURITY_DESCRIPTOR = msg({
	message: 'Security',
	comment: 'Short label in the account security tab. Keep it concise. Keep the tone plain and specific.',
});
const BLOCKED_USERS_DESCRIPTOR = msg({
	message: 'Blocked users',
	comment: 'Short label in the account security tab. Keep it concise.',
});
const COULD_NOT_LOAD_AUTHORIZED_APPS_DESCRIPTOR = msg({
	message: "Couldn't load authorized apps",
	comment: 'Title of the error modal shown when opening the authorized apps management modal fails.',
});
const DANGER_ZONE_DESCRIPTOR = msg({
	message: 'Danger zone',
	comment: 'Short label in the account security tab. Keep it concise. Keep the tone plain and specific.',
});

interface AccountSecuritySectionsProps {
	user: User;
	isClaimed: boolean;
	passkeys: ReadonlyArray<WebAuthnCredential>;
	showMaskedEmail: boolean;
	setShowMaskedEmail: (show: boolean) => void;
	targetSection?: AccountSettingsManagementSectionId | null;
}

export const AccountSecuritySections: React.FC<AccountSecuritySectionsProps> = observer(
	({user, isClaimed, passkeys, showMaskedEmail, setShowMaskedEmail, targetSection}) => {
		const {i18n} = useLingui();
		const capabilities = getAccountSecurityCapabilities(user);
		const showSignInDetails = isClaimed && (capabilities.canManageLocalEmail || capabilities.canManageLocalPassword);
		const [authorizedAppsSubmitting, setAuthorizedAppsSubmitting] = useState(false);
		const authorizedAppsSubmittingRef = useRef(false);
		const openAuthorizedAppsModal = useCallback(() => {
			if (authorizedAppsSubmittingRef.current) return;
			authorizedAppsSubmittingRef.current = true;
			setAuthorizedAppsSubmitting(true);
			void (async () => {
				try {
					const authorizations = await OAuth2AuthorizationCommands.listAuthorizations();
					ModalCommands.push(
						modal(() => (
							<AuthorizedAppsManagementModal
								authorizations={authorizations}
								data-flx="user.account-security-sections.authorized-apps-modal"
							/>
						)),
					);
				} catch (error) {
					FormUtils.pushApiErrorModal(i18n, error, i18n._(COULD_NOT_LOAD_AUTHORIZED_APPS_DESCRIPTOR));
				} finally {
					authorizedAppsSubmittingRef.current = false;
					setAuthorizedAppsSubmitting(false);
				}
			})();
		}, [i18n]);
		const openLinkedDevicesModal = () => {
			ModalCommands.push(
				modal(() => <LinkedDevicesManagementModal data-flx="user.account-security-sections.linked-devices-modal" />),
			);
		};
		return (
			<>
				{isClaimed && (
					<SettingsSection
						id="account"
						title={i18n._(SIGN_IN_DETAILS_DESCRIPTOR)}
						data-flx="user.account-security-sections.account"
					>
						{showSignInDetails ? (
							<AccountTabContent
								user={user}
								isClaimed={isClaimed}
								showMaskedEmail={showMaskedEmail}
								setShowMaskedEmail={setShowMaskedEmail}
								data-flx="user.account-security-sections.account-tab-content"
							/>
						) : (
							<div className={styles.notice} data-flx="user.account-security-sections.notice">
								<p className={styles.noticeText} data-flx="user.account-security-sections.notice-text">
									<Trans>
										This account is managed via Single Sign-On (SSO). Sign-in details and two-factor authentication are
										managed by your provider.
									</Trans>
								</p>
							</div>
						)}
					</SettingsSection>
				)}
				{isClaimed && (
					<SettingsSection
						id="security"
						title={i18n._(SECURITY_DESCRIPTOR)}
						defaultExpanded={targetSection === 'security'}
						data-flx="user.account-security-sections.security"
					>
						<SecurityTabContent
							user={user}
							capabilities={capabilities}
							isClaimed={isClaimed}
							passkeys={passkeys}
							authorizedAppsSubmitting={authorizedAppsSubmitting}
							onManageAuthorizedApps={openAuthorizedAppsModal}
							onManageLinkedDevices={openLinkedDevicesModal}
							data-flx="user.account-security-sections.security-tab-content"
						/>
					</SettingsSection>
				)}
				<SettingsSection
					id="danger_zone"
					title={i18n._(DANGER_ZONE_DESCRIPTOR)}
					data-flx="user.account-security-sections.danger-zone"
				>
					<DangerZoneTabContent
						user={user}
						isClaimed={isClaimed}
						data-flx="user.account-security-sections.danger-zone-tab-content"
					/>
				</SettingsSection>
				{isClaimed && (
					<SettingsSection
						id="blocked_users"
						title={i18n._(BLOCKED_USERS_DESCRIPTOR)}
						defaultExpanded={targetSection === 'blocked_users'}
						data-flx="user.account-security-sections.blocked-users"
					>
						<BlockedUsersContent data-flx="user.account-security-sections.blocked-users-content" />
					</SettingsSection>
				)}
			</>
		);
	},
);
