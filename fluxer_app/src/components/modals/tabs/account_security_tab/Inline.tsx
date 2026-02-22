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

import * as UserActionCreators from '@app/actions/UserActionCreators';
import {SettingsSection} from '@app/components/modals/shared/SettingsSection';
import {AccountTabContent as AccountTab} from '@app/components/modals/tabs/account_security_tab/AccountTab';
import {DangerZoneTabContent as DangerZoneTab} from '@app/components/modals/tabs/account_security_tab/DangerZoneTab';
import styles from '@app/components/modals/tabs/account_security_tab/Inline.module.css';
import {SecurityTabContent as SecurityTab} from '@app/components/modals/tabs/account_security_tab/SecurityTab';
import {Logger} from '@app/lib/Logger';
import UserStore from '@app/stores/UserStore';
import {PublicUserFlags, UserAuthenticatorTypes} from '@fluxer/constants/src/UserConstants';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useState} from 'react';

const logger = new Logger('AccountSecurityInlineTab');

export const AccountSecurityInlineTab = observer(() => {
	const {t} = useLingui();
	const user = UserStore.currentUser;
	const [showMaskedEmail, setShowMaskedEmail] = useState(false);
	const [showMaskedPhone, setShowMaskedPhone] = useState(false);
	const [passkeys, setPasskeys] = useState<Array<UserActionCreators.WebAuthnCredential>>([]);
	const [loadingPasskeys, setLoadingPasskeys] = useState(false);
	const [enablingSmsMfa, setEnablingSmsMfa] = useState(false);
	const [disablingSmsMfa, setDisablingSmsMfa] = useState(false);

	const loadPasskeys = useCallback(async () => {
		setLoadingPasskeys(true);
		try {
			const credentials = await UserActionCreators.listWebAuthnCredentials();
			setPasskeys(credentials);
		} catch (error) {
			logger.error('Failed to load passkeys', error);
		} finally {
			setLoadingPasskeys(false);
		}
	}, []);

	useEffect(() => {
		loadPasskeys();
	}, [loadPasskeys]);

	if (!user) return null;

	const hasSmsMfa = user.authenticatorTypes?.includes(UserAuthenticatorTypes.SMS) ?? false;
	const hasTotpMfa = user.authenticatorTypes?.includes(UserAuthenticatorTypes.TOTP) ?? false;
	const isSmsMfaDisabledForUser =
		(user.flags & PublicUserFlags.STAFF) !== 0 ||
		(user.flags & PublicUserFlags.CTP_MEMBER) !== 0 ||
		(user.flags & PublicUserFlags.PARTNER) !== 0;

	const isClaimed = user.isClaimed();

	return (
		<div className={styles.container}>
			<SettingsSection id="account" title={t`Account`}>
				<AccountTab
					user={user}
					isClaimed={isClaimed}
					showMaskedEmail={showMaskedEmail}
					setShowMaskedEmail={setShowMaskedEmail}
				/>
			</SettingsSection>
			<SettingsSection id="security" title={t`Security`}>
				<SecurityTab
					user={user}
					isClaimed={isClaimed}
					hasSmsMfa={hasSmsMfa}
					hasTotpMfa={hasTotpMfa}
					isSmsMfaDisabledForUser={isSmsMfaDisabledForUser}
					passkeys={passkeys}
					loadingPasskeys={loadingPasskeys}
					enablingSmsMfa={enablingSmsMfa}
					disablingSmsMfa={disablingSmsMfa}
					showMaskedPhone={showMaskedPhone}
					loadPasskeys={loadPasskeys}
					setEnablingSmsMfa={setEnablingSmsMfa}
					setDisablingSmsMfa={setDisablingSmsMfa}
					setShowMaskedPhone={setShowMaskedPhone}
				/>
			</SettingsSection>
			<SettingsSection id="danger_zone" title={t`Danger Zone`}>
				<DangerZoneTab user={user} isClaimed={isClaimed} />
			</SettingsSection>
		</div>
	);
});
