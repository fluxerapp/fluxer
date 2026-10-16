// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/setup/SelfHostedSetupWizardGate.module.css';
import {SetupWelcomeRoll} from '@app/features/app/components/setup/SetupWelcomeRoll';
import {useSetupWelcomeRotation} from '@app/features/app/components/setup/useSetupWelcomeRotation';
import {flxElementClassName} from '@app/lib/react';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type {ReactNode} from 'react';

const WELCOME_AUTHED_TITLE_DESCRIPTOR = msg({
	message: 'Welcome to {productName}',
	comment: 'Setup wizard title shown once the operator is signed in as the instance admin.',
});
const WELCOME_AUTHED_BODY_DESCRIPTOR = msg({
	message:
		'You are signed in as the instance administrator. The next steps configure branding, registration, and core policy for everyone on this instance.',
	comment: 'Setup wizard body shown to the signed-in instance administrator.',
});

interface SetupWelcomeGateProps {
	localeCode: string;
	productName: string;
	isAuthenticated: boolean;
}

export const SetupWelcomeGate = observer(function SetupWelcomeGate({
	localeCode,
	productName,
	isAuthenticated,
}: SetupWelcomeGateProps) {
	const {i18n} = useLingui();
	const {entry, upcoming} = useSetupWelcomeRotation(localeCode);
	const renderAdministratorIntroduction = (): ReactNode => {
		if (!isAuthenticated) {
			return null;
		}
		return (
			<>
				<h3 className={styles.welcomeTitle} data-flx="app.self-hosted-setup-wizard-gate.welcome-title">
					{i18n._(WELCOME_AUTHED_TITLE_DESCRIPTOR, {productName})}
				</h3>
				<p className={styles.body} data-flx="app.self-hosted-setup-wizard-gate.welcome-body">
					{i18n._(WELCOME_AUTHED_BODY_DESCRIPTOR)}
				</p>
			</>
		);
	};
	return (
		<flx-app-setup-welcome-gate
			className={flxElementClassName(styles.welcomeHero)}
			data-flx="app.self-hosted-setup-wizard-gate.welcome-hero"
		>
			<SetupWelcomeRoll
				entry={entry}
				upcoming={upcoming}
				frameClassName={styles.welcomeWordFrame}
				measureClassName={styles.welcomeWordMeasure}
				wordClassName={styles.welcomeWord}
				data-flx="app.setup.setup-welcome-gate.setup-welcome-roll"
			/>
			{renderAdministratorIntroduction()}
		</flx-app-setup-welcome-gate>
	);
});
