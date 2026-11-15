// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/setup/SelfHostedSetupWizardGate.module.css';
import {SetupWelcomeRoll} from '@app/features/app/components/setup/SetupWelcomeRoll';
import {useSetupWelcomeRotation} from '@app/features/app/components/setup/useSetupWelcomeRotation';
import {flxElementClassName} from '@app/lib/react';
import {observer} from 'mobx-react-lite';

export const SetupWelcomeGate = observer(function SetupWelcomeGate({localeCode}: {localeCode: string}) {
	const {entry, upcoming} = useSetupWelcomeRotation(localeCode);
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
		</flx-app-setup-welcome-gate>
	);
});
