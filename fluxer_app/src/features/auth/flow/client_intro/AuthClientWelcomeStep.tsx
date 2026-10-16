// SPDX-License-Identifier: AGPL-3.0-or-later

import {SetupWelcomeGate} from '@app/features/app/components/setup/SetupWelcomeGate';
import {PRODUCT_NAME} from '@app/features/app/config/ProductConstants';
import styles from '@app/features/auth/flow/client_intro/AuthClientPreferencesStep.module.css';
import {CONTINUE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button, ButtonVariant} from '@app/features/ui/button/Button';
import * as LocaleUtils from '@app/features/user/utils/LocaleUtils';
import {flxElementClassName} from '@app/lib/react';
import {useLingui} from '@lingui/react/macro';
import {ArrowRightIcon} from '@phosphor-icons/react';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';

interface AuthClientWelcomeStepProps {
	readonly onContinue: () => void;
}

export const AuthClientWelcomeStep = observer(function AuthClientWelcomeStep({onContinue}: AuthClientWelcomeStepProps) {
	const {i18n} = useLingui();
	return (
		<section
			className={clsx(styles.clientIntro, styles.clientWelcome)}
			data-flx="auth.flow.client-intro.auth-client-welcome-step.section"
		>
			<SetupWelcomeGate
				localeCode={LocaleUtils.getCurrentOrDetectedLocale()}
				productName={PRODUCT_NAME}
				isAuthenticated={false}
				data-flx="auth.flow.client-intro.auth-client-welcome-step.setup-welcome-gate"
			/>
			<flx-auth-client-welcome-step-footer
				className={flxElementClassName(styles.clientIntroFooter)}
				data-flx="auth.flow.client-intro.auth-client-welcome-step.client-intro-footer"
			>
				<Button
					type="button"
					variant={ButtonVariant.PRIMARY}
					onClick={onContinue}
					rightIcon={
						<ArrowRightIcon
							size={remFromPx(18)}
							weight="bold"
							data-flx="auth.flow.client-intro.auth-client-welcome-step.arrow-right-icon"
						/>
					}
					data-step-focus="true"
					data-flx="auth.flow.client-intro.auth-client-welcome-step.button.continue"
				>
					{i18n._(CONTINUE_DESCRIPTOR)}
				</Button>
			</flx-auth-client-welcome-step-footer>
		</section>
	);
});
