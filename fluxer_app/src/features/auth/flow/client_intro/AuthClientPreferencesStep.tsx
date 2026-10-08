// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/auth/flow/client_intro/AuthClientPreferencesStep.module.css';
import {CONTINUE_DESCRIPTOR, THEME_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import Theme from '@app/features/theme/state/Theme';
import {Button, ButtonVariant} from '@app/features/ui/button/Button';
import {ThemeSelector} from '@app/features/user/components/modals/tabs/appearance_tab/theme/ThemeTabContent';
import {LanguageSelector} from '@app/features/user/components/modals/tabs/LanguageTab';
import * as LocaleUtils from '@app/features/user/utils/LocaleUtils';
import {flxElementClassName} from '@app/lib/react';
import type {ThemeType} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ArrowRightIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

const CLIENT_PREFERENCES_TITLE_DESCRIPTOR = msg({
	message: 'Choose your defaults',
	comment: 'Short title in the auth preferences step before login or registration details.',
});
const CLIENT_PREFERENCES_BODY_DESCRIPTOR = msg({
	message: 'Choose a language and theme. Your account preferences can replace these after you sign in.',
	comment: 'Body copy in the auth preferences step. Keep it brief and plain.',
});
const LANGUAGE_LABEL_DESCRIPTOR = msg({
	message: 'Language',
	comment: 'Label for the language selector in the auth preferences step.',
});

const CLIENT_LANGUAGE_MENU_MAX_HEIGHT = 220;

interface AuthClientPreferencesStepProps {
	readonly body?: React.ReactNode;
	readonly continueLabel?: React.ReactNode;
	readonly onContinue: () => void;
	readonly title?: React.ReactNode;
}

export const AuthClientPreferencesStep = observer(function AuthClientPreferencesStep({
	onContinue,
	title,
	body,
	continueLabel,
}: AuthClientPreferencesStepProps) {
	const {i18n} = useLingui();
	const currentLocale = LocaleUtils.getCurrentOrDetectedLocale();
	const theme = Theme.themePreference;
	const handleThemeChange = useCallback((nextTheme: ThemeType) => {
		Theme.setTheme(nextTheme);
	}, []);
	const themeLabel = i18n._(THEME_DESCRIPTOR);
	return (
		<section className={styles.clientIntro} data-flx="auth.flow.client-intro.auth-client-preferences-step.section">
			<flx-auth-client-preferences-step-copy
				className={flxElementClassName(styles.clientIntroCopy)}
				data-flx="auth.flow.client-intro.auth-client-preferences-step.client-intro-copy"
			>
				<h1
					className={styles.clientIntroTitle}
					data-flx="auth.flow.client-intro.auth-client-preferences-step.client-intro-title"
				>
					{title ?? i18n._(CLIENT_PREFERENCES_TITLE_DESCRIPTOR)}
				</h1>
				<p
					className={styles.clientIntroBody}
					data-flx="auth.flow.client-intro.auth-client-preferences-step.client-intro-body"
				>
					{body ?? i18n._(CLIENT_PREFERENCES_BODY_DESCRIPTOR)}
				</p>
			</flx-auth-client-preferences-step-copy>
			<flx-auth-client-preferences-step-fields
				className={flxElementClassName(styles.clientIntroFields)}
				data-flx="auth.flow.client-intro.auth-client-preferences-step.client-intro-fields"
			>
				<flx-auth-client-preferences-step-field
					className={flxElementClassName(styles.clientIntroField)}
					data-flx="auth.flow.client-intro.auth-client-preferences-step.client-intro-field"
				>
					<flx-auth-client-preferences-step-field-label
						className={flxElementClassName(styles.clientIntroFieldLabel)}
						data-flx="auth.flow.client-intro.auth-client-preferences-step.client-intro-field-label"
					>
						{i18n._(LANGUAGE_LABEL_DESCRIPTOR)}
					</flx-auth-client-preferences-step-field-label>
					<LanguageSelector
						value={currentLocale}
						onChange={LocaleUtils.setLocalLocale}
						openMenuOnFocus={false}
						maxMenuHeight={CLIENT_LANGUAGE_MENU_MAX_HEIGHT}
						menuPlacement="bottom"
						className={styles.clientIntroLanguageSelector}
						data-flx="auth.flow.client-intro.auth-client-preferences-step.client-intro-language-selector.set-local-locale"
					/>
				</flx-auth-client-preferences-step-field>
				<flx-auth-client-preferences-step-field
					className={flxElementClassName(styles.clientIntroField)}
					data-flx="auth.flow.client-intro.auth-client-preferences-step.client-intro-field--2"
				>
					<flx-auth-client-preferences-step-field-label
						className={flxElementClassName(styles.clientIntroFieldLabel)}
						data-flx="auth.flow.client-intro.auth-client-preferences-step.client-intro-field-label--2"
					>
						{themeLabel}
					</flx-auth-client-preferences-step-field-label>
					<ThemeSelector
						value={theme}
						onChange={handleThemeChange}
						ariaLabel={themeLabel}
						className={styles.clientIntroThemeSelector}
						data-flx="auth.flow.client-intro.auth-client-preferences-step.client-intro-theme-selector.theme-change"
					/>
				</flx-auth-client-preferences-step-field>
			</flx-auth-client-preferences-step-fields>
			<flx-auth-client-preferences-step-footer
				className={flxElementClassName(styles.clientIntroFooter)}
				data-flx="auth.flow.client-intro.auth-client-preferences-step.client-intro-footer"
			>
				<Button
					type="button"
					variant={ButtonVariant.PRIMARY}
					onClick={onContinue}
					rightIcon={
						<ArrowRightIcon
							size={remFromPx(18)}
							weight="bold"
							data-flx="auth.flow.client-intro.auth-client-preferences-step.arrow-right-icon"
						/>
					}
					data-step-focus="true"
					data-flx="auth.flow.client-intro.auth-client-preferences-step.button.continue"
				>
					{continueLabel ?? i18n._(CONTINUE_DESCRIPTOR)}
				</Button>
			</flx-auth-client-preferences-step-footer>
		</section>
	);
});
