// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/setup/SelfHostedSetupWizardGate.module.css';
import {Note, StepHeader} from '@app/features/app/components/setup/SetupWizardSteps';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {AuthRegisterFormCore} from '@app/features/auth/flow/AuthRegisterFormCore';
import type {MissingField} from '@app/features/auth/flow/SubmitTooltip';
import type {AuthRegisterFormDraft} from '@app/features/auth/state/AuthRegisterDraftContext';
import type {ThemeType} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {makeAutoObservable, observableRef} from 'mobx';
import {observer} from 'mobx-react-lite';
import type {ReactNode, RefObject} from 'react';

const ACCOUNT_TITLE_DESCRIPTOR = msg({
	message: 'Create your account',
	comment: 'Setup wizard title for creating the first account, which becomes the administrator.',
});
const ACCOUNT_BODY_DESCRIPTOR = msg({
	message: 'This is your own account, and it can change every setting on this instance.',
	comment: 'Setup wizard body for creating the administrator account.',
});
const ACCOUNT_NO_EMAIL_DESCRIPTOR = msg({
	message: "Your instance can't send email yet, so keep this password safe.",
	comment: 'Extra line on the setup wizard account screen when the instance has no email service.',
});
export const CREATE_ACCOUNT_DESCRIPTOR = msg({
	message: 'Create account',
	comment: 'Button that creates the administrator account in the setup wizard.',
});

export interface SetupAccountValues {
	username: string;
	email: string;
}

export class SetupAccountDraft {
	values: SetupAccountValues;
	readonly passwordRef: RefObject<string | null> = {current: null};
	private readonly drafts = new Map<string, AuthRegisterFormDraft>();
	private readonly restoredDraft: SetupAccountValues;

	constructor(restoredDraft: Partial<Record<keyof SetupAccountValues, unknown>> | null) {
		this.values = {
			username: typeof restoredDraft?.username === 'string' ? restoredDraft.username : '',
			email: typeof restoredDraft?.email === 'string' ? restoredDraft.email : '',
		};
		this.restoredDraft = {...this.values};
		makeAutoObservable<this, 'drafts' | 'restoredDraft'>(
			this,
			{values: observableRef, passwordRef: false, drafts: false, restoredDraft: false},
			{autoBind: true},
		);
	}

	getRegisterFormDraft(draftKey: string): AuthRegisterFormDraft | undefined {
		const draft = this.drafts.get(draftKey);
		if (draft) return {...draft, formValues: {...draft.formValues}};
		if (!this.restoredDraft.username && !this.restoredDraft.email) return undefined;
		return {
			formValues: {...this.restoredDraft},
			selectedMonth: '',
			selectedDay: '',
			selectedYear: '',
		};
	}

	setRegisterFormDraft(draftKey: string, draft: AuthRegisterFormDraft): void {
		this.drafts.set(draftKey, {...draft, formValues: {...draft.formValues}});
		this.values = {username: draft.formValues.username ?? '', email: draft.formValues.email ?? ''};
	}

	clearRegisterFormDraft(draftKey: string): void {
		this.drafts.delete(draftKey);
	}

	resetSession(): void {
		this.drafts.clear();
		this.passwordRef.current = null;
	}
}

export const AccountStep = observer(
	({
		usernameSignIn,
		emailLater,
		theme,
		setupToken,
		formId,
		renderSubmit,
		onSetupTokenRejected,
		onPasswordRegistered,
	}: {
		usernameSignIn: boolean;
		emailLater: boolean;
		theme: ThemeType;
		setupToken: string | undefined;
		formId: string;
		renderSubmit: (state: {disabled: boolean; submitting: boolean; missingFields: Array<MissingField>}) => ReactNode;
		onSetupTokenRejected: (message: string) => void;
		onPasswordRegistered: (password: string) => void;
	}) => {
		const {i18n} = useLingui();
		return (
			<section className={styles.step} data-flx="app.self-hosted-setup-wizard-gate.account-step">
				<StepHeader
					title={i18n._(ACCOUNT_TITLE_DESCRIPTOR)}
					body={i18n._(ACCOUNT_BODY_DESCRIPTOR)}
					data-flx="app.setup.setup-wizard-steps.account-step.step-header"
				/>
				{emailLater && (
					<Note data-flx="app.setup.setup-wizard-steps.account-step.note">{i18n._(ACCOUNT_NO_EMAIL_DESCRIPTOR)}</Note>
				)}
				<div className={styles.accountForm} data-flx="app.self-hosted-setup-wizard-gate.account-form">
					<AuthRegisterFormCore
						fields={{
							showEmail: !usernameSignIn,
							showPassword: true,
							showPasswordConfirmation: true,
							showUsernameValidation: true,
							requireUsername: true,
						}}
						submitLabel={i18n._(CREATE_ACCOUNT_DESCRIPTOR)}
						redirectPath=""
						runtimeSnapshot={RuntimeConfig.getSnapshot()}
						offerRecoveryKit={false}
						hideDisplayName
						theme={theme}
						showLegalConsent={false}
						setupToken={setupToken}
						onSetupTokenRejected={onSetupTokenRejected}
						onPasswordRegistered={onPasswordRegistered}
						formId={formId}
						renderSubmit={renderSubmit}
						data-flx="app.self-hosted-setup-wizard-gate.account-register-form"
					/>
				</div>
			</section>
		);
	},
);
