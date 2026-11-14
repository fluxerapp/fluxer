// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AccountEntryResult} from '@app/features/auth/commands/AccountEntryCommands';
import * as AccountEntryCommands from '@app/features/auth/commands/AccountEntryCommands';
import {
	InstantAccountForm,
	type InstantAccountFormErrors,
	instantAccountErrors,
	NO_INSTANT_ACCOUNT_ERRORS,
} from '@app/features/auth/flow/InstantAccountForm';
import type {AuthIdentifierFirstStepProps} from '@app/features/auth/flow/identifier_first/AuthIdentifierFirstStep';
import styles from '@app/features/auth/flow/identifier_first/AuthIdentifierFirstStep.module.css';
import {QuietButton} from '@app/features/auth/flow/QuietButton';
import type {RegistrationLegalConsentConfig} from '@app/features/auth/flow/RegistrationLegalNotice';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {makeAutoObservable, observableRef} from 'mobx';
import {observer} from 'mobx-react-lite';
import type {ReactNode} from 'react';

const USE_EMAIL_INSTEAD_DESCRIPTOR = msg({
	message: 'Use email instead',
	comment: 'Quiet text button that goes back from the sign-up step without an email to the email field.',
});

export class IdentifierNameDraft {
	lastSubmittedName = '';
	errors = NO_INSTANT_ACCOUNT_ERRORS;

	constructor() {
		makeAutoObservable(this, {errors: observableRef}, {autoBind: true});
	}

	setLastSubmittedName(value: string): void {
		this.lastSubmittedName = value;
	}

	setErrors(value: InstantAccountFormErrors): void {
		this.errors = value;
	}
}

interface AuthIdentifierNameStepProps extends Pick<AuthIdentifierFirstStepProps, 'runtimeSnapshot' | 'inviteCode'> {
	readonly draft: IdentifierNameDraft;
	readonly registrationUrlCode: string | null;
	readonly heading: ReactNode;
	readonly consentConfig: RegistrationLegalConsentConfig;
	readonly submitting: boolean;
	readonly setSubmitting: (value: boolean) => void;
	readonly finishEntry: (result: AccountEntryResult) => Promise<void>;
	readonly onBack: () => void;
}

export const AuthIdentifierNameStep = observer(function AuthIdentifierNameStep({
	runtimeSnapshot,
	inviteCode,
	draft,
	registrationUrlCode,
	heading,
	consentConfig,
	submitting,
	setSubmitting,
	finishEntry,
	onBack,
}: AuthIdentifierNameStepProps) {
	const {i18n} = useLingui();
	const {lastSubmittedName, errors, setLastSubmittedName, setErrors} = draft;
	const handleNameSubmit = async (globalName: string) => {
		if (submitting) return;
		setLastSubmittedName(globalName);
		setSubmitting(true);
		setErrors(NO_INSTANT_ACCOUNT_ERRORS);
		try {
			const result = await AccountEntryCommands.createInstantAccount({
				globalName,
				consent: true,
				inviteCode,
				registrationUrlCode,
				snapshot: runtimeSnapshot,
			});
			await finishEntry(result);
		} catch (caught) {
			setErrors(instantAccountErrors(caught, i18n));
		} finally {
			setSubmitting(false);
		}
	};

	return (
		<flx-auth-identifier-first className="flx-element" data-flx="auth.flow.identifier-first.name-step">
			{heading}
			<InstantAccountForm
				initialName={lastSubmittedName}
				consentConfig={consentConfig}
				submitting={submitting}
				errors={errors}
				onEdit={() => setErrors(NO_INSTANT_ACCOUNT_ERRORS)}
				onSubmit={(globalName) => void handleNameSubmit(globalName)}
				data-flx="auth.flow.identifier-first.name-form"
			/>
			<div className={styles.quietActions} data-flx="auth.flow.identifier-first.name-actions">
				<QuietButton
					onClick={onBack}
					data-flx="auth.flow.identifier-first.auth-identifier-first-step.quiet-button.go-to--3"
				>
					{i18n._(USE_EMAIL_INSTEAD_DESCRIPTOR)}
				</QuietButton>
			</div>
		</flx-auth-identifier-first>
	);
});
