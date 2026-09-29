// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RequiredActionFlow} from '@app/features/auth/components/modals/RequiredActionFlow';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';

export interface PhoneFormInputs {
	phoneNumber: string;
}

export interface CodeFormInputs {
	code: string;
}

export interface NewEmailFormInputs {
	newEmail: string;
}

export type VerificationType = 'email' | 'phone';

export interface VerificationTab {
	id: VerificationType;
	label: string;
}

export interface ActiveInboundChallenge {
	code: string;
	ourNumber: string;
}

export type EmailScreen =
	| {kind: 'email-instructions'}
	| {kind: 'email-recovery-new'}
	| {kind: 'email-recovery-code'; recipient: string; ticket: string; proof: string}
	| {kind: 'bounced-email-new'}
	| {kind: 'bounced-email-code'; recipient: string; ticket: string};
export type PhoneScreen =
	| {kind: 'phone-number'}
	| {kind: 'phone-code'; recipient: string}
	| {kind: 'phone-inbound-start'}
	| {kind: 'phone-inbound-challenge'; code: string; ourNumber: string};

export const EMAIL_SCREEN_ORDER: ReadonlyArray<EmailScreen['kind']> = [
	'email-instructions',
	'email-recovery-new',
	'email-recovery-code',
	'bounced-email-new',
	'bounced-email-code',
];
export const PHONE_SCREEN_ORDER: ReadonlyArray<PhoneScreen['kind']> = [
	'phone-inbound-start',
	'phone-inbound-challenge',
	'phone-number',
	'phone-code',
];

export type SubmitCallback = () => Promise<void>;

export const normalizeVerificationCode = (code: string): string => code.split(' ').join('');
export const buildMockRequiredActionFlow = (): RequiredActionFlow => {
	const mode = DeveloperOptions.mockRequiredActionsMode;
	const reverify = DeveloperOptions.mockRequiredActionsReverify;
	const channelPlan = {
		actions: [],
		reverify,
		clearsAll: true,
		remainingActionsAfterCompletion: [],
		requiresInboundPhone: false,
	} as const;
	return {
		actions: [],
		key: `mock:${mode}:${reverify}`,
		mode,
		defaultTab: mode === 'phone' ? 'phone' : 'email',
		email: mode === 'phone' ? null : {channel: 'email', ...channelPlan},
		phone: mode === 'email' ? null : {channel: 'phone', ...channelPlan},
		reverify,
		requiresInboundPhone: false,
	};
};
