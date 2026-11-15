// SPDX-License-Identifier: AGPL-3.0-or-later

import type {EmailDeliveryAnswer} from '@app/features/app/components/setup/SetupWizardStateMachine';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {
	type AccountIdentityMode,
	AccountIdentityModes,
	type TagStyle,
	TagStyles,
} from '@fluxer/constants/src/AccountIdentityConstants';
import {makeAutoObservable} from 'mobx';

export interface SetupIdentityValues {
	signInMethod: AccountIdentityMode;
	tagStyle: TagStyle;
	emailAnswer: EmailDeliveryAnswer;
}

function isIdentityMode(value: unknown): value is AccountIdentityMode {
	return value === AccountIdentityModes.EMAIL || value === AccountIdentityModes.USERNAME;
}

function isTagStyle(value: unknown): value is TagStyle {
	return value === TagStyles.NONE || value === TagStyles.RANDOM;
}

export function isEmailAnswer(value: unknown): value is EmailDeliveryAnswer {
	return value === 'now' || value === 'later';
}

export class SetupIdentityDraft implements SetupIdentityValues {
	signInMethod: AccountIdentityMode;
	tagStyle: TagStyle;
	emailAnswer: EmailDeliveryAnswer;

	constructor(restoredDraft: Partial<Record<keyof SetupIdentityValues, unknown>> | null) {
		this.signInMethod = isIdentityMode(restoredDraft?.signInMethod)
			? restoredDraft.signInMethod
			: RuntimeConfig.accountIdentity;
		this.tagStyle = isTagStyle(restoredDraft?.tagStyle) ? restoredDraft.tagStyle : TagStyles.NONE;
		this.emailAnswer = isEmailAnswer(restoredDraft?.emailAnswer) ? restoredDraft.emailAnswer : 'now';
		makeAutoObservable(this, {}, {autoBind: true});
	}

	setSignInMethod(mode: AccountIdentityMode): void {
		this.signInMethod = mode;
	}

	setTagStyle(style: TagStyle): void {
		this.tagStyle = style;
	}

	setEmailAnswer(answer: EmailDeliveryAnswer): void {
		this.emailAnswer = answer;
	}
}
