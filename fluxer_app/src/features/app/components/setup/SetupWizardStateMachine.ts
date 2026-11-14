// SPDX-License-Identifier: AGPL-3.0-or-later

import {type AccountIdentityMode, AccountIdentityModes} from '@fluxer/constants/src/AccountIdentityConstants';

export type WizardStep =
	| 'welcome'
	| 'setup_token'
	| 'sign_in_method'
	| 'sign_in_locked'
	| 'shared_usernames'
	| 'email_delivery'
	| 'sign_in_to_finish'
	| 'account'
	| 'recovery_kit'
	| 'email_service'
	| 'email_sender'
	| 'name'
	| 'look'
	| 'communities'
	| 'direct_messages'
	| 'personal_notes'
	| 'registration'
	| 'push_relay'
	| 'gif_youtube'
	| 'attachments'
	| 'review'
	| 'done';

export type EmailDeliveryAnswer = 'now' | 'later';

export interface WizardConditions {
	isAuthenticated: boolean;
	accountIdentity: AccountIdentityMode;
	identityLocked: boolean;
	adminExists: boolean;
	setupTokenRequired: boolean;
	setupTokenRejected: boolean;
	emailsEnabled: boolean;
	emailAnswer: EmailDeliveryAnswer;
	finished: boolean;
}

interface StepDefinition {
	id: WizardStep;
	shown: (conditions: WizardConditions) => boolean;
	counted: boolean;
	beforeAccount: boolean;
}

const beforeAccount = (conditions: WizardConditions): boolean => !conditions.isAuthenticated && !conditions.finished;
const createsAccount = (conditions: WizardConditions): boolean => beforeAccount(conditions) && !conditions.adminExists;
const afterAccount = (conditions: WizardConditions): boolean => conditions.isAuthenticated && !conditions.finished;
const usesEmail = (conditions: WizardConditions): boolean => conditions.accountIdentity === AccountIdentityModes.EMAIL;
const connectsEmailNow = (conditions: WizardConditions): boolean =>
	afterAccount(conditions) && usesEmail(conditions) && !conditions.emailsEnabled && conditions.emailAnswer === 'now';

const STEP_DEFINITIONS: ReadonlyArray<StepDefinition> = [
	{id: 'welcome', shown: beforeAccount, counted: false, beforeAccount: true},
	{
		id: 'setup_token',
		shown: (c) => createsAccount(c) && (c.setupTokenRequired || c.setupTokenRejected),
		counted: true,
		beforeAccount: true,
	},
	{
		id: 'sign_in_method',
		shown: (c) => createsAccount(c) && !c.identityLocked,
		counted: true,
		beforeAccount: true,
	},
	{
		id: 'sign_in_locked',
		shown: (c) => createsAccount(c) && c.identityLocked,
		counted: true,
		beforeAccount: true,
	},
	{
		id: 'shared_usernames',
		shown: (c) => createsAccount(c) && !c.identityLocked && usesEmail(c),
		counted: true,
		beforeAccount: true,
	},
	{
		id: 'email_delivery',
		shown: (c) => createsAccount(c) && usesEmail(c) && !c.emailsEnabled,
		counted: true,
		beforeAccount: true,
	},
	{
		id: 'sign_in_to_finish',
		shown: (c) => beforeAccount(c) && c.adminExists,
		counted: false,
		beforeAccount: true,
	},
	{id: 'account', shown: createsAccount, counted: true, beforeAccount: true},
	{
		id: 'recovery_kit',
		shown: (c) => afterAccount(c) && c.accountIdentity === AccountIdentityModes.USERNAME,
		counted: true,
		beforeAccount: false,
	},
	{id: 'email_service', shown: connectsEmailNow, counted: true, beforeAccount: false},
	{id: 'email_sender', shown: connectsEmailNow, counted: true, beforeAccount: false},
	{id: 'name', shown: afterAccount, counted: true, beforeAccount: false},
	{id: 'look', shown: afterAccount, counted: true, beforeAccount: false},
	{id: 'communities', shown: afterAccount, counted: true, beforeAccount: false},
	{id: 'direct_messages', shown: afterAccount, counted: true, beforeAccount: false},
	{id: 'personal_notes', shown: afterAccount, counted: true, beforeAccount: false},
	{id: 'registration', shown: afterAccount, counted: true, beforeAccount: false},
	{id: 'push_relay', shown: afterAccount, counted: true, beforeAccount: false},
	{id: 'gif_youtube', shown: afterAccount, counted: true, beforeAccount: false},
	{id: 'attachments', shown: afterAccount, counted: true, beforeAccount: false},
	{id: 'review', shown: afterAccount, counted: true, beforeAccount: false},
	{id: 'done', shown: (c) => c.finished, counted: false, beforeAccount: false},
];

const STEP_ORDER: ReadonlyArray<WizardStep> = STEP_DEFINITIONS.map((definition) => definition.id);

export function isWizardStep(value: unknown): value is WizardStep {
	return typeof value === 'string' && (STEP_ORDER as ReadonlyArray<string>).includes(value);
}

function definitionOf(step: WizardStep): StepDefinition {
	return STEP_DEFINITIONS[STEP_ORDER.indexOf(step)];
}

export function isBeforeAccountStep(step: WizardStep): boolean {
	return definitionOf(step).beforeAccount;
}

export function visibleSteps(conditions: WizardConditions): ReadonlyArray<WizardStep> {
	return STEP_DEFINITIONS.filter((definition) => definition.shown(conditions)).map((definition) => definition.id);
}

export function resolveStep(step: WizardStep, conditions: WizardConditions): WizardStep {
	const steps = visibleSteps(conditions);
	if (steps.includes(step)) return step;
	const position = STEP_ORDER.indexOf(step);
	return steps.find((candidate) => STEP_ORDER.indexOf(candidate) > position) ?? steps[steps.length - 1] ?? 'welcome';
}

export function firstStepAfterAccount(conditions: WizardConditions): WizardStep {
	return visibleSteps(conditions).find((step) => !isBeforeAccountStep(step)) ?? 'review';
}

export function nextStep(step: WizardStep, conditions: WizardConditions): WizardStep {
	const steps = visibleSteps(conditions);
	const position = STEP_ORDER.indexOf(step);
	return steps.find((candidate) => STEP_ORDER.indexOf(candidate) > position) ?? step;
}

export function previousStep(step: WizardStep, conditions: WizardConditions): WizardStep | null {
	if (step === 'done') return null;
	const steps = visibleSteps(conditions);
	const position = STEP_ORDER.indexOf(step);
	const earlier = steps.filter((candidate) => STEP_ORDER.indexOf(candidate) < position);
	const previous = earlier[earlier.length - 1];
	if (previous === undefined) return null;
	if (isBeforeAccountStep(previous) !== isBeforeAccountStep(step)) return null;
	return previous;
}

export function countedStepsBeforeAccount(conditions: WizardConditions): number {
	return visibleSteps({...conditions, isAuthenticated: false, adminExists: false, finished: false}).filter(
		(step) => definitionOf(step).counted,
	).length;
}

export interface WizardProgress {
	current: number;
	total: number;
}

export function wizardProgress(
	step: WizardStep,
	conditions: WizardConditions,
	stepsBeforeAccount: number,
): WizardProgress | null {
	if (!definitionOf(step).counted) return null;
	const counted = visibleSteps(conditions).filter((candidate) => definitionOf(candidate).counted);
	const index = counted.indexOf(step);
	if (index < 0) return null;
	const offset = conditions.isAuthenticated ? stepsBeforeAccount : 0;
	const remainingAfterAccount = conditions.isAuthenticated
		? 0
		: visibleSteps({...conditions, isAuthenticated: true}).filter((candidate) => definitionOf(candidate).counted)
				.length;
	return {current: offset + index + 1, total: offset + counted.length + remainingAfterAccount};
}
