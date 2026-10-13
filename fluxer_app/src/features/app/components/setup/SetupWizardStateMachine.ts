// SPDX-License-Identifier: AGPL-3.0-or-later

import {type AccountIdentityMode, AccountIdentityModes} from '@fluxer/constants/src/AccountIdentityConstants';
import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

export type WizardStep =
	| 'welcome'
	| 'setup_token'
	| 'theme'
	| 'sign_in_method'
	| 'admin_intro'
	| 'admin_account'
	| 'loading'
	| 'admin_recovery_kit'
	| 'branding'
	| 'registration'
	| 'community'
	| 'media_expiry'
	| 'integration_gif'
	| 'integration_youtube'
	| 'integration_email'
	| 'integration_bluesky'
	| 'push_relay_consent'
	| 'services'
	| 'premium'
	| 'finish';

type WizardPhase = 'register' | 'loading' | 'configure';

const REGISTER_STEPS: ReadonlyArray<WizardStep> = [
	'welcome',
	'setup_token',
	'theme',
	'sign_in_method',
	'admin_intro',
	'admin_account',
];
const LOADING_STEPS: ReadonlyArray<WizardStep> = ['loading'];
const CONFIGURE_STEPS: ReadonlyArray<WizardStep> = [
	'welcome',
	'branding',
	'registration',
	'community',
	'media_expiry',
	'integration_gif',
	'integration_youtube',
	'integration_email',
	'integration_bluesky',
	'push_relay_consent',
	'services',
	'premium',
	'finish',
];

function derivePhase(isAuthenticated: boolean, hasConfig: boolean): WizardPhase {
	if (!isAuthenticated) return 'register';
	if (!hasConfig) return 'loading';
	return 'configure';
}

const USERNAME_CONFIGURE_STEPS: ReadonlyArray<WizardStep> = [
	'welcome',
	'admin_recovery_kit',
	...CONFIGURE_STEPS.slice(1).filter((step) => step !== 'integration_email'),
];

function phaseSteps(
	phase: WizardPhase,
	currentStep: WizardStep,
	accountIdentity: AccountIdentityMode,
	identityLocked: boolean,
	setupTokenRequired: boolean,
): ReadonlyArray<WizardStep> {
	switch (phase) {
		case 'register':
			return REGISTER_STEPS.filter(
				(step) =>
					step === currentStep ||
					((step !== 'sign_in_method' || !identityLocked) && (step !== 'setup_token' || setupTokenRequired)),
			);
		case 'loading':
			return LOADING_STEPS;
		case 'configure':
			return accountIdentity === AccountIdentityModes.USERNAME ? USERNAME_CONFIGURE_STEPS : CONFIGURE_STEPS;
	}
}

interface SetupWizardMachineContext {
	step: WizardStep;
	direction: number;
	isAuthenticated: boolean;
	hasConfig: boolean;
	accountIdentity: AccountIdentityMode;
	identityLocked: boolean;
	setupTokenRequired: boolean;
	setupTokenRejected: boolean;
}

export type SetupWizardMachineEvent =
	| {type: 'wizard.next'}
	| {type: 'wizard.back'}
	| {type: 'wizard.setupTokenRejected'}
	| {
			type: 'wizard.sync';
			isAuthenticated: boolean;
			hasConfig: boolean;
			accountIdentity: AccountIdentityMode;
			identityLocked: boolean;
			setupTokenRequired: boolean;
	  };

export interface SetupWizardModel {
	phase: WizardPhase;
	step: WizardStep;
	steps: ReadonlyArray<WizardStep>;
	direction: number;
	stepIndex: number;
}

function contextSteps(context: SetupWizardMachineContext): ReadonlyArray<WizardStep> {
	return phaseSteps(
		derivePhase(context.isAuthenticated, context.hasConfig),
		context.step,
		context.accountIdentity,
		context.identityLocked,
		context.setupTokenRequired || context.setupTokenRejected,
	);
}

function advance(context: SetupWizardMachineContext, delta: number): Partial<SetupWizardMachineContext> {
	const steps = contextSteps(context);
	const index = steps.indexOf(context.step);
	const safeIndex = index < 0 ? 0 : index;
	const nextIndex = Math.min(steps.length - 1, Math.max(0, safeIndex + delta));
	return {step: steps[nextIndex], direction: Math.sign(delta)};
}

function targetsPhase(event: SetupWizardMachineEvent, context: SetupWizardMachineContext, phase: WizardPhase): boolean {
	if (event.type !== 'wizard.sync') return false;
	return (
		derivePhase(event.isAuthenticated, event.hasConfig) === phase &&
		derivePhase(context.isAuthenticated, context.hasConfig) !== phase
	);
}

const setupWizardStateMachine = setup({
	types: {} as {
		context: SetupWizardMachineContext;
		events: SetupWizardMachineEvent;
	},
	guards: {
		shouldEnterRegister: ({context, event}) => targetsPhase(event, context, 'register'),
		shouldEnterLoading: ({context, event}) => targetsPhase(event, context, 'loading'),
		shouldEnterConfigure: ({context, event}) => targetsPhase(event, context, 'configure'),
	},
	actions: {
		applyConditions: assign({
			isAuthenticated: ({context, event}) =>
				event.type === 'wizard.sync' ? event.isAuthenticated : context.isAuthenticated,
			hasConfig: ({context, event}) => (event.type === 'wizard.sync' ? event.hasConfig : context.hasConfig),
			accountIdentity: ({context, event}) =>
				event.type === 'wizard.sync' ? event.accountIdentity : context.accountIdentity,
			identityLocked: ({context, event}) =>
				event.type === 'wizard.sync' ? event.identityLocked : context.identityLocked,
			setupTokenRequired: ({context, event}) =>
				event.type === 'wizard.sync' ? event.setupTokenRequired : context.setupTokenRequired,
		}),
		goNext: assign(({context}) => advance(context, 1)),
		goBack: assign(({context}) => advance(context, -1)),
		requireSetupToken: assign({setupTokenRejected: () => true, step: () => 'setup_token', direction: () => -1}),
		enterRegister: assign({step: () => REGISTER_STEPS[0], direction: () => 0}),
		enterLoading: assign({step: () => LOADING_STEPS[0], direction: () => 0}),
		enterConfigure: assign({step: () => CONFIGURE_STEPS[0], direction: () => 0}),
	},
}).createMachine({
	id: 'setupWizard',
	context: {
		step: 'welcome',
		direction: 0,
		isAuthenticated: false,
		hasConfig: false,
		accountIdentity: AccountIdentityModes.EMAIL,
		identityLocked: false,
		setupTokenRequired: false,
		setupTokenRejected: false,
	},
	initial: 'register',
	on: {
		'wizard.sync': [
			{guard: 'shouldEnterConfigure', target: '.configure', actions: 'applyConditions'},
			{guard: 'shouldEnterLoading', target: '.loading', actions: 'applyConditions'},
			{guard: 'shouldEnterRegister', target: '.register', actions: 'applyConditions'},
			{actions: 'applyConditions'},
		],
	},
	states: {
		register: {
			entry: 'enterRegister',
			on: {
				'wizard.next': {actions: 'goNext'},
				'wizard.back': {actions: 'goBack'},
				'wizard.setupTokenRejected': {actions: 'requireSetupToken'},
			},
		},
		loading: {
			entry: 'enterLoading',
		},
		configure: {
			entry: 'enterConfigure',
			on: {
				'wizard.next': {actions: 'goNext'},
				'wizard.back': {actions: 'goBack'},
			},
		},
	},
});

export type SetupWizardSnapshot = SnapshotFrom<typeof setupWizardStateMachine>;

export function createSetupWizardSnapshot(): SetupWizardSnapshot {
	return initialTransition(setupWizardStateMachine)[0];
}

export function transitionSetupWizardSnapshot(
	snapshot: SetupWizardSnapshot,
	event: SetupWizardMachineEvent,
): SetupWizardSnapshot {
	return transition(setupWizardStateMachine, snapshot, event)[0] as SetupWizardSnapshot;
}

export function selectSetupWizardModel(snapshot: SetupWizardSnapshot): SetupWizardModel {
	const phase = derivePhase(snapshot.context.isAuthenticated, snapshot.context.hasConfig);
	const steps = contextSteps(snapshot.context);
	const stepIndex = steps.indexOf(snapshot.context.step);
	return {
		phase,
		step: snapshot.context.step,
		steps,
		direction: snapshot.context.direction,
		stepIndex: stepIndex < 0 ? 0 : stepIndex,
	};
}
