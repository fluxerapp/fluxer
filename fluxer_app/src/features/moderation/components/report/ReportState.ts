// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type Action,
	type FlowStep,
	type FormValues,
	INITIAL_FORM_VALUES,
	type State,
} from '@app/features/moderation/components/report/ReportTypes';
import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

type ReportMachineContext = Omit<State, 'flowStep'>;

function createInitialContext(): ReportMachineContext {
	return {
		selectedType: null,
		email: '',
		verificationCode: '',
		ticket: null,
		formValues: {...INITIAL_FORM_VALUES},
		isSendingCode: false,
		isVerifying: false,
		isSubmitting: false,
		errorMessage: null,
		successReportId: null,
		resendCooldownSeconds: 0,
		fieldErrors: {},
	};
}

export function createInitialState(): State {
	return {
		...createInitialContext(),
		flowStep: 'selection',
	};
}

export const reportStateMachine = setup({
	types: {} as {
		context: ReportMachineContext;
		events: Action;
	},
	actions: {
		resetContext: assign(() => createInitialContext()),
		selectType: assign(({event}) => {
			if (event.type !== 'SELECT_TYPE') return {};
			return {
				...createInitialContext(),
				selectedType: event.reportType,
			};
		}),
		goToEmail: assign(() => ({
			verificationCode: '',
			ticket: null,
			isVerifying: false,
			errorMessage: null,
			resendCooldownSeconds: 0,
			fieldErrors: {},
		})),
		goToVerification: assign(() => ({
			verificationCode: '',
			ticket: null,
			errorMessage: null,
			resendCooldownSeconds: 0,
			fieldErrors: {},
		})),
		goToDetails: assign(() => ({
			errorMessage: null,
			fieldErrors: {},
		})),
		setError: assign(({event}) => ({
			errorMessage: event.type === 'SET_ERROR' ? event.message : null,
		})),
		setEmail: assign(({event}) => ({
			email: event.type === 'SET_EMAIL' ? event.email : '',
			errorMessage: null,
		})),
		setVerificationCode: assign(({event}) => ({
			verificationCode: event.type === 'SET_VERIFICATION_CODE' ? event.code : '',
			errorMessage: null,
		})),
		setTicket: assign(({event}) => ({
			ticket: event.type === 'SET_TICKET' ? event.ticket : null,
		})),
		setFormField: assign(({context, event}) => {
			if (event.type !== 'SET_FORM_FIELD') return {};
			return {
				formValues: {...context.formValues, [event.field]: event.value},
				errorMessage: null,
				fieldErrors: {...context.fieldErrors, [event.field]: undefined},
			};
		}),
		setSendingCode: assign(({event}) => ({
			isSendingCode: event.type === 'SENDING_CODE' ? event.value : false,
		})),
		setVerifying: assign(({event}) => ({
			isVerifying: event.type === 'VERIFYING' ? event.value : false,
		})),
		setSubmitting: assign(({event}) => ({
			isSubmitting: event.type === 'SUBMITTING' ? event.value : false,
		})),
		submitSuccess: assign(({event}) => ({
			successReportId: event.type === 'SUBMIT_SUCCESS' ? event.reportId : null,
			isSubmitting: false,
			errorMessage: null,
			fieldErrors: {},
		})),
		startResendCooldown: assign(({event}) => ({
			resendCooldownSeconds: event.type === 'START_RESEND_COOLDOWN' ? event.seconds : 0,
		})),
		tickResendCooldown: assign(({context}) => ({
			resendCooldownSeconds: Math.max(0, context.resendCooldownSeconds - 1),
		})),
		setFieldErrors: assign(({event}) => ({
			fieldErrors: event.type === 'SET_FIELD_ERRORS' ? event.errors : {},
		})),
		clearFieldErrors: assign(() => ({
			fieldErrors: {},
		})),
		clearFieldError: assign(({context, event}) => {
			if (event.type !== 'CLEAR_FIELD_ERROR') return {};
			const fieldErrors = {...context.fieldErrors};
			delete fieldErrors[event.field];
			return {fieldErrors};
		}),
	},
}).createMachine({
	id: 'reportFlow',
	context: createInitialContext(),
	initial: 'selection',
	on: {
		RESET_ALL: {target: '.selection', actions: 'resetContext'},
		SELECT_TYPE: {target: '.email', actions: 'selectType'},
		GO_TO_SELECTION: {target: '.selection', actions: 'resetContext'},
		GO_TO_EMAIL: {target: '.email', actions: 'goToEmail'},
		GO_TO_VERIFICATION: {target: '.verification', actions: 'goToVerification'},
		GO_TO_DETAILS: {target: '.details', actions: 'goToDetails'},
		SET_ERROR: {actions: 'setError'},
		SET_EMAIL: {actions: 'setEmail'},
		SET_VERIFICATION_CODE: {actions: 'setVerificationCode'},
		SET_TICKET: {actions: 'setTicket'},
		SET_FORM_FIELD: {actions: 'setFormField'},
		SENDING_CODE: {actions: 'setSendingCode'},
		VERIFYING: {actions: 'setVerifying'},
		SUBMITTING: {actions: 'setSubmitting'},
		SUBMIT_SUCCESS: {target: '.complete', actions: 'submitSuccess'},
		START_RESEND_COOLDOWN: {actions: 'startResendCooldown'},
		TICK_RESEND_COOLDOWN: {actions: 'tickResendCooldown'},
		SET_FIELD_ERRORS: {actions: 'setFieldErrors'},
		CLEAR_FIELD_ERRORS: {actions: 'clearFieldErrors'},
		CLEAR_FIELD_ERROR: {actions: 'clearFieldError'},
	},
	states: {
		selection: {},
		email: {},
		verification: {},
		details: {},
		complete: {},
	},
});

export type ReportMachineSnapshot = SnapshotFrom<typeof reportStateMachine>;

export function createReportSnapshot(): ReportMachineSnapshot {
	return initialTransition(reportStateMachine)[0];
}

export function transitionReportSnapshot(snapshot: ReportMachineSnapshot, event: Action): ReportMachineSnapshot {
	return transition(reportStateMachine, snapshot, event)[0] as ReportMachineSnapshot;
}

export function getReportFlowStep(snapshot: ReportMachineSnapshot): FlowStep {
	switch (snapshot.value) {
		case 'email':
		case 'verification':
		case 'details':
		case 'complete':
			return snapshot.value;
		default:
			return 'selection';
	}
}

export function selectReportState(snapshot: ReportMachineSnapshot): State {
	const context = snapshot.context;
	return {
		selectedType: context.selectedType,
		flowStep: getReportFlowStep(snapshot),
		email: context.email,
		verificationCode: context.verificationCode,
		ticket: context.ticket,
		formValues: context.formValues as FormValues,
		isSendingCode: context.isSendingCode,
		isVerifying: context.isVerifying,
		isSubmitting: context.isSubmitting,
		errorMessage: context.errorMessage,
		successReportId: context.successReportId,
		resendCooldownSeconds: context.resendCooldownSeconds,
		fieldErrors: context.fieldErrors,
	};
}
