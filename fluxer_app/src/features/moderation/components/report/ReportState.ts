// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type Action,
	type FlowStep,
	type FormValues,
	INITIAL_FORM_VALUES,
	type State,
} from '@app/features/moderation/components/report/ReportTypes';
import {
	backReportFlowWalk,
	startReportFlowWalk,
	walkToReportFlowOption,
} from '@app/features/moderation/components/report_flow/ReportFlowWalk';
import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

type ReportMachineContext = Omit<State, 'flowStep'>;

function createInitialContext(): ReportMachineContext {
	return {
		selectedType: null,
		email: '',
		verificationCode: '',
		ticket: null,
		formValues: {...INITIAL_FORM_VALUES},
		prefillOption: null,
		goodFaithConfirmed: false,
		flow: null,
		flowStatus: 'loading',
		walk: null,
		answersRejected: false,
		isSendingCode: false,
		isVerifying: false,
		isSubmitting: false,
		errorMessage: null,
		errorIsRateLimit: false,
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
		returnToSelection: assign(({context}) => ({
			...createInitialContext(),
			email: context.email,
			verificationCode: context.verificationCode,
			ticket: context.ticket,
		})),
		selectType: assign(({context, event}) => {
			if (event.type !== 'SELECT_TYPE') return {};
			return {
				...createInitialContext(),
				email: context.email,
				verificationCode: context.verificationCode,
				ticket: context.ticket,
				selectedType: event.reportType,
			};
		}),
		prefill: assign(({event}) => {
			if (event.type !== 'PREFILL') return {};
			return {
				...createInitialContext(),
				selectedType: event.reportType,
				formValues: {...INITIAL_FORM_VALUES, ...event.values},
				prefillOption: event.option,
			};
		}),
		goToEmail: assign(({context}) => ({
			verificationCode: context.ticket ? context.verificationCode : '',
			isVerifying: false,
			errorMessage: null,
			fieldErrors: {},
		})),
		goToVerification: assign(({context}) => ({
			verificationCode: context.ticket ? context.verificationCode : '',
			errorMessage: null,
			fieldErrors: {},
		})),
		goToReason: assign(({context}) => ({
			walk: context.walk?.phase === 'summary' ? backReportFlowWalk(context.walk) : context.walk,
			errorMessage: null,
			answersRejected: false,
			fieldErrors: {},
		})),
		goToDetails: assign(() => ({
			errorMessage: null,
			answersRejected: false,
			fieldErrors: {},
		})),
		requestFlow: assign(() => ({
			flowStatus: 'loading' as const,
		})),
		loadFlow: assign(({context, event}) => {
			if (event.type !== 'FLOW_LOADED' || event.flow.target_type !== context.selectedType) return {};
			if (event.flow === context.flow) return {flowStatus: 'loaded' as const};
			const prefilledWalk =
				context.prefillOption !== null ? walkToReportFlowOption(event.flow, context.prefillOption) : null;
			return {
				flow: event.flow,
				walk: prefilledWalk ?? startReportFlowWalk(event.flow),
				prefillOption: null,
				flowStatus: 'loaded' as const,
			};
		}),
		markFlowUnavailable: assign(({context, event}) => {
			if (event.type !== 'FLOW_UNAVAILABLE' || event.reportType !== context.selectedType) return {};
			return {flowStatus: 'unavailable' as const};
		}),
		setWalk: assign(({event}) => (event.type === 'WALK_CHANGED' ? {walk: event.walk} : {})),
		rejectAnswers: assign(({event}) => ({
			errorMessage: event.type === 'ANSWERS_REJECTED' ? event.message : null,
			errorIsRateLimit: false,
			answersRejected: true,
		})),
		setError: assign(({event}) => ({
			errorMessage: event.type === 'SET_ERROR' ? event.message : null,
			errorIsRateLimit: event.type === 'SET_ERROR' && event.rateLimit === true,
			answersRejected: false,
		})),
		setEmail: assign(({context, event}) => {
			const email = event.type === 'SET_EMAIL' ? event.email : '';
			if (email.trim() === context.email.trim()) return {email, errorMessage: null};
			return {email, ticket: null, verificationCode: '', errorMessage: null};
		}),
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
		setGoodFaithConfirmed: assign(({context, event}) => ({
			goodFaithConfirmed: event.type === 'SET_GOOD_FAITH_CONFIRMED' ? event.value : false,
			errorMessage: null,
			fieldErrors: {...context.fieldErrors, goodFaithConfirmed: undefined},
		})),
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
		startResendCooldown: assign(({context, event}) => {
			const resendCooldownSeconds = event.type === 'START_RESEND_COOLDOWN' ? event.seconds : 0;
			if (resendCooldownSeconds > 0 || !context.errorIsRateLimit) return {resendCooldownSeconds};
			return {resendCooldownSeconds, errorMessage: null, errorIsRateLimit: false};
		}),
		tickResendCooldown: assign(({context}) => {
			const resendCooldownSeconds = Math.max(0, context.resendCooldownSeconds - 1);
			if (resendCooldownSeconds > 0 || !context.errorIsRateLimit) return {resendCooldownSeconds};
			return {resendCooldownSeconds, errorMessage: null, errorIsRateLimit: false};
		}),
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
	guards: {
		walkReachedSummary: ({event}) => event.type === 'WALK_CHANGED' && event.walk.phase === 'summary',
	},
}).createMachine({
	id: 'reportFlow',
	context: createInitialContext(),
	initial: 'selection',
	on: {
		RESET_ALL: {target: '.selection', actions: 'resetContext'},
		SELECT_TYPE: {target: '.email', actions: 'selectType'},
		PREFILL: {target: '.email', actions: 'prefill'},
		GO_TO_SELECTION: {target: '.selection', actions: 'returnToSelection'},
		GO_TO_EMAIL: {target: '.email', actions: 'goToEmail'},
		GO_TO_VERIFICATION: {target: '.verification', actions: 'goToVerification'},
		GO_TO_REASON: {target: '.reason', actions: 'goToReason'},
		GO_TO_DETAILS: {target: '.details', actions: 'goToDetails'},
		FLOW_REQUESTED: {actions: 'requestFlow'},
		FLOW_LOADED: {actions: 'loadFlow'},
		FLOW_UNAVAILABLE: {actions: 'markFlowUnavailable'},
		WALK_CHANGED: [
			{guard: 'walkReachedSummary', target: '.details', actions: ['setWalk', 'goToDetails']},
			{actions: 'setWalk'},
		],
		ANSWERS_REJECTED: {actions: 'rejectAnswers'},
		SET_GOOD_FAITH_CONFIRMED: {actions: 'setGoodFaithConfirmed'},
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
		reason: {},
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
		case 'reason':
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
		prefillOption: context.prefillOption,
		goodFaithConfirmed: context.goodFaithConfirmed,
		flow: context.flow,
		flowStatus: context.flowStatus,
		walk: context.walk,
		answersRejected: context.answersRejected,
		isSendingCode: context.isSendingCode,
		isVerifying: context.isVerifying,
		isSubmitting: context.isSubmitting,
		errorMessage: context.errorMessage,
		errorIsRateLimit: context.errorIsRateLimit,
		successReportId: context.successReportId,
		resendCooldownSeconds: context.resendCooldownSeconds,
		fieldErrors: context.fieldErrors,
	};
}
