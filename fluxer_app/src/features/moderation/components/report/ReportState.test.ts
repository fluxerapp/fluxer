// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createReportSnapshot,
	selectReportState,
	transitionReportSnapshot,
} from '@app/features/moderation/components/report/ReportState';
import {describe, expect, it} from 'vitest';

describe('ReportState machine', () => {
	it('moves through the public report flow while preserving context', () => {
		let snapshot = createReportSnapshot();
		expect(selectReportState(snapshot).flowStep).toBe('selection');

		snapshot = transitionReportSnapshot(snapshot, {type: 'SELECT_TYPE', reportType: 'message'});
		let state = selectReportState(snapshot);
		expect(state.flowStep).toBe('email');
		expect(state.selectedType).toBe('message');

		snapshot = transitionReportSnapshot(snapshot, {type: 'SET_EMAIL', email: 'reporter@example.com'});
		snapshot = transitionReportSnapshot(snapshot, {type: 'GO_TO_VERIFICATION'});
		state = selectReportState(snapshot);
		expect(state.flowStep).toBe('verification');
		expect(state.email).toBe('reporter@example.com');

		snapshot = transitionReportSnapshot(snapshot, {type: 'SET_TICKET', ticket: 'ticket-1'});
		snapshot = transitionReportSnapshot(snapshot, {type: 'GO_TO_DETAILS'});
		state = selectReportState(snapshot);
		expect(state.flowStep).toBe('details');
		expect(state.ticket).toBe('ticket-1');
	});

	it('clears step-local verification state when returning to email', () => {
		let snapshot = createReportSnapshot();
		snapshot = transitionReportSnapshot(snapshot, {type: 'SELECT_TYPE', reportType: 'user'});
		snapshot = transitionReportSnapshot(snapshot, {type: 'SET_VERIFICATION_CODE', code: 'ABCD-1234'});
		snapshot = transitionReportSnapshot(snapshot, {type: 'SET_TICKET', ticket: 'ticket-1'});
		snapshot = transitionReportSnapshot(snapshot, {type: 'VERIFYING', value: true});
		snapshot = transitionReportSnapshot(snapshot, {type: 'START_RESEND_COOLDOWN', seconds: 30});
		snapshot = transitionReportSnapshot(snapshot, {type: 'GO_TO_EMAIL'});

		const state = selectReportState(snapshot);
		expect(state.flowStep).toBe('email');
		expect(state.verificationCode).toBe('');
		expect(state.ticket).toBeNull();
		expect(state.isVerifying).toBe(false);
		expect(state.resendCooldownSeconds).toBe(0);
	});

	it('updates form fields and clears individual field errors', () => {
		let snapshot = createReportSnapshot();
		snapshot = transitionReportSnapshot(snapshot, {
			type: 'SET_FIELD_ERRORS',
			errors: {messageLink: 'Message link is invalid'},
		});
		snapshot = transitionReportSnapshot(snapshot, {
			type: 'SET_FORM_FIELD',
			field: 'messageLink',
			value: 'https://fluxer.app/channels/1/2/3',
		});
		let state = selectReportState(snapshot);
		expect(state.formValues.messageLink).toBe('https://fluxer.app/channels/1/2/3');
		expect(state.fieldErrors.messageLink).toBeUndefined();

		snapshot = transitionReportSnapshot(snapshot, {
			type: 'SET_FIELD_ERRORS',
			errors: {messageLink: 'Message link is invalid', category: 'Category required'},
		});
		snapshot = transitionReportSnapshot(snapshot, {type: 'CLEAR_FIELD_ERROR', field: 'messageLink'});
		state = selectReportState(snapshot);
		expect(state.fieldErrors.messageLink).toBeUndefined();
		expect(state.fieldErrors.category).toBe('Category required');
	});

	it('enters complete on submit success and reset returns to a fresh selection state', () => {
		let snapshot = createReportSnapshot();
		snapshot = transitionReportSnapshot(snapshot, {type: 'SELECT_TYPE', reportType: 'guild'});
		snapshot = transitionReportSnapshot(snapshot, {type: 'SUBMITTING', value: true});
		snapshot = transitionReportSnapshot(snapshot, {type: 'SUBMIT_SUCCESS', reportId: 'report-1'});
		let state = selectReportState(snapshot);
		expect(state.flowStep).toBe('complete');
		expect(state.successReportId).toBe('report-1');
		expect(state.isSubmitting).toBe(false);

		snapshot = transitionReportSnapshot(snapshot, {type: 'RESET_ALL'});
		state = selectReportState(snapshot);
		expect(state.flowStep).toBe('selection');
		expect(state.selectedType).toBeNull();
		expect(state.successReportId).toBeNull();
	});
});
