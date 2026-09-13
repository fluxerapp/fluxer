// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createAuthorizeSnapshot,
	getAuthorizeStateValue,
	selectAuthorizePhase,
	transitionAuthorizeSnapshot,
} from '@app/features/auth/components/pages/oauth_authorize_page/state/authorizeMachine';
import {describe, expect, it} from 'vitest';

describe('authorizeMachine', () => {
	it('moves from loading into review and through bot invite success', () => {
		let snapshot = createAuthorizeSnapshot();
		expect(getAuthorizeStateValue(snapshot)).toBe('loading');
		expect(selectAuthorizePhase(snapshot)).toEqual({kind: 'loading'});

		snapshot = transitionAuthorizeSnapshot(snapshot, {type: 'INIT_OK', step: 'scopes'});
		expect(selectAuthorizePhase(snapshot)).toEqual({kind: 'review', step: 'scopes'});

		snapshot = transitionAuthorizeSnapshot(snapshot, {type: 'SET_REVIEW_STEP', step: 'permissions'});
		expect(selectAuthorizePhase(snapshot)).toEqual({kind: 'review', step: 'permissions'});

		snapshot = transitionAuthorizeSnapshot(snapshot, {
			type: 'SUBMIT_BOT_INVITE_DONE',
			destinationName: 'Engineering',
		});
		expect(selectAuthorizePhase(snapshot)).toEqual({kind: 'success', destinationName: 'Engineering'});
	});

	it('ignores review-only events outside the review state', () => {
		let snapshot = createAuthorizeSnapshot();
		snapshot = transitionAuthorizeSnapshot(snapshot, {type: 'SET_REVIEW_STEP', step: 'community'});
		expect(selectAuthorizePhase(snapshot)).toEqual({kind: 'loading'});

		snapshot = transitionAuthorizeSnapshot(snapshot, {
			type: 'SUBMIT_BOT_INVITE_DONE',
			destinationName: 'Engineering',
		});
		expect(selectAuthorizePhase(snapshot)).toEqual({kind: 'loading'});
	});

	it('allows invalid request and session-expired terminal states from any phase', () => {
		let snapshot = createAuthorizeSnapshot();
		snapshot = transitionAuthorizeSnapshot(snapshot, {type: 'INIT_OK'});
		snapshot = transitionAuthorizeSnapshot(snapshot, {type: 'INIT_INVALID', message: 'Bad redirect_uri'});
		expect(selectAuthorizePhase(snapshot)).toEqual({kind: 'invalid_request', message: 'Bad redirect_uri'});

		snapshot = transitionAuthorizeSnapshot(snapshot, {type: 'INIT_SESSION_EXPIRED'});
		expect(selectAuthorizePhase(snapshot)).toEqual({kind: 'session_expired'});
	});
});
