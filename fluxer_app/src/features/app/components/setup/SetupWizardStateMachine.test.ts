// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createSetupWizardSnapshot,
	type SetupWizardMachineEvent,
	type SetupWizardSnapshot,
	selectSetupWizardModel,
	transitionSetupWizardSnapshot,
} from '@app/features/app/components/setup/SetupWizardStateMachine';
import {describe, expect, it} from 'vitest';

function transition(snapshot: SetupWizardSnapshot, event: SetupWizardMachineEvent): SetupWizardSnapshot {
	return transitionSetupWizardSnapshot(snapshot, event);
}

function sync(snapshot: SetupWizardSnapshot, isAuthenticated: boolean, hasConfig: boolean): SetupWizardSnapshot {
	return transition(snapshot, {type: 'wizard.sync', isAuthenticated, hasConfig});
}

describe('setupWizardStateMachine', () => {
	it('starts unauthenticated in the register phase on the welcome step', () => {
		const model = selectSetupWizardModel(createSetupWizardSnapshot());

		expect(model).toMatchObject({phase: 'register', step: 'welcome', direction: 0, stepIndex: 0});
	});

	it('advances and retreats within the register phase by step identity', () => {
		let snapshot = createSetupWizardSnapshot();
		snapshot = transition(snapshot, {type: 'wizard.next'});
		expect(selectSetupWizardModel(snapshot)).toMatchObject({step: 'theme', direction: 1});

		snapshot = transition(snapshot, {type: 'wizard.next'});
		expect(selectSetupWizardModel(snapshot).step).toBe('admin_intro');

		snapshot = transition(snapshot, {type: 'wizard.back'});
		expect(selectSetupWizardModel(snapshot)).toMatchObject({step: 'theme', direction: -1});
	});

	it('does not advance past the last register step', () => {
		let snapshot = createSetupWizardSnapshot();
		for (let index = 0; index < 8; index += 1) {
			snapshot = transition(snapshot, {type: 'wizard.next'});
		}

		expect(selectSetupWizardModel(snapshot).step).toBe('admin_account');
	});

	it('does not retreat before the first step', () => {
		const snapshot = transition(createSetupWizardSnapshot(), {type: 'wizard.back'});

		expect(selectSetupWizardModel(snapshot).step).toBe('welcome');
	});

	it('routes to the loading phase once authenticated before the config loads', () => {
		let snapshot = createSetupWizardSnapshot();
		snapshot = transition(snapshot, {type: 'wizard.next'});
		snapshot = transition(snapshot, {type: 'wizard.next'});
		snapshot = transition(snapshot, {type: 'wizard.next'});
		expect(selectSetupWizardModel(snapshot).step).toBe('admin_account');

		snapshot = sync(snapshot, true, false);

		expect(selectSetupWizardModel(snapshot)).toMatchObject({phase: 'loading', step: 'loading', direction: 0});
	});

	it('lands on branding then registration after registering, never skipping them', () => {
		let snapshot = createSetupWizardSnapshot();
		snapshot = transition(snapshot, {type: 'wizard.next'});
		snapshot = transition(snapshot, {type: 'wizard.next'});
		snapshot = transition(snapshot, {type: 'wizard.next'});
		expect(selectSetupWizardModel(snapshot).step).toBe('admin_account');

		snapshot = sync(snapshot, true, false);
		snapshot = sync(snapshot, true, true);

		expect(selectSetupWizardModel(snapshot)).toMatchObject({phase: 'configure', step: 'welcome', direction: 0});

		snapshot = transition(snapshot, {type: 'wizard.next'});
		expect(selectSetupWizardModel(snapshot).step).toBe('branding');

		snapshot = transition(snapshot, {type: 'wizard.next'});
		expect(selectSetupWizardModel(snapshot).step).toBe('registration');

		snapshot = transition(snapshot, {type: 'wizard.next'});
		expect(selectSetupWizardModel(snapshot).step).toBe('community');
	});

	it('enters the configure phase at welcome when already authenticated with config', () => {
		const snapshot = sync(createSetupWizardSnapshot(), true, true);

		expect(selectSetupWizardModel(snapshot)).toMatchObject({phase: 'configure', step: 'welcome'});
	});

	it('preserves the current step when a redundant sync keeps the same phase', () => {
		let snapshot = sync(createSetupWizardSnapshot(), true, true);
		snapshot = transition(snapshot, {type: 'wizard.next'});
		snapshot = transition(snapshot, {type: 'wizard.next'});
		expect(selectSetupWizardModel(snapshot).step).toBe('registration');

		snapshot = sync(snapshot, true, true);

		expect(selectSetupWizardModel(snapshot).step).toBe('registration');
	});

	it('does not advance past the final configure step', () => {
		let snapshot = sync(createSetupWizardSnapshot(), true, true);
		for (let index = 0; index < 20; index += 1) {
			snapshot = transition(snapshot, {type: 'wizard.next'});
		}

		expect(selectSetupWizardModel(snapshot).step).toBe('finish');
	});

	it('falls back to the register phase when authentication is lost', () => {
		let snapshot = sync(createSetupWizardSnapshot(), true, true);
		snapshot = transition(snapshot, {type: 'wizard.next'});
		expect(selectSetupWizardModel(snapshot).step).toBe('branding');

		snapshot = sync(snapshot, false, false);

		expect(selectSetupWizardModel(snapshot)).toMatchObject({phase: 'register', step: 'welcome'});
	});
});
