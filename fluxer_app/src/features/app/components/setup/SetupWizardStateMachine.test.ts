// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createSetupWizardSnapshot,
	type SetupWizardSnapshot,
	selectSetupWizardModel,
	transitionSetupWizardSnapshot,
} from '@app/features/app/components/setup/SetupWizardStateMachine';
import {runtimeSnapshotFromDiscovery} from '@app/features/app/state/InstanceSnapshotStore';
import {instanceDiscoveryFixture} from '@app/features/platform/state/__fixtures__/RuntimeBootstrapFixture';
import {AccountIdentityModes} from '@fluxer/constants/src/AccountIdentityConstants';
import {parseInstanceDiscoveryDocument} from '@fluxer/instance_bootstrap/src/Discovery';
import {describe, expect, test} from 'vitest';

function registerSnapshot(setupTokenRequired: boolean): SetupWizardSnapshot {
	return transitionSetupWizardSnapshot(createSetupWizardSnapshot(), {
		type: 'wizard.sync',
		isAuthenticated: false,
		hasConfig: false,
		accountIdentity: AccountIdentityModes.USERNAME,
		identityLocked: false,
		setupTokenRequired,
	});
}

describe('setup wizard setup token step', () => {
	test('follows the welcome step when the discovery document requires the token', () => {
		const wire = instanceDiscoveryFixture('https://chat.example.test/api');
		wire.app_public = {
			...wire.app_public,
			setup: {configured: false, admin_url: null, account_identity_locked: false, setup_token_required: true},
		};
		const setup = runtimeSnapshotFromDiscovery(parseInstanceDiscoveryDocument(wire)).appPublic.setup;
		expect(setup.setup_token_required).toBe(true);
		expect(selectSetupWizardModel(registerSnapshot(setup.setup_token_required === true)).steps).toEqual([
			'welcome',
			'setup_token',
			'theme',
			'sign_in_method',
			'admin_intro',
			'admin_account',
		]);
	});

	test('opens after a rejection while the client believes no token is required', () => {
		let snapshot = registerSnapshot(false);
		snapshot = transitionSetupWizardSnapshot(snapshot, {type: 'wizard.next'});
		snapshot = transitionSetupWizardSnapshot(snapshot, {type: 'wizard.next'});
		expect(selectSetupWizardModel(snapshot).step).toBe('sign_in_method');
		snapshot = transitionSetupWizardSnapshot(snapshot, {type: 'wizard.setupTokenRejected'});
		snapshot = transitionSetupWizardSnapshot(snapshot, {
			type: 'wizard.sync',
			isAuthenticated: false,
			hasConfig: false,
			accountIdentity: AccountIdentityModes.USERNAME,
			identityLocked: false,
			setupTokenRequired: false,
		});
		const model = selectSetupWizardModel(snapshot);
		expect(model.step).toBe('setup_token');
		expect(model.steps[model.stepIndex + 1]).toBe('theme');
	});
});
