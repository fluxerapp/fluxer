// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import DesktopRuntimeTransactions from '@app/features/app/state/DesktopRuntimeTransaction';
import {runtimeSnapshotFromDiscovery} from '@app/features/app/state/InstanceSnapshotStore';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {
	BOOTSTRAP_APP_PUBLIC,
	instanceDiscoveryFixture,
} from '@app/features/platform/state/__fixtures__/RuntimeBootstrapFixture';
import {getPremiumProductFullName, getPremiumProductName} from '@app/features/premium/utils/PremiumUtils';
import {parseInstanceDiscoveryDocument} from '@fluxer/instance_bootstrap/src/Discovery';
import type {MessageDescriptor} from '@lingui/core';
import {afterEach, describe, expect, test, vi} from 'vitest';

const desktopRuntime = vi.hoisted(() => ({required: false}));

vi.mock('@lingui/core/macro', () => ({msg: (descriptor: MessageDescriptor) => descriptor}));
vi.mock('@app/features/app/state/DesktopRuntimeTransaction', async (importOriginal) => {
	const actual = await importOriginal<typeof import('@app/features/app/state/DesktopRuntimeTransaction')>();
	return {...actual, requiresDesktopRuntimeTransaction: () => desktopRuntime.required};
});

afterEach(() => {
	desktopRuntime.required = false;
	vi.restoreAllMocks();
});

function applyBranding(branding: Partial<typeof BOOTSTRAP_APP_PUBLIC.branding>): void {
	const discovery = instanceDiscoveryFixture('https://chat.example.test/api');
	RuntimeConfig.applySnapshot(
		runtimeSnapshotFromDiscovery(
			parseInstanceDiscoveryDocument({
				...discovery,
				app_public: {...BOOTSTRAP_APP_PUBLIC, branding: {...BOOTSTRAP_APP_PUBLIC.branding, ...branding}},
			}),
		),
	);
}

describe('premium product name', () => {
	test('reads the active instance branding', () => {
		applyBranding({product_name: 'Example Chat', premium_product_name: 'Gold'});
		expect(getPremiumProductName()).toBe('Gold');
		expect(getPremiumProductFullName()).toBe('Example Chat Gold');
	});

	test('follows a switch to another instance', () => {
		applyBranding({product_name: 'Example Chat', premium_product_name: 'Gold'});
		applyBranding({product_name: 'Fluxer', premium_product_name: 'Plutonium'});
		expect(getPremiumProductName()).toBe('Plutonium');
		expect(getPremiumProductFullName()).toBe('Fluxer Plutonium');
	});

	test('falls back to the default names when no runtime is active', async () => {
		applyBranding({product_name: 'Example Chat', premium_product_name: 'Gold'});
		vi.spyOn(DesktopRuntimeTransactions, 'deactivate').mockResolvedValue(undefined);
		desktopRuntime.required = true;
		await RuntimeConfig.deactivate();
		expect(getPremiumProductName()).toBe('Plutonium');
		expect(getPremiumProductFullName()).toBe('Fluxer Plutonium');
	});

	test('falls back to the default premium name when the instance sends a blank one', () => {
		applyBranding({premium_product_name: '   '});
		expect(getPremiumProductName()).toBe('Plutonium');
	});
});
