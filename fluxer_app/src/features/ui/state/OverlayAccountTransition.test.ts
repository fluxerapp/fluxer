// SPDX-License-Identifier: AGPL-3.0-or-later
// @vitest-environment happy-dom

import {instanceDiscoveryFixture} from '@app/features/platform/state/__fixtures__/RuntimeBootstrapFixture';
import type {I18n} from '@lingui/core';
import {afterEach, describe, expect, it, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => ({
	msg: (descriptor: unknown) => descriptor,
	t: (descriptor: unknown) => descriptor,
}));
vi.mock('@lingui/react/macro', () => ({Trans: () => null, useLingui: () => ({i18n: {_: () => ''}})}));
vi.mock('@app/features/ui/state/MobileLayout', () => ({default: {enabled: true, isMobileLayout: () => true}}));

const {AccountScopedWork, AccountScopedWorkTransitionReason} = await import(
	'@app/features/platform/state/AccountScopedWork'
);
const {default: RuntimeConfig} = await import('@app/features/app/state/RuntimeConfig');
const {runtimeSnapshotFromDiscovery} = await import('@app/features/app/state/InstanceSnapshotStore');
const {parseInstanceDiscoveryDocument} = await import('@fluxer/instance_bootstrap/src/Discovery');
const {default: QuickSwitcher} = await import('@app/features/search/state/QuickSwitcher');
const {default: ContextMenu} = await import('@app/features/ui/state/ContextMenu');
const {default: MediaViewer} = await import('@app/features/ui/state/MediaViewer');

RuntimeConfig.applySnapshot(
	runtimeSnapshotFromDiscovery(parseInstanceDiscoveryDocument(instanceDiscoveryFixture('https://one.example/api'))),
);

const CHANNEL_ID = '1555752678099779590';
const MESSAGE_ID = '1555923739894349824';

function switchAccounts(): Promise<void> {
	return AccountScopedWork.runSuspended(AccountScopedWorkTransitionReason.ACCOUNT_SWITCH, async () => {});
}

describe('overlays across an account transition', () => {
	afterEach(() => {
		MediaViewer.close();
		ContextMenu.close();
		QuickSwitcher.hide();
	});

	it('closes a media viewer opened on a message of the outgoing account', async () => {
		MediaViewer.open(
			[
				{
					src: 'https://one.example/a.png',
					originalSrc: 'https://one.example/a.png',
					naturalWidth: 1,
					naturalHeight: 1,
					type: 'image',
				},
			],
			0,
			CHANNEL_ID,
			MESSAGE_ID,
			undefined,
			null,
			true,
		);
		expect(MediaViewer.isOpen).toBe(true);

		await switchAccounts();

		expect(MediaViewer.isOpen).toBe(false);
		expect(MediaViewer.items).toHaveLength(0);
		expect(MediaViewer.channelId).toBeUndefined();
		expect(MediaViewer.messageId).toBeUndefined();
		expect(MediaViewer.allowAttachmentDelete).toBe(false);
	});

	it('closes a context menu opened under the outgoing account', async () => {
		const onClose = vi.fn();
		const target = document.createElement('button');
		document.body.appendChild(target);
		ContextMenu.open({id: 'message-menu', target: {x: 0, y: 0, target}, render: () => null, config: {onClose}});
		expect(ContextMenu.contextMenu?.id).toBe('message-menu');

		await switchAccounts();

		expect(ContextMenu.contextMenu).toBeNull();
		expect(onClose).toHaveBeenCalledTimes(1);
		target.remove();
	});

	it('closes the quick switcher so the incoming account can open it again', async () => {
		QuickSwitcher.setI18n({_: () => '', locale: 'en-US'} as unknown as I18n);
		QuickSwitcher.show();
		QuickSwitcher.search('typed under the outgoing account');
		expect(QuickSwitcher.query).toBe('typed under the outgoing account');
		expect(QuickSwitcher.isOpen).toBe(true);

		await switchAccounts();

		expect(QuickSwitcher.isOpen).toBe(false);
		expect(QuickSwitcher.query).toBe('');
		expect(QuickSwitcher.results).toHaveLength(0);
	});
});
