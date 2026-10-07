// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeEach, describe, expect, test, vi} from 'vitest';

const mocks = vi.hoisted(() => ({
	currentAccountKey: 'active::1' as string | null,
	accounts: new Set<string>(['active::1', 'other::2']),
	transitions: [] as Array<string>,
	switches: [] as Array<{accountKey: string; redirectAfterSwitch: string | null}>,
	clickHandler: null as ((id: string, url?: string) => void) | null,
	shownIcons: [] as Array<string>,
	nextId: 'native-1',
}));

vi.mock('@lingui/core/macro', () => ({msg: (value: unknown) => value}));

vi.mock('@app/features/auth/state/Authentication', () => ({default: {currentUserId: null}}));

vi.mock('@app/features/navigation/utils/RouterUtils', () => ({
	transitionTo: (url: string) => mocks.transitions.push(url),
}));

vi.mock('@app/features/notification/utils/NotificationIconURL', () => ({getNotificationIconURL: () => ''}));

vi.mock('@app/features/streamer_mode/state/StreamerMode', () => ({default: {shouldDisableNotifications: false}}));

vi.mock('@app/features/ui/commands/SoundCommands', () => ({playSound: vi.fn()}));

vi.mock('@app/features/ui/state/Sound', () => ({default: {isSoundTypeEnabled: () => false}}));

vi.mock('@app/features/user/state/Users', () => ({default: {getUser: () => null}}));

vi.mock('@app/features/ui/utils/PwaUtils', () => ({
	isInstalledIOSPwa: () => false,
	isInstalledPwa: () => false,
	isMobileOrTablet: () => false,
}));

vi.mock('@app/features/ui/utils/NativeUtils', () => ({
	hasUnavailableElectronNativeContext: () => false,
	isDesktop: () => true,
	getElectronAPI: () => ({
		platform: 'darwin',
		onNotificationClick: (handler: (id: string, url?: string) => void) => {
			mocks.clickHandler = handler;
			return () => undefined;
		},
		showNotification: async (payload: {icon: string}) => {
			mocks.shownIcons.push(payload.icon);
			return {id: mocks.nextId};
		},
	}),
}));

vi.mock('@app/features/auth/state/Accounts', () => ({
	default: {
		get currentAccountKey() {
			return mocks.currentAccountKey;
		},
		getAccount: (accountKey: string) => (mocks.accounts.has(accountKey) ? {storageKey: accountKey} : null),
	},
}));

vi.mock('@app/features/auth/utils/AccountSwitcherModalUtils', () => ({
	switchStoredAccountFromSwitcher: async (request: {accountKey: string; redirectAfterSwitch: string | null}) => {
		mocks.switches.push({accountKey: request.accountKey, redirectAfterSwitch: request.redirectAfterSwitch});
	},
}));

const NotificationUtils = await import('@app/features/notification/utils/NotificationUtils');

async function settle(): Promise<void> {
	for (let index = 0; index < 5; index += 1) {
		await new Promise((resolve) => setTimeout(resolve, 0));
	}
}

beforeEach(() => {
	mocks.currentAccountKey = 'active::1';
	mocks.transitions.length = 0;
	mocks.switches.length = 0;
	mocks.shownIcons.length = 0;
});

describe('desktop notification click routing', () => {
	test('a click on a notification from another account switches to it and opens the channel', async () => {
		NotificationUtils.ensureDesktopNotificationClickHandler();
		mocks.nextId = 'native-other';
		await NotificationUtils.showNotification({title: 't', body: 'b', url: '/channels/9/8/7', accountKey: 'other::2'});

		mocks.clickHandler?.('native-other', '/channels/9/8/7');
		await settle();

		expect(mocks.switches).toEqual([{accountKey: 'other::2', redirectAfterSwitch: '/channels/9/8/7'}]);
		expect(mocks.transitions).toEqual([]);
	});

	test('a click after switching away routes back through the account that received it', async () => {
		NotificationUtils.ensureDesktopNotificationClickHandler();
		mocks.nextId = 'native-active';
		await NotificationUtils.showNotification({title: 't', body: 'b', url: '/channels/1/2/3', accountKey: 'active::1'});
		mocks.currentAccountKey = 'other::2';

		mocks.clickHandler?.('native-active', '/channels/1/2/3');
		await settle();

		expect(mocks.switches).toEqual([{accountKey: 'active::1', redirectAfterSwitch: '/channels/1/2/3'}]);
	});

	test('a click for the active account navigates in place', async () => {
		NotificationUtils.ensureDesktopNotificationClickHandler();
		mocks.nextId = 'native-same';
		await NotificationUtils.showNotification({title: 't', body: 'b', url: '/channels/1/2/3', accountKey: 'active::1'});

		mocks.clickHandler?.('native-same', '/channels/1/2/3');
		await settle();

		expect(mocks.switches).toEqual([]);
		expect(mocks.transitions).toEqual(['/channels/1/2/3']);
	});

	test('a click for an account that was removed does nothing', async () => {
		await NotificationUtils.openNotificationTarget('gone::3', '/channels/1/2/3');

		expect(mocks.switches).toEqual([]);
		expect(mocks.transitions).toEqual([]);
	});
});

describe('native notification icons', () => {
	test('a desktop local proxy URL is unwrapped to the remote media URL', async () => {
		const remote = 'https://media.example.test/avatars/1/abc.png?size=128';
		const wrapped = `fluxer-app://app/proxy/instance-key?url=${encodeURIComponent(remote)}`;

		expect(await NotificationUtils.resolveNativeNotificationIcon(wrapped)).toBe(remote);
	});

	test('a bundled asset is inlined as a data URL', async () => {
		const fetchMock = vi
			.spyOn(globalThis, 'fetch')
			.mockResolvedValue(new Response(new Blob([new Uint8Array([137, 80, 78, 71])], {type: 'image/png'})));
		try {
			const icon = await NotificationUtils.resolveNativeNotificationIcon('fluxer-app://app/assets/avatar.png');

			expect(icon.startsWith('data:image/png;base64,')).toBe(true);
		} finally {
			fetchMock.mockRestore();
		}
	});

	test('an icon that cannot be fetched is dropped', async () => {
		const fetchMock = vi.spyOn(globalThis, 'fetch').mockRejectedValue(new Error('offline'));
		try {
			expect(await NotificationUtils.resolveNativeNotificationIcon('fluxer-app://app/assets/avatar.png')).toBe('');
		} finally {
			fetchMock.mockRestore();
		}
	});
});
