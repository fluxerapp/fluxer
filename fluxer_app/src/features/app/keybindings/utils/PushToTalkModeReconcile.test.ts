// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	GlobalShortcutsApi,
	GlobalShortcutsBackend,
	GlobalShortcutsPortalState,
	GlobalShortcutsPortalStatus,
	GlobalShortcutsStatus,
} from '@app/types/electron.d';
import {afterAll, beforeAll, describe, expect, it, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => ({
	msg: (descriptor: unknown) => descriptor,
	t: (descriptor: unknown) => descriptor,
}));

vi.mock('@lingui/react/macro', () => ({
	Trans: () => null,
	useLingui: () => ({i18n: {_: (descriptor: {message?: string}) => descriptor.message ?? ''}}),
}));

const BOOTSTRAP_ENDPOINT = 'https://primary.test/api';

(globalThis.window as unknown as Record<string, unknown>).__FLUXER_BOOTSTRAP__ = {
	config: {
		releaseChannel: 'stable',
		bootstrapApiEndpoint: BOOTSTRAP_ENDPOINT,
		bootstrapApiPublicEndpoint: BOOTSTRAP_ENDPOINT,
	},
	instance: {
		api_code_version: Number.MAX_SAFE_INTEGER,
		endpoints: {
			api: BOOTSTRAP_ENDPOINT,
			api_client: BOOTSTRAP_ENDPOINT,
			api_public: BOOTSTRAP_ENDPOINT,
			gateway: 'wss://gateway.primary.test',
			media: 'https://media.primary.test',
			static_cdn: 'https://cdn.primary.test',
			marketing: 'https://primary.test',
			admin: 'https://admin.primary.test',
			invite: 'https://primary.test/invite',
			gift: 'https://primary.test/gift',
			webapp: 'https://app.primary.test',
			upload_relay: 'https://upload.primary.test',
		},
		captcha: {provider: 'none'},
		features: {
			voice_enabled: false,
			stripe_enabled: false,
			self_hosted: false,
			presigned_attachment_uploads: false,
			emails_enabled: false,
			phone_verification_enabled: false,
		},
		gif: {provider: 'klipy', display_name: 'Klipy', attribution_required: false},
		sso: {enabled: false, enforced: false, display_name: null, redirect_uri: ''},
		registration: {mode: 'open', admin_registration_urls_enabled: true},
		community: {single_community: false, single_community_guild_id: null, direct_messages_disabled: false},
		services: {gif_enabled: true, youtube_enabled: false, bluesky_enabled: false},
		limits: undefined,
		push: {public_vapid_key: null},
		app_public: {
			branding: {
				product_name: 'Fluxer',
				icon_url: null,
				symbol_url: null,
				logo_url: null,
				wordmark_url: null,
				favicon_url: null,
				theme_color: null,
			},
			setup: {configured: true, admin_url: null},
			legal: {terms_url: null, privacy_url: null},
			registration: {collect_date_of_birth: true},
		},
	},
};

let statusListener: ((status: GlobalShortcutsStatus) => void) | null = null;

const globalShortcutsApi: Partial<GlobalShortcutsApi> = {
	sync: async () => {},
	getStatus: () => new Promise<GlobalShortcutsStatus>(() => {}),
	onStatus: (callback) => {
		statusListener = callback;
		return () => {
			statusListener = null;
		};
	},
};

vi.mock('@app/features/ui/utils/NativeUtils', async (importOriginal) => ({
	...(await importOriginal<typeof import('@app/features/ui/utils/NativeUtils')>()),
	getElectronAPI: () => ({globalShortcuts: globalShortcutsApi}),
}));

const PORTAL_ASSIGNED_ACTIONS_KEY = 'GlobalShortcuts:portalAssignedActions:v1';

interface PortalStatusOptions {
	recovering?: boolean;
	trigger?: string | null;
	error?: string | null;
	backend?: GlobalShortcutsBackend;
	directInputEnabled?: boolean;
}

function portalStatus(state: GlobalShortcutsPortalState, options: PortalStatusOptions = {}): GlobalShortcutsStatus {
	const trigger = options.trigger === undefined ? 'F13' : options.trigger;
	const portal: GlobalShortcutsPortalStatus = {
		state,
		version: 2,
		canConfigure: true,
		canRecheck: state === 'unsupported' || state === 'error',
		portalAppId: 'app.fluxer.FluxerDesktop',
		shortcuts: [
			{action: 'voice_push_to_talk', triggerDescription: state === 'bound' ? trigger : null},
			{action: 'voice_push_to_mute', triggerDescription: null},
		],
		error: options.error ?? (state === 'error' ? 'portal-unavailable' : null),
		recovering: options.recovering ?? false,
	};
	return {
		backend: options.backend ?? (state === 'unsupported' ? 'none' : 'portal'),
		platform: 'linux',
		linux: {
			session: 'wayland',
			sandbox: 'none',
			desktop: 'gnome',
			portal,
			directInput: {available: true, enabled: options.directInputEnabled ?? false, locked: false},
		},
		hooksActive: false,
		hookError: null,
		supportsMouseButtons: false,
		supportsModifierOnly: false,
	};
}

type Modules = {
	GlobalShortcuts: typeof import('@app/features/input/state/GlobalShortcuts').default;
	Keybind: typeof import('@app/features/input/state/InputKeybind').default;
	AppStorage: typeof import('@app/features/platform/state/PersistentStorage').default;
	reactToPushToTalkModeChanges: typeof import('@app/features/app/keybindings/utils/PushToTalkModeReconcile').reactToPushToTalkModeChanges;
};

let modules: Modules;

function emit(status: GlobalShortcutsStatus): void {
	expect(statusListener).not.toBeNull();
	statusListener?.(status);
}

describe('push-to-talk mode reconcile', () => {
	beforeAll(async () => {
		const {default: AppStorage} = await import('@app/features/platform/state/PersistentStorage');
		AppStorage.setJSON(PORTAL_ASSIGNED_ACTIONS_KEY, ['voice_push_to_talk']);
		const {default: GlobalShortcuts} = await import('@app/features/input/state/GlobalShortcuts');
		const {default: Keybind} = await import('@app/features/input/state/InputKeybind');
		const {reactToPushToTalkModeChanges} = await import('@app/features/app/keybindings/utils/PushToTalkModeReconcile');
		modules = {GlobalShortcuts, Keybind, AppStorage, reactToPushToTalkModeChanges};
		Keybind.setTransmitMode('voice_push_to_talk');
	}, 180_000);

	afterAll(async () => {
		modules.GlobalShortcuts.detach();
		const {default: Idle} = await import('@app/features/ui/state/Idle');
		Idle.destroy();
	});

	it('restores the last settled portal assignment at startup before any status arrives', () => {
		const {GlobalShortcuts, Keybind} = modules;
		expect(Keybind.isPushToTalkEffective()).toBe(false);
		GlobalShortcuts.attach();
		expect(Keybind.isPushToTalkEffective()).toBe(true);
		emit(portalStatus('unknown'));
		emit(portalStatus('probing'));
		expect(Keybind.isPushToTalkEffective()).toBe(true);
	});

	it('keeps push-to-talk effective through transient and error portal states', () => {
		const {Keybind, AppStorage, reactToPushToTalkModeChanges} = modules;
		const reconcile = vi.fn();
		const dispose = reactToPushToTalkModeChanges(reconcile);
		emit(portalStatus('bound'));
		expect(AppStorage.getJSON(PORTAL_ASSIGNED_ACTIONS_KEY)).toEqual(['voice_push_to_talk']);
		for (const status of [
			portalStatus('bound', {recovering: true}),
			portalStatus('bound', {recovering: true, trigger: null}),
			portalStatus('declined', {recovering: true}),
			portalStatus('unknown', {recovering: true, backend: 'none'}),
			portalStatus('probing', {recovering: true}),
			portalStatus('binding'),
			portalStatus('error', {recovering: true}),
			portalStatus('error', {error: 'portal-unavailable'}),
			portalStatus('error', {error: 'timeout'}),
			portalStatus('error', {error: 'session-closed'}),
			portalStatus('error', {error: 'identity'}),
			portalStatus('error', {error: 'bind-failed'}),
			portalStatus('unsupported', {recovering: true}),
			portalStatus('not-set-up'),
			portalStatus('unknown'),
			portalStatus('bound'),
		]) {
			emit(status);
			expect(Keybind.isPushToTalkEffective()).toBe(true);
		}
		expect(reconcile).not.toHaveBeenCalled();
		expect(AppStorage.getJSON(PORTAL_ASSIGNED_ACTIONS_KEY)).toEqual(['voice_push_to_talk']);
		dispose();
	});

	it('keeps push-to-talk effective while a recovering session reopens with no listed triggers', () => {
		const {Keybind, AppStorage, reactToPushToTalkModeChanges} = modules;
		const reconcile = vi.fn();
		const dispose = reactToPushToTalkModeChanges(reconcile);
		emit(portalStatus('bound'));
		for (const status of [
			portalStatus('bound', {recovering: true}),
			portalStatus('bound', {recovering: true, trigger: null}),
			portalStatus('binding', {recovering: true}),
			portalStatus('bound'),
		]) {
			emit(status);
			expect(Keybind.isPushToTalkEffective()).toBe(true);
			expect(AppStorage.getJSON(PORTAL_ASSIGNED_ACTIONS_KEY)).toEqual(['voice_push_to_talk']);
		}
		expect(reconcile).not.toHaveBeenCalled();
		dispose();
	});

	it('flips push-to-talk only on settled states and preserves self-mute when it does', () => {
		const {Keybind, AppStorage, reactToPushToTalkModeChanges} = modules;
		const reconcile = vi.fn();
		const dispose = reactToPushToTalkModeChanges(reconcile);
		const expectFlip = (status: GlobalShortcutsStatus, effective: boolean): void => {
			reconcile.mockClear();
			emit(status);
			expect(Keybind.isPushToTalkEffective()).toBe(effective);
			expect(reconcile).toHaveBeenCalledTimes(1);
			expect(reconcile).toHaveBeenLastCalledWith({preserveSelfMute: true});
		};
		expectFlip(portalStatus('declined'), false);
		expect(AppStorage.getJSON(PORTAL_ASSIGNED_ACTIONS_KEY)).toEqual([]);
		expectFlip(portalStatus('bound'), true);
		expectFlip(portalStatus('unsupported'), false);
		expectFlip(portalStatus('bound'), true);
		expectFlip(portalStatus('unknown', {backend: 'evdev', directInputEnabled: true}), false);
		expectFlip(portalStatus('bound'), true);
		expectFlip(portalStatus('bound', {trigger: null}), false);
		expectFlip(portalStatus('bound'), true);
		reconcile.mockClear();
		Keybind.setTransmitMode('voice_activity');
		expect(reconcile).toHaveBeenLastCalledWith({preserveSelfMute: false});
		Keybind.setTransmitMode('voice_push_to_talk');
		dispose();
	});
});
