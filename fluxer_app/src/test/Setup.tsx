/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {observer} from 'mobx-react-lite';
import type {ReactNode} from 'react';
import {vi} from 'vitest';
import 'urlpattern-polyfill';

vi.mock('@app/lib/Platform', () => ({
	Platform: {
		OS: 'web',
		isWeb: true,
		isNative: false,
		isIOS: false,
		isAndroid: false,
		isElectron: false,
		select: (specifics: Record<string, any>) =>
			specifics.web ?? specifics.ios ?? specifics.android ?? Object.values(specifics)[0],
		Version: '1.0.0',
	},
	isWebPlatform: () => true,
	isNativePlatform: () => false,
	isElectronPlatform: () => false,
	getNativeLocaleIdentifier: () => null,
}));

vi.mock('mobx-persist-store', () => ({
	makePersistable: vi.fn(() => Promise.resolve()),
	stopPersisting: vi.fn(),
	configurePersistable: vi.fn(),
	isHydrated: vi.fn(() => true),
	isPersisting: vi.fn(() => false),
}));

vi.mock('@app/lib/Logger', () => {
	const noop = () => {};
	class MockLogger {
		child() {
			return new MockLogger();
		}
		trace = noop;
		debug = noop;
		info = noop;
		warn = noop;
		error = noop;
		fatal = noop;
	}
	return {
		Logger: MockLogger,
		LogLevel: {Trace: 0, Debug: 1, Info: 2, Warn: 3, Error: 4, Fatal: 5, Silent: 6},
	};
});

vi.mock('@app/stores/UpdaterStore', () => ({
	default: {
		hasUpdate: false,
		state: 'idle',
		isChecking: false,
		updateType: null,
		updateInfo: {
			native: {available: false, version: null},
			web: {available: false, sha: null, buildNumber: null},
		},
		displayVersion: null,
		currentVersion: null,
		channel: null,
		lastCheckedAt: null,
		checkForUpdates: vi.fn(() => Promise.resolve()),
		dismissUpdate: vi.fn(),
		applyUpdate: vi.fn(),
	},
}));

vi.mock('katex', () => ({
	default: {
		renderToString: vi.fn(() => '<span class="katex">mocked</span>'),
	},
}));

vi.mock('@app/lib/HttpClient', () => {
	const defaultInstance = {
		api_code_version: 1,
		endpoints: {
			api: 'https://localhost/api',
			api_client: 'https://localhost/api',
			api_public: 'https://localhost/api',
			gateway: 'wss://localhost/gateway',
			media: 'https://localhost/media',
			cdn: 'https://localhost/cdn',
			marketing: 'https://localhost/marketing',
			admin: 'https://localhost/admin',
			invite: 'https://localhost/invite',
			gift: 'https://localhost/gift',
			webapp: 'https://localhost',
		},
		captcha: {provider: 'none', hcaptcha_site_key: null, turnstile_site_key: null},
		features: {sms_mfa_enabled: false, voice_enabled: false, stripe_enabled: false, self_hosted: false},
		limits: {version: 1, traitDefinitions: [], rules: []},
		app_public: {
			sentry_dsn: '',
		},
	};

	const mockResponse = Promise.resolve({ok: true, status: 200, headers: {}, body: defaultInstance});
	const client = {
		get: vi.fn(() => mockResponse),
		post: vi.fn(() => mockResponse),
		put: vi.fn(() => mockResponse),
		patch: vi.fn(() => mockResponse),
		delete: vi.fn(() => mockResponse),
		request: vi.fn(() => mockResponse),
		setBaseUrl: vi.fn(),
		setSudoHandler: vi.fn(),
		setSudoFailureHandler: vi.fn(),
		setSudoTokenProvider: vi.fn(),
		setSudoTokenListener: vi.fn(),
		setSudoTokenInvalidator: vi.fn(),
		setRelayDirectoryUrl: vi.fn(),
		setTargetInstanceDomain: vi.fn(),
	};

	return {__esModule: true, default: client};
});

vi.mock('@app/components/channel/emoji_picker/EmojiPickerConstants', () => ({
	EMOJI_CLAP: '👏',
	EMOJI_SPRITE_SIZE: 32,
	EMOJI_ROW_HEIGHT: 48,
	CATEGORY_HEADER_HEIGHT: 32,
	EMOJIS_PER_ROW: 9,
	OVERSCAN_ROWS: 5,
	getSpriteSheetPath: () => 'sprite.png',
	getSpriteSheetBackground: () => 'url(sprite.png)',
}));

vi.mock('@app/components/modals/ImageCropModal', () => ({
	default: () => null,
}));

vi.mock('@lingui/core/macro', () => {
	interface LinguiMessage {
		message?: string;
		defaultMessage?: string;
	}

	const formatMessage = (str: TemplateStringsArray | string | LinguiMessage | undefined, ...expr: Array<unknown>) => {
		if (typeof str === 'string') return str;
		if (Array.isArray(str)) {
			return String(str.reduce((acc, chunk, i) => acc + chunk + (expr[i] ?? ''), ''));
		}
		if (str && typeof str === 'object' && !Array.isArray(str)) {
			const message = (str as LinguiMessage).message;
			const defaultMessage = (str as LinguiMessage).defaultMessage;
			return message ?? defaultMessage ?? '';
		}
		return '';
	};

	return {
		t: formatMessage,
		msg: formatMessage,
	};
});

vi.mock('@lingui/react/macro', () => {
	const React = require('react');

	interface TransProps {
		children?: ReactNode;
		id?: string;
		message?: ReactNode;
	}

	const Trans = observer(({children, id, message}: TransProps) => {
		if (children != null) return React.createElement(React.Fragment, null, children);
		return React.createElement('span', null, message ?? id ?? null);
	});

	interface PluralProps {
		value: number;
		one?: ReactNode;
		other?: ReactNode;
		zero?: ReactNode;
		few?: ReactNode;
		many?: ReactNode;
	}

	const Plural = observer(({value, one, other, zero, few, many}: PluralProps) => {
		if (value === 0 && zero != null) return zero;
		if (value === 1 && one != null) return one;
		if (typeof few !== 'undefined' && value >= 2 && value <= 4) return few;
		if (typeof many !== 'undefined' && value >= 5) return many;
		return other ?? null;
	});

	interface SelectProps extends Record<string, ReactNode | string | number | undefined> {
		value: string | number;
		other?: ReactNode;
	}

	const Select = observer(({value, other, ...cases}: SelectProps) => {
		const key = String(value);
		return Object.hasOwn(cases, key) ? (cases[key] as ReactNode) : (other ?? null);
	});

	const SelectOrdinal = Plural;

	return {
		Trans,
		Plural,
		Select,
		SelectOrdinal,
	};
});

vi.mock('@app/utils/NotificationUtils', () => ({
	ensureDesktopNotificationClickHandler: vi.fn(),
	hasNotification: () => false,
	isGranted: async () => false,
	playNotificationSoundIfEnabled: vi.fn(),
	requestPermission: async () => {},
	showNotification: async () => ({browserNotification: null, nativeNotificationId: null}),
	closeNativeNotification: () => undefined,
	closeNativeNotifications: () => undefined,
}));
