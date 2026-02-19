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

import {configure} from 'mobx';

// TODO: Not sure why this is needed, but without it, we might get stuck in a loop of errors.
configure({disableErrorBoundaries: true});

import '@app/components/quick_switcher/QuickSwitcherModal';
import '@app/global.css';
import '@app/stores/SpellcheckStore';
import '@app/styles/generated/color-system.css';
import '@app/styles/preflight.css';
import 'urlpattern-polyfill';
import {App} from '@app/App';
import * as AuthenticationActionCreators from '@app/actions/AuthenticationActionCreators';
import {setupHttpClient} from '@app/bootstrap/SetupHttpClient';
import Config from '@app/Config';
import {BootstrapErrorScreen} from '@app/components/BootstrapErrorScreen';
import {ErrorFallback} from '@app/components/ErrorFallback';
import {NetworkErrorScreen} from '@app/components/NetworkErrorScreen';
import {initI18n} from '@app/I18n';
import CaptchaInterceptor from '@app/lib/CaptchaInterceptor';
import {Logger} from '@app/lib/Logger';
import {initializeEmojiParser} from '@app/lib/markdown/EmojiProviderSetup';
import {registerServiceWorker} from '@app/service_worker/Register';
import AccountManager from '@app/stores/AccountManager';
import ChannelDisplayNameStore from '@app/stores/ChannelDisplayNameStore';
import GeoIPStore from '@app/stores/GeoIPStore';
import KeybindStore from '@app/stores/KeybindStore';
import NewDeviceMonitoringStore from '@app/stores/NewDeviceMonitoringStore';
import NotificationStore from '@app/stores/NotificationStore';
import QuickSwitcherStore from '@app/stores/QuickSwitcherStore';
import RuntimeConfigStore from '@app/stores/RuntimeConfigStore';
import MediaEngineFacade from '@app/stores/voice/MediaEngineFacade';
import {preloadClientInfo} from '@app/utils/ClientInfoUtils';
import {getElectronAPI} from '@app/utils/NativeUtils';
import TtsUtils from '@app/utils/TtsUtils';
import {i18n} from '@lingui/core';
import {I18nProvider} from '@lingui/react';
import type {Scope} from '@sentry/react';
import * as Sentry from '@sentry/react';
import ReactDOM from 'react-dom/client';

interface SentryErrorBoundaryProps {
	fallback?: React.ReactNode;
	beforeCapture?: (scope: Scope, context: unknown) => void;
	children?: React.ReactNode;
}

const SentryErrorBoundary = Sentry.ErrorBoundary as React.ComponentType<SentryErrorBoundaryProps>;

const logger = new Logger('index');

preloadClientInfo();

async function resumePendingDesktopHandoffLogin(): Promise<void> {
	const electronApi = getElectronAPI();
	if (!electronApi || typeof electronApi.consumeDesktopHandoffCode !== 'function') {
		return;
	}

	let handoffCode: string | null = null;
	try {
		handoffCode = await electronApi.consumeDesktopHandoffCode();
	} catch (error) {
		logger.warn('Failed to consume pending desktop handoff code:', error);
		return;
	}

	if (!handoffCode) {
		return;
	}

	try {
		const result = await AuthenticationActionCreators.pollDesktopHandoffStatus(handoffCode);
		if (result.status === 'completed' && result.token && result.user_id) {
			await AuthenticationActionCreators.completeLogin({token: result.token, userId: result.user_id});
		} else {
			logger.warn('Pending desktop handoff not completed:', {status: result.status});
		}
	} catch (error) {
		logger.warn('Failed to resume pending desktop handoff login:', error);
	}
}

function initSentry(): void {
	const resolvedSentryDsn = RuntimeConfigStore.sentryDsn;
	const normalizedBuildSha =
		Config.PUBLIC_BUILD_SHA && Config.PUBLIC_BUILD_SHA !== 'dev' ? Config.PUBLIC_BUILD_SHA : undefined;
	const buildNumberString =
		Config.PUBLIC_BUILD_NUMBER && Config.PUBLIC_BUILD_NUMBER > 0 ? String(Config.PUBLIC_BUILD_NUMBER) : undefined;
	const buildTimestampString = Config.PUBLIC_BUILD_TIMESTAMP ? String(Config.PUBLIC_BUILD_TIMESTAMP) : undefined;
	const releaseLabel = normalizedBuildSha ? `fluxer-app@${normalizedBuildSha}` : undefined;

	if (!resolvedSentryDsn) {
		return;
	}

	const buildContextEntries: Array<[string, string]> = [];
	if (normalizedBuildSha) {
		buildContextEntries.push(['sha', normalizedBuildSha]);
	}
	if (buildNumberString) {
		buildContextEntries.push(['number', buildNumberString]);
	}
	if (buildTimestampString) {
		buildContextEntries.push(['timestamp', buildTimestampString]);
	}
	if (Config.PUBLIC_RELEASE_CHANNEL) {
		buildContextEntries.push(['channel', Config.PUBLIC_RELEASE_CHANNEL]);
	}

	Sentry.init({
		dsn: resolvedSentryDsn,
		environment: Config.PUBLIC_RELEASE_CHANNEL,
		release: releaseLabel,
		dist: buildNumberString,
		sendDefaultPii: true,
		beforeSend(event, hint) {
			const error = hint.originalException;
			if (error instanceof Error) {
				if (error.name === 'HTTPResponseError' || error.name === 'TimeoutError') {
					return null;
				}
			}
			return event;
		},
		initialScope: (scope: Scope) => {
			if (Config.PUBLIC_RELEASE_CHANNEL) {
				scope.setTag('release_channel', Config.PUBLIC_RELEASE_CHANNEL);
			}
			if (normalizedBuildSha) {
				scope.setTag('build_sha', normalizedBuildSha);
			}
			if (buildNumberString) {
				scope.setTag('build_number', buildNumberString);
			}
			if (buildTimestampString) {
				scope.setTag('build_timestamp', buildTimestampString);
			}

			if (buildContextEntries.length > 0) {
				scope.setContext('build', Object.fromEntries(buildContextEntries));
			}

			return scope;
		},
	});
}

async function bootstrap(): Promise<void> {
	await initI18n();

	QuickSwitcherStore.setI18n(i18n);
	ChannelDisplayNameStore.setI18n(i18n);
	KeybindStore.setI18n(i18n);
	NewDeviceMonitoringStore.setI18n(i18n);
	NotificationStore.setI18n(i18n);
	MediaEngineFacade.setI18n(i18n);
	CaptchaInterceptor.setI18n(i18n);
	TtsUtils.setI18n(i18n);
	TtsUtils.init();

	try {
		await RuntimeConfigStore.waitForInit();
		initSentry();
	} catch (error) {
		logger.error('Failed to initialize runtime config:', error);
		const root = ReactDOM.createRoot(document.getElementById('root')!);
		root.render(
			<I18nProvider i18n={i18n}>
				<NetworkErrorScreen />
			</I18nProvider>,
		);
		return;
	}

	if (!RuntimeConfigStore.isSelfHosted()) {
		try {
			await GeoIPStore.fetchGeoData();
		} catch (error) {
			logger.warn('Failed to fetch GeoIP data (continuing anyway):', error);
		}
	}

	await AccountManager.bootstrap();

	setupHttpClient();
	initializeEmojiParser();

	await resumePendingDesktopHandoffLogin();

	const root = ReactDOM.createRoot(document.getElementById('root')!);
	root.render(
		<SentryErrorBoundary
			fallback={
				<I18nProvider i18n={i18n}>
					<ErrorFallback />
				</I18nProvider>
			}
			beforeCapture={(scope: Scope, _context: unknown) => {
				scope.setTag('sentry', 'true');
			}}
		>
			<App />
		</SentryErrorBoundary>,
	);
	registerServiceWorker();
}

bootstrap().catch(async (error) => {
	logger.error('Failed to bootstrap app:', error);

	try {
		await initI18n();
		const root = ReactDOM.createRoot(document.getElementById('root')!);
		root.render(
			<I18nProvider i18n={i18n}>
				<BootstrapErrorScreen error={error} />
			</I18nProvider>,
		);
	} catch (renderError) {
		logger.error('Failed to render error screen:', renderError);
		document.body.style.margin = '0';
		document.body.style.minHeight = '100vh';
		document.body.innerHTML = `
			<div
				style="
					min-height: 100vh;
					display: flex;
					align-items: center;
					justify-content: center;
					padding: 2rem;
					text-align: center;
					box-sizing: border-box;
				"
			>
				<p
					style="
						max-width: 32rem;
						font-size: 1.25rem;
						line-height: 1.5;
						margin: 0;
					"
				>
					Something went wrong and the app couldn't load. Please try refreshing the page.
				</p>
			</div>
		`;
	}
});
