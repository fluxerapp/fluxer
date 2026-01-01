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

import 'urlpattern-polyfill';
import '~/styles/preflight.css';
import '~/styles/generated/color-system.css';
import '~/global.css';
import '~/stores/SpellcheckStore';
import '~/components/quick-switcher/QuickSwitcherModal';

import {i18n} from '@lingui/core';
import {I18nProvider} from '@lingui/react';
import * as Sentry from '@sentry/react';
import ReactDOM from 'react-dom/client';

import {App} from '~/App';
import {setupHttpClient} from '~/bootstrap/setupHttpClient';
import {BootstrapErrorScreen} from '~/components/BootstrapErrorScreen';
import {ErrorFallback} from '~/components/ErrorFallback';
import {NetworkErrorScreen} from '~/components/NetworkErrorScreen';
import {initI18n} from '~/i18n';
import CaptchaInterceptor from '~/lib/CaptchaInterceptor';
import AccountManager from '~/stores/AccountManager';
import ChannelDisplayNameStore from '~/stores/ChannelDisplayNameStore';
import GeoIPStore from '~/stores/GeoIPStore';
import KeybindStore from '~/stores/KeybindStore';
import NewDeviceMonitoringStore from '~/stores/NewDeviceMonitoringStore';
import NotificationStore from '~/stores/NotificationStore';
import QuickSwitcherStore from '~/stores/QuickSwitcherStore';
import RuntimeConfigStore from '~/stores/RuntimeConfigStore';
import MediaEngineFacade from '~/stores/voice/MediaEngineFacade';
import {registerServiceWorker} from '~/sw/register';
import {preloadClientInfo} from '~/utils/ClientInfoUtils';
import Config from './Config';

preloadClientInfo();

const normalizePathSegment = (value: string): string => value.replace(/^\/+|\/+$/g, '');

function buildRuntimeSentryDsn(): string | null {
	if (typeof window === 'undefined') {
		return null;
	}

	if (!Config.PUBLIC_SENTRY_PROJECT_ID || !Config.PUBLIC_SENTRY_PUBLIC_KEY) {
		return null;
	}

	const origin = window.location.origin;
	if (!origin) {
		return null;
	}

	const proxyPath = normalizePathSegment(Config.PUBLIC_SENTRY_PROXY_PATH ?? '/error-reporting-proxy');
	const projectSegment = normalizePathSegment(Config.PUBLIC_SENTRY_PROJECT_ID);

	const url = new URL(`/${proxyPath}/${projectSegment}`, origin);
	url.username = Config.PUBLIC_SENTRY_PUBLIC_KEY;
	return url.toString();
}

const resolvedSentryDsn = Config.PUBLIC_SENTRY_DSN ?? buildRuntimeSentryDsn();

if (resolvedSentryDsn) {
	Sentry.init({
		dsn: resolvedSentryDsn,
		environment: Config.PUBLIC_PROJECT_ENV,
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

	try {
		await Promise.all([RuntimeConfigStore.waitForInit(), GeoIPStore.fetchGeoData()]);
	} catch (error) {
		console.error('Failed to initialize runtime config or fetch GeoIP data:', error);
		const root = ReactDOM.createRoot(document.getElementById('root')!);
		root.render(
			<I18nProvider i18n={i18n}>
				<NetworkErrorScreen />
			</I18nProvider>,
		);
		return;
	}

	await AccountManager.bootstrap();

	setupHttpClient();

	const root = ReactDOM.createRoot(document.getElementById('root')!);
	root.render(
		<Sentry.ErrorBoundary
			fallback={
				<I18nProvider i18n={i18n}>
					<ErrorFallback />
				</I18nProvider>
			}
		>
			<App />
		</Sentry.ErrorBoundary>,
	);
	registerServiceWorker();
}

bootstrap().catch(async (error) => {
	console.error('Failed to bootstrap app:', error);

	try {
		await initI18n();
		const root = ReactDOM.createRoot(document.getElementById('root')!);
		root.render(
			<I18nProvider i18n={i18n}>
				<BootstrapErrorScreen error={error} />
			</I18nProvider>,
		);
	} catch (renderError) {
		console.error('Failed to render error screen:', renderError);
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
