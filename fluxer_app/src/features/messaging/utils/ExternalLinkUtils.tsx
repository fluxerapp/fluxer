// SPDX-License-Identifier: AGPL-3.0-or-later

import {openOAuthAuthorizeModalFromUrl} from '@app/features/auth/commands/OAuthAuthorizeModalCommands';
import {ExternalLinkWarningModal} from '@app/features/messaging/components/modals/ExternalLinkWarningModal';
import {unwrapDesktopLocalResourceURL} from '@app/features/messaging/utils/DesktopLocalResourceTarget';
import TrustedDomain from '@app/features/trusted_domain/state/TrustedDomain';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {getSafeExternalUrl, isDesktop, openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import type React from 'react';

export const MIDDLE_MOUSE_BUTTON = 1;

let externalLinkInterceptorAttached = false;

function isTrustedExternalUrl(url: string): boolean {
	try {
		return TrustedDomain.isTrustedDomain(new URL(url).hostname);
	} catch {
		return false;
	}
}

export function openUrlInBrowserWithWarning(url: string): void {
	const target = unwrapDesktopLocalResourceURL(url);
	if (isTrustedExternalUrl(target)) {
		void openExternalUrl(target);
		return;
	}
	ModalCommands.push(
		modal(() => (
			<ExternalLinkWarningModal
				url={target}
				data-flx="messaging.external-link-utils.open-external-url-with-warning.external-link-warning-modal"
			/>
		)),
	);
}

export function openExternalUrlWithWarning(url: string): void {
	if (openOAuthAuthorizeModalFromUrl(url)) {
		return;
	}
	openUrlInBrowserWithWarning(url);
}

export function handleExternalLinkAuxClick(event: React.MouseEvent, url: string | null | undefined): void {
	if (event.button !== MIDDLE_MOUSE_BUTTON || !url) return;
	event.preventDefault();
	event.stopPropagation();
	openUrlInBrowserWithWarning(url);
}

function findUnhandledExternalAnchorHref(event: MouseEvent): string | null {
	if (event.defaultPrevented) return null;
	const target = event.target as Element | null;
	const anchor = target?.closest?.('a[target="_blank"]');
	const href = anchor?.getAttribute('href') ?? null;
	return getSafeExternalUrl(href) ? href : null;
}

export function attachExternalLinkInterceptor(): () => void {
	if (!isDesktop() || externalLinkInterceptorAttached) return () => undefined;
	const handler = (event: MouseEvent) => {
		const expectedButton = event.type === 'click' ? 0 : MIDDLE_MOUSE_BUTTON;
		if (event.button !== expectedButton) return;
		const href = findUnhandledExternalAnchorHref(event);
		if (href == null) return;
		event.preventDefault();
		openUrlInBrowserWithWarning(href);
	};
	document.addEventListener('click', handler);
	document.addEventListener('auxclick', handler);
	externalLinkInterceptorAttached = true;
	return () => {
		document.removeEventListener('click', handler);
		document.removeEventListener('auxclick', handler);
		externalLinkInterceptorAttached = false;
	};
}
