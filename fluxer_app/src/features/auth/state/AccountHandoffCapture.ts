// SPDX-License-Identifier: AGPL-3.0-or-later

import {getProtectedSessionStorage} from '@app/features/platform/state/ProtectedWebStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {action, observable} from 'mobx';

const logger = new Logger('AccountHandoffCapture');

const ARRIVAL_PATH = '/channels/@me';
const LEGACY_HANDOFF_PATH = '/channels/@start';
const APP_ENTRY_PATHS: ReadonlySet<string> = new Set(['/', '/app', ARRIVAL_PATH]);

interface CapturedAccountHandoff {
	readonly handoffCode: string;
	readonly nonce: string;
	readonly name: string;
}

const HANDOFF_CODE_PATTERN = /^[A-Za-z0-9_-]{43}$/;
const NONCE_PATTERN = /^[A-Za-z0-9_-]{16,256}$/;
const MAX_NAME_LENGTH = 256;

const HANDOFF_STORAGE_KEY = 'fluxer.account_handoff';

let captured: CapturedAccountHandoff | null = null;
const arrival = observable.box(false);

function storeHandoff(handoff: CapturedAccountHandoff | null): void {
	try {
		const storage = getProtectedSessionStorage();
		if (handoff === null) {
			storage?.removeItem(HANDOFF_STORAGE_KEY);
		} else {
			const params = new URLSearchParams({handoff: handoff.handoffCode, nonce: handoff.nonce, name: handoff.name});
			storage?.setItem(HANDOFF_STORAGE_KEY, `#${params.toString()}`);
		}
	} catch (error) {
		logger.warn('Failed to update stored account handoff', error);
	}
}

function readStoredHandoff(): CapturedAccountHandoff | null {
	try {
		return parseAccountHandoffFragment(getProtectedSessionStorage()?.getItem(HANDOFF_STORAGE_KEY) ?? '');
	} catch (error) {
		logger.warn('Failed to read stored account handoff', error);
		return null;
	}
}

function parseAccountHandoffFragment(hash: string): CapturedAccountHandoff | null {
	const params = new URLSearchParams(hash.startsWith('#') ? hash.slice(1) : hash);
	const handoffCode = params.get('handoff') ?? '';
	const nonce = params.get('nonce') ?? '';
	if (!HANDOFF_CODE_PATTERN.test(handoffCode) || !NONCE_PATTERN.test(nonce)) {
		return null;
	}
	return {handoffCode, nonce, name: (params.get('name') ?? '').trim().slice(0, MAX_NAME_LENGTH)};
}

function resolveAccountHandoffLanding(location: {
	readonly pathname: string;
	readonly search: string;
	readonly hash: string;
}): {readonly handoff: CapturedAccountHandoff | null; readonly url: string} | null {
	const url = `${ARRIVAL_PATH}${location.search}`;
	if (location.pathname === LEGACY_HANDOFF_PATH) {
		return {handoff: parseAccountHandoffFragment(location.hash), url};
	}
	if (location.hash === '' || !APP_ENTRY_PATHS.has(location.pathname)) return null;
	const handoff = parseAccountHandoffFragment(location.hash);
	return handoff === null ? null : {handoff, url};
}

export const captureAccountHandoffFromLocation = action((): void => {
	const landing = resolveAccountHandoffLanding(window.location);
	if (landing !== null) {
		if (landing.handoff !== null) {
			captured = landing.handoff;
			storeHandoff(captured);
		}
		window.history.replaceState(window.history.state, '', landing.url);
	}
	arrival.set(window.location.pathname === ARRIVAL_PATH && readCapturedAccountHandoff() !== null);
});

export function isAccountHandoffArrival(pathname: string): boolean {
	return pathname === ARRIVAL_PATH && arrival.get();
}

export function readCapturedAccountHandoff(): CapturedAccountHandoff | null {
	captured = captured ?? readStoredHandoff();
	return captured;
}

export function clearCapturedAccountHandoff(): void {
	captured = null;
	storeHandoff(null);
}

export const endAccountHandoffArrival = action((): void => {
	clearCapturedAccountHandoff();
	arrival.set(false);
});
