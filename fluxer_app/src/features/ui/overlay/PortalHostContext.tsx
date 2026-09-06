// SPDX-License-Identifier: AGPL-3.0-or-later

import {createContext, useContext, useSyncExternalStore} from 'react';

export type PortalHostElement = HTMLElement | null;
export type PortalHostContextValue = PortalHostElement | undefined;

let activeHost: PortalHostElement = null;
const listeners = new Set<() => void>();

const subscribe = (callback: () => void): (() => void) => {
	listeners.add(callback);
	return () => {
		listeners.delete(callback);
	};
};

const getSnapshot = (): PortalHostElement => activeHost;
const getServerSnapshot = (): PortalHostElement => null;

export const PortalHostContext = createContext<PortalHostContextValue>(undefined);

export function setActivePortalHost(host: PortalHostElement): void {
	if (activeHost === host) return;
	activeHost = host;
	listeners.forEach((listener) => listener());
}

export function getActivePortalHost(): PortalHostElement {
	return activeHost;
}

export function usePortalHost(): PortalHostElement {
	const globalHost = useSyncExternalStore(subscribe, getSnapshot, getServerSnapshot);
	const contextHost = useContext(PortalHostContext);
	return contextHost === undefined ? globalHost : contextHost;
}

export function resolvePortalHost(host: PortalHostElement): HTMLElement {
	return host ?? document.body;
}

export function scopePortalHostToDocument(host: PortalHostElement, scopeDocument: Document): PortalHostElement {
	if (host == null) return null;
	return host.ownerDocument === scopeDocument ? host : null;
}
