// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import type {UserData} from '@app/features/auth/state/AccountStorage';
import type {InstanceHTTPTarget} from '@app/features/platform/transport/InstanceHTTP';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import type {DesktopHandoffAPI} from '@fluxer/desktop_ipc/src/BrowserHandoffContract';
import {DesktopHandoffReturnMethod, DesktopHandoffStatus} from '@fluxer/desktop_ipc/src/BrowserHandoffContract';

export interface BrowserLoginHandoffTarget {
	readonly webAppEndpoint: string;
	readonly instance: InstanceHTTPTarget;
}

export interface BrowserLoginHandoffSession {
	readonly code: string;
	readonly expiresAt: string;
	readonly pollSecret: string | null;
	readonly returnMethod: DesktopHandoffReturnMethod;
	readonly target: BrowserLoginHandoffTarget;
}

export interface BrowserLoginHandoffStatusResult {
	readonly status: DesktopHandoffStatus;
	readonly token: string | null;
	readonly userId: string | null;
	readonly userData: UserData | undefined;
}

export const BrowserLoginHandoffTransportKind = Object.freeze({
	IPC: 'ipc',
	RENDERER: 'renderer',
} as const);

export type BrowserLoginHandoffTransportKind =
	(typeof BrowserLoginHandoffTransportKind)[keyof typeof BrowserLoginHandoffTransportKind];

export interface BrowserLoginHandoffTransport {
	readonly kind: BrowserLoginHandoffTransportKind;
	initiate: (target: BrowserLoginHandoffTarget) => Promise<BrowserLoginHandoffSession>;
	status: (session: BrowserLoginHandoffSession) => Promise<BrowserLoginHandoffStatusResult>;
}

function normalizeHandoffStatus(status: string): DesktopHandoffStatus {
	if (status === DesktopHandoffStatus.COMPLETED) {
		return DesktopHandoffStatus.COMPLETED;
	}
	if (status === DesktopHandoffStatus.EXPIRED) {
		return DesktopHandoffStatus.EXPIRED;
	}
	if (status === DesktopHandoffStatus.DENIED) {
		return DesktopHandoffStatus.DENIED;
	}
	return DesktopHandoffStatus.PENDING;
}

function createRendererHandoffTransport(): BrowserLoginHandoffTransport {
	return {
		kind: BrowserLoginHandoffTransportKind.RENDERER,
		initiate: async (target) => {
			const result = await AuthenticationCommands.initiateDesktopHandoff(target.instance);
			return {
				code: result.code,
				expiresAt: result.expires_at,
				pollSecret: result.poll_secret ?? null,
				returnMethod: DesktopHandoffReturnMethod.CODE,
				target,
			};
		},
		status: async (session) => {
			const result = await AuthenticationCommands.pollDesktopHandoffStatus(
				session.code,
				session.pollSecret,
				session.target.instance,
			);
			return {
				status: normalizeHandoffStatus(result.status),
				token: result.token ?? null,
				userId: result.user_id ?? null,
				userData: AuthenticationCommands.authResponseUserToUserData(result.user),
			};
		},
	};
}

function createIpcHandoffTransport(api: DesktopHandoffAPI): BrowserLoginHandoffTransport {
	return {
		kind: BrowserLoginHandoffTransportKind.IPC,
		initiate: async (target) => {
			const session = await api.initiate({
				apiEndpoint: target.instance.apiEndpoint,
				apiVersion: target.instance.apiVersion,
				webAppEndpoint: target.webAppEndpoint,
			});
			return {
				code: session.code,
				expiresAt: session.expiresAt,
				pollSecret: null,
				returnMethod:
					session.returnMethod === DesktopHandoffReturnMethod.DEEP_LINK
						? DesktopHandoffReturnMethod.DEEP_LINK
						: DesktopHandoffReturnMethod.CODE,
				target: {webAppEndpoint: session.instance.webAppEndpoint, instance: target.instance},
			};
		},
		status: async (session) => {
			const result = await api.status(session.code);
			if (result.status !== DesktopHandoffStatus.COMPLETED) {
				return {status: result.status, token: null, userId: null, userData: undefined};
			}
			return {
				status: result.status,
				token: result.token,
				userId: result.userId,
				userData: AuthenticationCommands.authResponseUserToUserData(result.user),
			};
		},
	};
}

export function resolveBrowserLoginHandoffTransport(): BrowserLoginHandoffTransport {
	const api = getElectronAPI()?.desktopHandoff;
	if (api == null || typeof api.initiate !== 'function' || typeof api.status !== 'function') {
		return createRendererHandoffTransport();
	}
	return createIpcHandoffTransport(api);
}
