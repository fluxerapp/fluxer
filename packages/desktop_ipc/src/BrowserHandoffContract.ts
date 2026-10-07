// SPDX-License-Identifier: AGPL-3.0-or-later

export const DESKTOP_HANDOFF_CHANNELS = Object.freeze({
	initiate: 'desktop-handoff:initiate',
	status: 'desktop-handoff:status',
} as const);

export const DesktopHandoffStatus = Object.freeze({
	PENDING: 'pending',
	EXPIRED: 'expired',
	COMPLETED: 'completed',
} as const);

export type DesktopHandoffStatus = (typeof DesktopHandoffStatus)[keyof typeof DesktopHandoffStatus];

export interface DesktopHandoffInstance {
	readonly apiEndpoint: string;
	readonly apiVersion: number;
	readonly webAppEndpoint: string;
}

export interface DesktopHandoffSession {
	readonly instance: DesktopHandoffInstance;
	readonly code: string;
	readonly expiresAt: string;
}

export interface DesktopHandoffUser {
	readonly username: string;
	readonly discriminator: string;
	readonly global_name: string | null;
	readonly avatar: string | null;
	readonly email?: string | null;
}

export interface DesktopHandoffIncompleteResult {
	readonly status: typeof DesktopHandoffStatus.PENDING | typeof DesktopHandoffStatus.EXPIRED;
}

export interface DesktopHandoffCompletedResult {
	readonly status: typeof DesktopHandoffStatus.COMPLETED;
	readonly token: string;
	readonly userId: string;
	readonly user: DesktopHandoffUser | null;
}

export type DesktopHandoffStatusResult = DesktopHandoffIncompleteResult | DesktopHandoffCompletedResult;

export interface DesktopHandoffAPI {
	initiate: (instance: DesktopHandoffInstance) => Promise<DesktopHandoffSession>;
	status: (code: string) => Promise<DesktopHandoffStatusResult>;
}
