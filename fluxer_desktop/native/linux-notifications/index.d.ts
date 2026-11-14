// SPDX-License-Identifier: AGPL-3.0-or-later

export type Urgency = 'low' | 'normal' | 'critical';

export interface NotifyImageData {
	width: number;
	height: number;
	rowstride: number;
	hasAlpha: boolean;
	bitsPerSample: number;
	channels: number;
	data: Buffer | Uint8Array;
}

export interface NotifyAction {
	key: string;
	label: string;
}

export interface NotifyPayload {
	appName: string;
	replacesId?: number;
	appIcon?: string;
	summary: string;
	body: string;
	actions?: ReadonlyArray<NotifyAction>;
	expireTimeoutMs?: number;
	hints?: {
		urgency?: Urgency;
		category?: string;
		desktopEntry?: string;
		soundFile?: string;
		transient?: boolean;
		actionIcons?: boolean;
		imageData?: NotifyImageData;
	};
}

export type FreedesktopNotificationEvent =
	| {kind: 'actionInvoked'; id: number; actionKey: string}
	| {kind: 'closed'; id: number; reason: number};

export declare class FreedesktopNotifications {
	constructor(onEvent: (event: FreedesktopNotificationEvent) => void);

	notify(payload: NotifyPayload): Promise<number>;

	closeNotification(id: number): Promise<void>;

	getServerCapabilities(): Promise<Array<string>>;

	close(): Promise<void>;
}

export declare const loadError: Error | null;
