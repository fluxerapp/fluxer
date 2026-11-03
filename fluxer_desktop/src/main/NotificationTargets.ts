// SPDX-License-Identifier: AGPL-3.0-or-later

export interface NotificationTarget {
	readonly accountKey: string | null;
	readonly url: string | null;
}

const MAX_NOTIFICATION_TARGETS = 1000;

export class NotificationTargets {
	private readonly targets = new Map<string, NotificationTarget>();
	private readonly capacity: number;

	constructor(capacity: number = MAX_NOTIFICATION_TARGETS) {
		this.capacity = capacity;
	}

	remember(id: string, target: NotificationTarget): void {
		this.targets.delete(id);
		this.targets.set(id, target);
		while (this.targets.size > this.capacity) {
			const oldest = this.targets.keys().next().value;
			if (oldest === undefined) return;
			this.targets.delete(oldest);
		}
	}

	take(id: string): NotificationTarget | null {
		const target = this.targets.get(id) ?? null;
		this.targets.delete(id);
		return target;
	}

	forget(id: string): void {
		this.targets.delete(id);
	}
}

export function readNotificationAccountKey(value: unknown): string | null {
	return typeof value === 'string' && value.length > 0 && value.length <= 2048 ? value : null;
}

export function readNotificationUrl(value: unknown): string | null {
	return typeof value === 'string' && value.length > 0 && value.length <= 8192 ? value : null;
}
