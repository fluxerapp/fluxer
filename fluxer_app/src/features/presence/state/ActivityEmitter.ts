// SPDX-License-Identifier: AGPL-3.0-or-later

type ActivityListener = () => void;

class ActivityEmitterClass {
	private listeners = new Map<string, Set<ActivityListener>>();

	subscribeToPresence(userId: string, listener: ActivityListener): () => void {
		let set = this.listeners.get(userId);
		if (!set) {
			set = new Set();
			this.listeners.set(userId, set);
		}
		set.add(listener);
		return () => {
			set?.delete(listener);
			if (set?.size === 0) this.listeners.delete(userId);
		};
	}

	emitPresenceChange(userId: string): void {
		const set = this.listeners.get(userId);
		if (!set) return;
		for (const listener of set) listener();
	}
}

export const ActivityEmitter = new ActivityEmitterClass();
