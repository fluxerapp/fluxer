// SPDX-License-Identifier: AGPL-3.0-or-later

import Navigation from '@app/features/navigation/state/Navigation';

class ActiveComposer {
	private readonly mounted = new Map<string, number>();
	private focusedChannelId: string | null = null;

	register(channelId: string): () => void {
		this.mounted.set(channelId, (this.mounted.get(channelId) ?? 0) + 1);
		return () => {
			const count = this.mounted.get(channelId) ?? 0;
			if (count <= 1) {
				this.mounted.delete(channelId);
				if (this.focusedChannelId === channelId) this.focusedChannelId = null;
			} else {
				this.mounted.set(channelId, count - 1);
			}
		};
	}

	focus(channelId: string): void {
		this.focusedChannelId = channelId;
	}

	accepts(channelId: string): boolean {
		if (this.mounted.size <= 1) return true;
		return this.target() === channelId;
	}

	resolve(channelId: string): string {
		if (this.mounted.size <= 1) return channelId;
		return this.target() ?? channelId;
	}

	private target(): string | null {
		if (this.focusedChannelId != null && this.mounted.has(this.focusedChannelId)) return this.focusedChannelId;
		if (Navigation.channelId != null && this.mounted.has(Navigation.channelId)) return Navigation.channelId;
		if (Navigation.threadId != null && this.mounted.has(Navigation.threadId)) return Navigation.threadId;
		return Array.from(this.mounted.keys()).at(-1) ?? null;
	}
}

export default new ActiveComposer();
