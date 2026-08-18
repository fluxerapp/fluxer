// SPDX-License-Identifier: AGPL-3.0-or-later

export interface TrackedRecipientSnapshot {
	readonly recipientIds: ReadonlyArray<string>;
}

export interface TrackedRecipientUser {
	readonly username: string;
	readonly globalName?: string | null;
}

/**
 * Builds the MobX tracking key for group-DM display names.
 *
 * Only the recipients of tracked group DMs are read, so the observing reaction
 * depends on those users alone rather than on the whole (unbounded) user cache.
 * Reading a recipient that is not cached yet still registers a dependency on
 * that key, so a user arriving later re-triggers the reaction.
 *
 * Kept as a standalone function rather than a class method because
 * makeAutoObservable converts methods into actions, and actions run untracked.
 */
export const trackedRecipientNameKey = (
	snapshots: ReadonlyMap<string, TrackedRecipientSnapshot>,
	users: Record<string, TrackedRecipientUser | undefined>,
): string => {
	const parts: Array<[string, string, string]> = [];
	for (const snapshot of snapshots.values()) {
		for (const recipientId of snapshot.recipientIds) {
			const user = users[recipientId];
			parts.push([recipientId, user?.username ?? '', user?.globalName ?? '']);
		}
	}
	return JSON.stringify(parts);
};
