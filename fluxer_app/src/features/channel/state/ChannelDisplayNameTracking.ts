// SPDX-License-Identifier: AGPL-3.0-or-later

export interface TrackedRecipientSnapshot {
	readonly recipientIds: ReadonlyArray<string>;
}

export interface TrackedRecipientUser {
	readonly username: string;
	readonly globalName?: string | null;
}

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
