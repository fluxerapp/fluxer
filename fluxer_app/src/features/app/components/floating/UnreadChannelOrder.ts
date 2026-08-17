// SPDX-License-Identifier: AGPL-3.0-or-later

function resolveUnreadOrderIndex(order: Map<string, number>, channelId: string): number {
	const index = order.get(channelId);
	if (index == null) {
		return Number.POSITIVE_INFINITY;
	}
	return index;
}

export function mergeFrozenUnreadOrder<T extends {id: string}>(
	order: Map<string, number>,
	current: ReadonlyArray<T>,
): Array<T> {
	for (const channel of current) {
		if (!order.has(channel.id)) {
			order.set(channel.id, order.size);
		}
	}
	return [...current].sort((a, b) => resolveUnreadOrderIndex(order, a.id) - resolveUnreadOrderIndex(order, b.id));
}
