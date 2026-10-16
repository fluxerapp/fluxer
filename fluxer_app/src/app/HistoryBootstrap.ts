// SPDX-License-Identifier: AGPL-3.0-or-later

const SYNTHETIC_STATE_FLAG = '__fluxer_synth';
const MAX_SYNTHETIC_DEPTH = 6;

interface SyntheticHistoryState {
	[SYNTHETIC_STATE_FLAG]: true;
	depth: number;
}

function isSyntheticHistoryState(state: unknown): state is SyntheticHistoryState {
	return (
		typeof state === 'object' && state !== null && (state as Record<string, unknown>)[SYNTHETIC_STATE_FLAG] === true
	);
}

export function derivePathStack(path: string): Array<string> {
	const segments = path
		.split('?')[0]
		.split('#')[0]
		.split('/')
		.filter((s) => s.length > 0);
	if (segments[0] !== 'channels' || segments.length < 2) return [];
	const root = segments[1];
	if (root === '@me' || root === '@favorites' || root === '@discover') {
		const stack: Array<string> = [];
		if (root !== '@me') stack.push('/channels/@me');
		stack.push(`/channels/${root}`);
		if (segments.length >= 3) stack.push(`/channels/${root}/${segments[2]}`);
		return stack.slice(0, -1);
	}
	const stack: Array<string> = [`/channels/${root}`];
	if (segments.length >= 3) stack.push(`/channels/${root}/${segments[2]}`);
	return stack.slice(0, -1);
}

export function bootstrapSyntheticHistory(): void {
	if (typeof window === 'undefined' || !window.history) return;
	if (isSyntheticHistoryState(window.history.state)) return;
	if (window.history.length > 1) return;
	const path = window.location.pathname + window.location.search + window.location.hash;
	const parents = derivePathStack(window.location.pathname).slice(-MAX_SYNTHETIC_DEPTH);
	if (parents.length === 0) return;
	const deepestParent = parents[0];
	window.history.replaceState(
		{[SYNTHETIC_STATE_FLAG]: true, depth: 0} satisfies SyntheticHistoryState,
		'',
		deepestParent,
	);
	for (let i = 1; i < parents.length; i++) {
		window.history.pushState({[SYNTHETIC_STATE_FLAG]: true, depth: i} satisfies SyntheticHistoryState, '', parents[i]);
	}
	window.history.pushState(null, '', path);
}
