// SPDX-License-Identifier: AGPL-3.0-or-later

import {getProtectedSessionStorage} from '@app/features/platform/state/ProtectedWebStorage';

const STORAGE_KEY = 'MemberList:reloadScroll';

interface RememberedScroll {
	readonly identityKey: string;
	readonly scrollTop: number;
}

function read(): RememberedScroll | null {
	try {
		const parsed: unknown = JSON.parse(getProtectedSessionStorage()?.getItem(STORAGE_KEY) ?? 'null');
		if (parsed === null || typeof parsed !== 'object') return null;
		const {identityKey, scrollTop} = parsed as Partial<RememberedScroll>;
		return typeof identityKey === 'string' && typeof scrollTop === 'number' ? {identityKey, scrollTop} : null;
	} catch {
		return null;
	}
}

export function rememberMemberListScroll(identityKey: string, scrollTop: number): void {
	try {
		getProtectedSessionStorage()?.setItem(
			STORAGE_KEY,
			JSON.stringify({identityKey, scrollTop: Math.max(0, Math.round(scrollTop))}),
		);
	} catch {}
}

export function recallMemberListScroll(identityKey: string): number {
	const remembered = read();
	return remembered?.identityKey === identityKey ? remembered.scrollTop : 0;
}

export function forgetMemberListScroll(identityKey: string): void {
	if (read()?.identityKey !== identityKey) return;
	try {
		getProtectedSessionStorage()?.removeItem(STORAGE_KEY);
	} catch {}
}
