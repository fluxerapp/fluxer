// SPDX-License-Identifier: AGPL-3.0-or-later

import {useOptOutSelection} from '@app/features/auth/components/pages/oauth_authorize_page/hooks/useOptOutSelection';

import {useCallback} from 'react';

const LOCKED_SCOPES = new Set(['bot']);
const isLockedScope = (scope: string) => LOCKED_SCOPES.has(scope);

export interface ScopeSelection {
	selected: ReadonlySet<string>;
	toggle: (scope: string) => void;
	isLocked: (scope: string) => boolean;
	adjusted: boolean;
	toScopeString: () => string;
}

export function useScopeSelection(requested: ReadonlyArray<string>): ScopeSelection {
	const {selected, toggle, isLocked, adjusted} = useOptOutSelection(requested, isLockedScope);
	const toScopeString = useCallback(() => Array.from(selected).join(' '), [selected]);
	return {selected, toggle, isLocked, adjusted, toScopeString};
}
